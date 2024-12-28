#include "tiger/output/output.h"

#include <cstdio>
#include <fstream>
#include <sstream>

#include "tiger/output/logger.h"

#include <iostream>

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/raw_ostream.h"
#include <map>

extern frame::RegManager *reg_manager;
extern frame::Frags *frags;
extern llvm::IRBuilder<> *ir_builder;
extern llvm::Module *ir_module;
std::map<std::string, std::pair<int, int>> frame_info_map;

namespace output {
void AssemGen::LoadllvmAndGen(bool need_ra) {
  llvm::LLVMContext context;
  llvm::SMDiagnostic error;

  std::ifstream infile(filename + ".ll");
  std::string line;
  bool in_frame_info = false;
  while (std::getline(infile, line)) {
    if (line == "; ===== Frame Information =====") {
      in_frame_info = true;
      continue;
    }
    if (in_frame_info && line[0] == ';') {
      std::stringstream ss(line.substr(2));
      std::string func_name;
      int offset, framesize;
      ss >> func_name >> offset >> framesize;
      frame_info_map[func_name] = std::make_pair(offset, framesize);
    }
  }
  infile.close();
  std::unique_ptr<llvm::Module> mod =
      llvm::parseIRFile(filename + ".ll", error, context);

  if (!mod) {
    error.print("output", llvm::errs());
    return;
  }

  for (auto &func : mod->getFunctionList()) {
    if (func.isDeclaration())
      continue;
    frags->PushBack(new frame::ProcFrag(&func));
  }

  for (auto &global : mod->getGlobalList()) {
    if (global.hasInitializer()) {
      auto *init = global.getInitializer();
      if (auto *str_struct = llvm::dyn_cast<llvm::ConstantStruct>(init)) {
        if (str_struct->getType()->getName() == "string") {
          llvm::Constant *strArrayPtr =
              llvm::cast<llvm::Constant>(str_struct->getOperand(1));
          llvm::GlobalVariable *strArray =
              llvm::cast<llvm::GlobalVariable>(strArrayPtr->getOperand(0));

          if (auto str_init = llvm::dyn_cast<llvm::ConstantDataArray>(
                                  strArray->getInitializer()) == nullptr) {
            frags->PushBack(new frame::StringFrag(&global, ""));
            continue;
          }
          std::string strContent =
              static_cast<llvm::ConstantDataArray *>(strArray->getInitializer())
                  ->getAsCString()
                  .str();
          frags->PushBack(new frame::StringFrag(&global, strContent));
        }
      } else if (auto *num = llvm::dyn_cast<llvm::ConstantInt>(init)) {
        int framesize = num->getSExtValue();
        frags->PushBack(new frame::FrameSizeFrag(&global, framesize));
      }
    }
  }

  GenAssem(need_ra);
}

void AssemGen::GenAssem(bool need_ra) {
  frame::Frag::OutputPhase phase;

  // Output proc
  phase = frame::Frag::Proc;
  fprintf(out_, ".text\n");
  for (auto &&frag : frags->GetList())
    frag->OutputAssem(out_, phase, need_ra);

  fprintf(out_, ".section .rodata\n");
  // Output string
  phase = frame::Frag::String;
  for (auto &&frag : frags->GetList())
    frag->OutputAssem(out_, phase, need_ra);

  // Output framesize
  phase = frame::Frag::FrameSize;
  for (auto &&frag : frags->GetList())
    frag->OutputAssem(out_, phase, need_ra);
}

} // namespace output

namespace frame {

void ProcFrag::OutputAssem(FILE *out, OutputPhase phase, bool need_ra) const {
  std::unique_ptr<canon::Traces> traces;
  std::unique_ptr<cg::AssemInstr> assem_instr;
  std::unique_ptr<ra::Result> allocation;

  // When generating proc fragment, do not output string assembly
  if (phase != Proc)
    return;

  {
    // Canonicalize
    TigerLog("-------====Canonicalize=====-----\n");
    canon::Canon canon(body_);

    // Group list into basic blocks
    // make all basic block name unique
    TigerLog("------====Basic block_=====-------\n");
    canon::BasicBlockList *basicblock_list = canon.BasicBlocks();

    // Order basic blocks into traces_
    TigerLog("-------====Trace=====-----\n");
    traces = canon.TransferTraces();
  }

  temp::Map *color =
      temp::Map::LayerMap(reg_manager->temp_map_, temp::Map::Name());
  {
    // Lab 5: code generation
    TigerLog("-------====Code generate=====-----\n");
    cg::CodeGen code_gen(std::move(traces));
    code_gen.Codegen();
    assem_instr = code_gen.TransferAssemInstr();
    TigerLog(assem_instr.get(), color);
  }

  assem::InstrList *il = assem_instr.get()->GetInstrList();

  if (need_ra) {
    // Lab 6: register allocation
    TigerLog("----====Register allocate====-----\n");
    ra::RegAllocator reg_allocator(body_->getName().str(),
                                   std::move(assem_instr));
    reg_allocator.reg_main();
    allocation = reg_allocator.TransferResult();
    il = allocation->il_;
    color = temp::Map::LayerMap(reg_manager->temp_map_, allocation->coloring_);
  }

  std::string proc_name = body_->getName().str();

  TigerLog("-------====Output assembly for %s=====-----\n", proc_name.data());

  assem::Proc *proc = frame::ProcEntryExit3(proc_name, il);

  frame_info_map[proc_name].second =
      frame_info_map[proc_name].second / 16 * 16 + 8;

  fprintf(out, ".globl %s\n", proc_name.data());
  fprintf(out, ".type %s, @function\n", proc_name.data());
  fprintf(out, ".set %s_framesize_local, %d\n", proc_name.data(),
          frame_info_map[proc_name].second);
  // prologue
  fprintf(out, "%s", proc->prolog_.data());
  // body
  proc->body_->Print(out, color);
  // epilog_
  fprintf(out, "%s", proc->epilog_.data());
  fprintf(out, ".size %s, .-%s\n", proc_name.data(), proc_name.data());
}

void StringFrag::OutputAssem(FILE *out, OutputPhase phase, bool need_ra) const {
  // When generating string fragment, do not output proc assembly
  if (phase != String)
    return;

  // str_val_->setName(temp::LabelFactory::NewLabel()->Name());
  fprintf(out, "%s:\n", str_val_->getName().data());
  int length = static_cast<int>(str_.size());
  // It may contain zeros in the middle of string. To keep this work, we need
  // to print all the charactors instead of using fprintf(str)
  fprintf(out, ".long %d\n", length);
  fprintf(out, ".string \"");
  for (int i = 0; i < length; i++) {
    if (str_[i] == '\n') {
      fprintf(out, "\\n");
    } else if (str_[i] == '\t') {
      fprintf(out, "\\t");
    } else if (str_[i] == '\"') {
      fprintf(out, "\\\"");
    } else {
      fprintf(out, "%c", str_[i]);
    }
  }
  fprintf(out, "\"\n");
}

void FrameSizeFrag::OutputAssem(FILE *out, OutputPhase phase,
                                bool need_ra) const {
  // When generating string fragment, do not output proc assembly
  if (phase != FrameSize)
    return;

  std::string fsg_name = framesize_val_->getName().data();
  std::string func_name =
      fsg_name.substr(0, fsg_name.find("_framesize_global"));
  fprintf(out, "%s:\n", framesize_val_->getName().data());

  fprintf(out, ".quad %d\n", frame_info_map[func_name].second);
}
} // namespace frame
