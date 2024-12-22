#include "tiger/codegen/codegen.h"
#include "tiger/codegen/assem.h"

#include <cassert>
#include <iostream>
#include <llvm-14/llvm/IR/GlobalVariable.h>
#include <llvm-14/llvm/IR/Instruction.h>
#include <llvm-14/llvm/IR/Instructions.h>
#include <llvm-14/llvm/Support/Casting.h>
#include <sstream>

extern frame::RegManager *reg_manager;
extern frame::Frags *frags;

namespace {

constexpr int maxlen = 1024;

} // namespace

namespace cg {

void CodeGen::Codegen() {
  temp_map_ = new std::unordered_map<llvm::Value *, temp::Temp *>();
  bb_map_ = new std::unordered_map<llvm::BasicBlock *, int>();
  auto *list = new assem::InstrList();

  // firstly get all global string's location
  for (auto &&frag : frags->GetList()) {
    if (auto *str_frag = dynamic_cast<frame::StringFrag *>(frag)) {
      auto tmp = temp::TempFactory::NewTemp();
      list->Append(new assem::OperInstr(
          "leaq " + std::string(str_frag->str_val_->getName()) + "(%rip),`d0",
          new temp::TempList(tmp), new temp::TempList(), nullptr));
      temp_map_->insert({str_frag->str_val_, tmp});
    }
  }

  // move arguments to temp
  auto arg_iter = traces_->GetBody()->arg_begin();
  auto regs = reg_manager->ArgRegs();
  auto tmp_iter = regs->GetList().begin();

  // first arguement is rsp, we need to skip it
  ++arg_iter;

  for (; arg_iter != traces_->GetBody()->arg_end() &&
         tmp_iter != regs->GetList().end();
       ++arg_iter, ++tmp_iter) {
    auto tmp = temp::TempFactory::NewTemp();
    list->Append(new assem::OperInstr("movq `s0,`d0", new temp::TempList(tmp),
                                      new temp::TempList(*tmp_iter), nullptr));
    temp_map_->insert({static_cast<llvm::Value *>(arg_iter), tmp});
  }

  // pass-by-stack parameters
  if (arg_iter != traces_->GetBody()->arg_end()) {
    auto last_sp = temp::TempFactory::NewTemp();
    list->Append(
        new assem::OperInstr("movq %rsp,`d0", new temp::TempList(last_sp),
                             new temp::TempList(reg_manager->GetRegister(
                                 frame::X64RegManager::Reg::RSP)),
                             nullptr));
    list->Append(new assem::OperInstr(
        "addq $" + std::string(traces_->GetFunctionName()) +
            "_framesize_local,`s0",
        new temp::TempList(last_sp),
        new temp::TempList({last_sp, reg_manager->GetRegister(
                                         frame::X64RegManager::Reg::RSP)}),
        nullptr));
    while (arg_iter != traces_->GetBody()->arg_end()) {
      auto tmp = temp::TempFactory::NewTemp();
      list->Append(new assem::OperInstr(
          "movq " +
              std::to_string(8 * (arg_iter - traces_->GetBody()->arg_begin())) +
              "(`s0),`d0",
          new temp::TempList(tmp), new temp::TempList(last_sp), nullptr));
      temp_map_->insert({static_cast<llvm::Value *>(arg_iter), tmp});
      ++arg_iter;
    }
  }

  // construct bb_map
  int bb_index = 0;
  for (auto &&bb : traces_->GetBasicBlockList()->GetList()) {
    bb_map_->insert({bb, bb_index++});
  }

  for (auto &&bb : traces_->GetBasicBlockList()->GetList()) {
    // record every return value from llvm instruction
    for (auto &&inst : bb->getInstList())
      temp_map_->insert({&inst, temp::TempFactory::NewTemp()});
  }

  for (auto &&bb : traces_->GetBasicBlockList()->GetList()) {
    // Generate label for basic block
    list->Append(new assem::LabelInstr(std::string(bb->getName())));

    // Generate instructions for basic block
    for (auto &&inst : bb->getInstList())
      InstrSel(list, inst, traces_->GetFunctionName(), bb);
  }

  assem_instr_ = std::make_unique<AssemInstr>(frame::ProcEntryExit2(
      frame::ProcEntryExit1(traces_->GetFunctionName(), list)));
}

void AssemInstr::Print(FILE *out, temp::Map *map) const {
  for (auto instr : instr_list_->GetList())
    instr->Print(out, map);
  fprintf(out, "\n");
}

void CodeGen::InstrSel(assem::InstrList *instr_list, llvm::Instruction &inst,
                       std::string_view function_name, llvm::BasicBlock *bb) {
  // TODO: your lab5 code here
  auto op_code = inst.getOpcode();
  switch (inst.getOpcode()) {
    /**
     *  %rsp only changed by addq or subq, only when enter or exit function.
     *  First two lines in ir changed %rsp, but ProcEntryExit3 actually change
     * %rsp. So we should ignore distination is %rsp in ir.
     */
  case llvm::Instruction::Add:
  case llvm::Instruction::Sub:
  case llvm::Instruction::Mul: {
    std::string oper;
    if (inst.getOpcode() == llvm::Instruction::Add)
      oper = "addq";
    else if (inst.getOpcode() == llvm::Instruction::Sub)
      oper = "subq";
    else if (inst.getOpcode() == llvm::Instruction::Mul)
      oper = "imulq";

    llvm::Value *val1 = inst.getOperand(0);
    llvm::Value *val2 = inst.getOperand(1);
    if (llvm::dyn_cast<llvm::ConstantInt>(val1))
      std::swap(val1, val2);

    temp::Temp *dist_temp;
    if (IsRsp(&inst, inst.getFunction()->getName())) {
      dist_temp = reg_manager->GetRegister(frame::X64RegManager::Reg::RSP);
      break;
    } else {
      dist_temp = temp_map_->find(&inst)->second;
      temp::Temp *val1_temp =
          IsRsp(val1, inst.getFunction()->getName())
              ? reg_manager->GetRegister(frame::X64RegManager::Reg::RSP)
              : temp_map_->find(val1)->second;
      instr_list->Append(new assem::MoveInstr("movq `s0,`d0",
                                              new temp::TempList(dist_temp),
                                              new temp::TempList(val1_temp)));
    }

    if (llvm::dyn_cast<llvm::Instruction>(val2)) {
      temp::Temp *val2_temp =
          IsRsp(val2, inst.getFunction()->getName())
              ? reg_manager->GetRegister(frame::X64RegManager::Reg::RSP)
              : temp_map_->find(val2)->second;
      instr_list->Append(new assem::OperInstr(
          oper + " `s0,`d0", new temp::TempList(dist_temp),
          new temp::TempList({val2_temp, dist_temp}), nullptr));
    } else if (llvm::ConstantInt *constInt =
                   llvm::dyn_cast<llvm::ConstantInt>(val2)) {
      instr_list->Append(new assem::OperInstr(
          oper + " $" + std::to_string(constInt->getSExtValue()) + ",`d0",
          new temp::TempList(dist_temp), new temp::TempList(dist_temp),
          nullptr));
    }
  } break;
  case llvm::Instruction::SDiv: {
    llvm::Value *val1 = inst.getOperand(0);
    llvm::Value *val2 = inst.getOperand(1);
    if (llvm::dyn_cast<llvm::ConstantInt>(val1))
      std::swap(val1, val2);

    temp::Temp *val1_temp =
        IsRsp(val1, inst.getFunction()->getName())
            ? reg_manager->GetRegister(frame::X64RegManager::Reg::RSP)
            : temp_map_->find(val1)->second;
    instr_list->Append(
        new assem::MoveInstr("movq `s0,`d0",
                             new temp::TempList(reg_manager->GetRegister(
                                 frame::X64RegManager::Reg::RAX)),
                             new temp::TempList(val1_temp)));
    instr_list->Append(
        new assem::MoveInstr("movq $0,`d0",
                             new temp::TempList(reg_manager->GetRegister(
                                 frame::X64RegManager::Reg::RDX)),
                             new temp::TempList()));

    if (llvm::dyn_cast<llvm::Instruction>(val2)) {
      temp::Temp *val2_temp =
          IsRsp(val2, inst.getFunction()->getName())
              ? reg_manager->GetRegister(frame::X64RegManager::Reg::RSP)
              : temp_map_->find(val2)->second;
      instr_list->Append(new assem::OperInstr("idivq `s0", new temp::TempList(),
                                              new temp::TempList(val2_temp),
                                              nullptr));
    } else if (llvm::ConstantInt *constInt =
                   llvm::dyn_cast<llvm::ConstantInt>(val2)) {
      temp::Temp *temp = temp::TempFactory::NewTemp();
      instr_list->Append(new assem::MoveInstr(
          "movq $" + std::to_string(constInt->getSExtValue()) + ",`d0",
          new temp::TempList(temp), new temp::TempList()));
      instr_list->Append(new assem::OperInstr("idivq `s0", new temp::TempList(),
                                              new temp::TempList(temp),
                                              nullptr));
    }
    instr_list->Append(new assem::MoveInstr(
        "movq `s0,`d0", new temp::TempList(temp_map_->find(&inst)->second),
        new temp::TempList(
            reg_manager->GetRegister(frame::X64RegManager::Reg::RAX))));
  } break;
  case llvm::Instruction::GetElementPtr: {
    llvm::GetElementPtrInst *gep_inst =
        llvm::dyn_cast<llvm::GetElementPtrInst>(&inst);
    temp::Temp *temp;

    instr_list->Append(new assem::MoveInstr(
        "movq `s0,`d0", new temp::TempList(temp_map_->find(gep_inst)->second),
        new temp::TempList(temp_map_->find(gep_inst->getOperand(0))->second)));
    for (int i = 1; i < inst.getNumOperands(); i++) {
      temp = temp::TempFactory::NewTemp();

      if (llvm::ConstantInt *constant_int =
              llvm::dyn_cast<llvm::ConstantInt>(inst.getOperand(i))) {
        instr_list->Append(new assem::MoveInstr(
            "movq $" + std::to_string(constant_int->getSExtValue()) + ",`d0",
            new temp::TempList(temp), new temp::TempList()));
      } else {
        instr_list->Append(new assem::MoveInstr(
            "movq `s0,`d0", new temp::TempList(temp),
            new temp::TempList(
                temp_map_->find(gep_inst->getOperand(i))->second)));
      }
      instr_list->Append(
          new assem::OperInstr("imulq $8,`d0", new temp::TempList(temp),
                               new temp::TempList(temp), nullptr));
    }
    instr_list->Append(new assem::OperInstr(
        "addq `s0,`d0", new temp::TempList(temp_map_->find(gep_inst)->second),
        new temp::TempList({temp, temp_map_->find(gep_inst)->second}),
        nullptr));
  } break;
  case llvm::Instruction::Call: {
    llvm::Function *func =
        llvm::dyn_cast<llvm::CallInst>(&inst)->getCalledFunction();
    auto regs = reg_manager->ArgRegs();
    auto tmp_iter = regs->GetList().begin();
    // If first arg is %rsp, we need to skip it.
    int i = IsRsp(inst.getOperand(0), inst.getFunction()->getName()) ? 1 : 0,
        num_args = inst.getNumOperands() - 1;

    for (; i < num_args && tmp_iter != regs->GetList().end(); ++i, ++tmp_iter) {
      if (llvm::ConstantInt *constant_int =
              llvm::dyn_cast<llvm::ConstantInt>(inst.getOperand(i))) {
        instr_list->Append(new assem::MoveInstr(
            "movq $" + std::to_string(constant_int->getSExtValue()) + ",`d0",
            new temp::TempList(*tmp_iter), new temp::TempList()));
      } else {
        temp::Temp *src_temp =
            IsRsp(inst.getOperand(i), inst.getFunction()->getName())
                ? reg_manager->GetRegister(frame::X64RegManager::Reg::RSP)
                : temp_map_->find(inst.getOperand(i))->second;
        instr_list->Append(new assem::MoveInstr("movq `s0,`d0",
                                                new temp::TempList(*tmp_iter),
                                                new temp::TempList(src_temp)));
      }
    }

    if (i < num_args) {
      auto last_sp = temp::TempFactory::NewTemp();
      instr_list->Append(
          new assem::MoveInstr("movq %rsp,`d0", new temp::TempList(last_sp),
                               new temp::TempList(reg_manager->GetRegister(
                                   frame::X64RegManager::Reg::RSP))));
      instr_list->Append(new assem::OperInstr(
          "addq $" + std::string(func->getName()) + "_framesize_local,`s0",
          new temp::TempList(last_sp),
          new temp::TempList({last_sp, reg_manager->GetRegister(
                                           frame::X64RegManager::Reg::RSP)}),
          nullptr));
      while (i < num_args) {
        if (llvm::ConstantInt *constant_int =
                llvm::dyn_cast<llvm::ConstantInt>(inst.getOperand(i))) {
          instr_list->Append(new assem::MoveInstr(
              "movq $" + std::to_string(constant_int->getSExtValue()) + "," +
                  std::to_string(8 * i) + "(`s1)",
              new temp::TempList(), new temp::TempList(last_sp)));
        } else {
          temp::Temp *src_temp =
              IsRsp(inst.getOperand(i), inst.getFunction()->getName())
                  ? reg_manager->GetRegister(frame::X64RegManager::Reg::RSP)
                  : temp_map_->find(inst.getOperand(i))->second;
          instr_list->Append(new assem::MoveInstr(
              "movq `s0," + std::to_string(8 * i) + "(`s1)",
              new temp::TempList(), new temp::TempList({src_temp, last_sp})));
        }
        ++i;
      }
    }
    // assem::Targets *targets = new assem::Targets(new std::vector<temp::Label
    // *>(
    //   {temp::LabelFactory::NamedLabel(std::string(func -> getName()))}));
    instr_list->Append(new assem::OperInstr(
        "callq " + std::string(func->getName()), reg_manager->CallerSaves(),
        new temp::TempList(), nullptr));
    instr_list->Append(new assem::MoveInstr(
        "movq `s0,`d0", new temp::TempList(temp_map_->find(&inst)->second),
        new temp::TempList(
            reg_manager->GetRegister(frame::X64RegManager::Reg::RAX))));
  } break;

  case llvm::Instruction::Ret: {
    llvm::ReturnInst *return_inst = llvm::dyn_cast<llvm::ReturnInst>(&inst);

    if (inst.getNumOperands() > 0) {
      llvm::Value *val = inst.getOperand(0);
      if (llvm::ConstantInt *constInt =
              llvm::dyn_cast<llvm::ConstantInt>(val)) {
        instr_list->Append(new assem::MoveInstr(
            "movq $" + std::to_string(constInt->getSExtValue()) + ",%rax",
            new temp::TempList(
                reg_manager->GetRegister(frame::X64RegManager::Reg::RAX)),
            new temp::TempList()));
      } else if (llvm::dyn_cast<llvm::Instruction>(val)) {
        instr_list->Append(new assem::MoveInstr(
            "movq `s0,%rax",
            new temp::TempList(
                reg_manager->GetRegister(frame::X64RegManager::Reg::RAX)),
            new temp::TempList(temp_map_->find(val)->second)));
      }
    }
    instr_list->Append(new assem::MoveInstr(
        "movq $" +
            std::to_string(bb_map_->find(return_inst->getParent())->second) +
            ",`d0",
        new temp::TempList(phi_temp_), new temp::TempList()));

    assem::Targets *targets = new assem::Targets(
        new std::vector<temp::Label *>({temp::LabelFactory::NamedLabel(
            std::string(return_inst->getFunction()->getName()) + "_end")}));
    instr_list->Append(new assem::OperInstr(
        "jmp " + std::string(return_inst->getFunction()->getName()) + "_end",
        new temp::TempList(), new temp::TempList(), targets));
  } break;

  case llvm::Instruction::Br: {
    llvm::BranchInst *branch_inst = llvm::dyn_cast<llvm::BranchInst>(&inst);
    if (branch_inst->isConditional()) {
      if (llvm::ConstantInt *constInt =
              llvm::dyn_cast<llvm::ConstantInt>(branch_inst->getOperand(0))) {
        instr_list->Append(new assem::OperInstr(
            "cmpq $" + std::to_string(constInt->getSExtValue()) + ",$1",
            new temp::TempList(), new temp::TempList(), nullptr));
      } else if (llvm::dyn_cast<llvm::Instruction>(
                     branch_inst->getOperand(0))) {
        instr_list->Append(new assem::OperInstr(
            "cmpq $1,`s0", new temp::TempList(),
            new temp::TempList(
                temp_map_->find(branch_inst->getOperand(0))->second),
            nullptr));
      }

      /**
       *  Operand(1) is false branch, Operand(2) is true branch.
       */
      instr_list->Append(new assem::MoveInstr(
          "movq $" +
              std::to_string(bb_map_->find(branch_inst->getParent())->second) +
              ",`d0",
          new temp::TempList(phi_temp_), new temp::TempList()));

      assem::Targets *target1 = new assem::Targets(
          new std::vector<temp::Label *>({temp::LabelFactory::NamedLabel(
              branch_inst->getOperand(1)->getName())}));
      instr_list->Append(new assem::OperInstr(
          "jne " + std::string(branch_inst->getOperand(1)->getName()),
          new temp::TempList(), new temp::TempList(), target1));

      instr_list->Append(new assem::MoveInstr(
          "movq $" +
              std::to_string(bb_map_->find(branch_inst->getParent())->second) +
              ",`d0",
          new temp::TempList(phi_temp_), new temp::TempList()));

      assem::Targets *target2 = new assem::Targets(
          new std::vector<temp::Label *>({temp::LabelFactory::NamedLabel(
              branch_inst->getOperand(2)->getName())}));
      instr_list->Append(new assem::OperInstr(
          "jmp " + std::string(branch_inst->getOperand(2)->getName()),
          new temp::TempList(), new temp::TempList(), target2));
    } else {
      instr_list->Append(new assem::MoveInstr(
          "movq $" +
              std::to_string(bb_map_->find(branch_inst->getParent())->second) +
              ",`d0",
          new temp::TempList(phi_temp_), new temp::TempList()));

      assem::Targets *target = new assem::Targets(
          new std::vector<temp::Label *>({temp::LabelFactory::NamedLabel(
              branch_inst->getOperand(0)->getName())}));
      instr_list->Append(new assem::OperInstr(
          "jmp " + std::string(branch_inst->getOperand(0)->getName()),
          new temp::TempList(), new temp::TempList(), target));
    }
  } break;

  case llvm::Instruction::ICmp: {
    llvm::ICmpInst *cmp_inst = llvm::dyn_cast<llvm::ICmpInst>(&inst);
    llvm::Value *val1 = cmp_inst->getOperand(0);
    llvm::Value *val2 = cmp_inst->getOperand(1);

    llvm::ConstantInt *constInt1 = llvm::dyn_cast<llvm::ConstantInt>(val1);
    llvm::ConstantInt *constInt2 = llvm::dyn_cast<llvm::ConstantInt>(val2);

    /**
     *  cmpq b, a is like computing a - b without setting destination. Pay
     * attention to the order.
     */
    if (constInt1 && constInt2) {
      instr_list->Append(new assem::OperInstr(
          "cmpq $" + std::to_string(constInt2->getSExtValue()) + ",$" +
              std::to_string(constInt1->getSExtValue()),
          new temp::TempList(), new temp::TempList(), nullptr));
    } else if (constInt1) {
      instr_list->Append(new assem::OperInstr(
          "cmpq `s0,$" + std::to_string(constInt1->getSExtValue()),
          new temp::TempList(),
          new temp::TempList(temp_map_->find(val2)->second), nullptr));
    } else if (constInt2) {
      instr_list->Append(new assem::OperInstr(
          "cmpq $" + std::to_string(constInt2->getSExtValue()) + ",`s0",
          new temp::TempList(),
          new temp::TempList(temp_map_->find(val1)->second), nullptr));
    } else {
      instr_list->Append(new assem::OperInstr(
          "cmpq `s0,`s1", new temp::TempList(),
          new temp::TempList(
              {temp_map_->find(val2)->second, temp_map_->find(val1)->second}),
          nullptr));
    }

    instr_list->Append(new assem::OperInstr(
        "movq $0,`d0", new temp::TempList(temp_map_->find(cmp_inst)->second),
        new temp::TempList(), nullptr));
    switch (cmp_inst->getPredicate()) {
    case llvm::CmpInst::Predicate::ICMP_EQ:
      instr_list->Append(new assem::OperInstr(
          "sete `d0", new temp::TempList(temp_map_->find(cmp_inst)->second),
          new temp::TempList(), nullptr));
      break;
    case llvm::CmpInst::Predicate::ICMP_NE:
      instr_list->Append(new assem::OperInstr(
          "setne `d0", new temp::TempList(temp_map_->find(cmp_inst)->second),
          new temp::TempList(), nullptr));
      break;
    case llvm::CmpInst::Predicate::ICMP_SLT:
      instr_list->Append(new assem::OperInstr(
          "setl `d0", new temp::TempList(temp_map_->find(cmp_inst)->second),
          new temp::TempList(), nullptr));
      break;
    case llvm::CmpInst::Predicate::ICMP_SLE:
      instr_list->Append(new assem::OperInstr(
          "setle `d0", new temp::TempList(temp_map_->find(cmp_inst)->second),
          new temp::TempList(), nullptr));
      break;
    case llvm::CmpInst::Predicate::ICMP_SGT:
      instr_list->Append(new assem::OperInstr(
          "setg `d0", new temp::TempList(temp_map_->find(cmp_inst)->second),
          new temp::TempList(), nullptr));
      break;
    case llvm::CmpInst::Predicate::ICMP_SGE:
      instr_list->Append(new assem::OperInstr(
          "setge `d0", new temp::TempList(temp_map_->find(cmp_inst)->second),
          new temp::TempList(), nullptr));
      break;
    }
  } break;

    /**
     * 这种方法好就好在jmp前只需要传bb的index就行。另一种实现还需要知道传哪个temp，所以需要维护一个from_bb,
     * to_bb -> llvm::Value的映射。 而且还不一定查得到to_bb, 比如ret指令。
     * 但缺点就是汇编代码太冗余了。
     */
  case llvm::Instruction::PHI: {

    llvm::PHINode *phi_node = llvm::dyn_cast<llvm::PHINode>(&inst);
    std::vector<std::string> random_codes;
    unsigned num_args = phi_node->getNumOperands();
    for (int i = 0; i <= num_args; i++)
      random_codes.push_back(std::to_string(rand()));

    for (int i = 0; i < num_args; i++) {
      llvm::BasicBlock *incomingBlock = phi_node->getIncomingBlock(i);

      std::string phi_label =
          std::string(phi_node->getParent()->getName()) + "_" + random_codes[i];
      instr_list->Append(new assem::OperInstr(
          "cmpq $" + std::to_string(bb_map_->find(incomingBlock)->second) +
              ",`s0",
          new temp::TempList(), new temp::TempList(phi_temp_), nullptr));
      assem::Targets *target =
          new assem::Targets(new std::vector<temp::Label *>(
              {temp::LabelFactory::NamedLabel(phi_label)}));
      instr_list->Append(new assem::OperInstr("je " + phi_label,
                                              new temp::TempList(),
                                              new temp::TempList(), target));
    }

    std::string phi_end_label = std::string(phi_node->getParent()->getName()) +
                                "_end_" + random_codes.back();
    for (int i = 0; i < num_args; i++) {
      llvm::Value *incomingValue = phi_node->getIncomingValue(i);

      std::string phi_label =
          std::string(phi_node->getParent()->getName()) + "_" + random_codes[i];
      instr_list->Append(new assem::LabelInstr(phi_label));
      /**
       *  If phi incomming is constantInt, it is bool. So we need to use
       * getZExtValue. If is null, it is $0 Otherwise it should be in temp_map_
       */
      if (llvm::ConstantInt *constant_int =
              llvm::dyn_cast<llvm::ConstantInt>(incomingValue)) {
        int64_t constant_int_val = constant_int->getBitWidth() == 1
                                       ? constant_int->getZExtValue()
                                       : constant_int->getSExtValue();
        instr_list->Append(new assem::MoveInstr(
            "movq $" + std::to_string(constant_int_val) + ",`d0",
            new temp::TempList(temp_map_->find(phi_node)->second),
            new temp::TempList()));
      } else if (llvm::dyn_cast<llvm::ConstantPointerNull>(incomingValue)) {
        instr_list->Append(new assem::MoveInstr(
            "movq $0,`d0",
            new temp::TempList(temp_map_->find(phi_node)->second),
            new temp::TempList()));
      } else {
        instr_list->Append(new assem::MoveInstr(
            "movq `s0,`d0",
            new temp::TempList(temp_map_->find(phi_node)->second),
            new temp::TempList(temp_map_->find(incomingValue)->second)));
      }
      assem::Targets *target =
          new assem::Targets(new std::vector<temp::Label *>(
              {temp::LabelFactory::NamedLabel(phi_end_label)}));
      instr_list->Append(new assem::OperInstr("jmp " + phi_end_label,
                                              new temp::TempList(),
                                              new temp::TempList(), target));
    }
    instr_list->Append(new assem::LabelInstr(phi_end_label));
  } break;

  case llvm::Instruction::Load: {
    this->load_codegen(instr_list, llvm::dyn_cast<llvm::LoadInst>(&inst));
    break;
  }
  // case llvm::Instruction::Add: {
  //   break;
  // }
  // case llvm::Instruction::Sub: {
  //   break;
  // }
  // case llvm::Instruction::Mul: {
  //   break;
  // }
  // case llvm::Instruction::SDiv: {
  //   break;
  // }
  case llvm::Instruction::PtrToInt: {
    this->ptrtoint_codegen(instr_list,
                           llvm::dyn_cast<llvm::PtrToIntInst>(&inst));
    break;
  }
  case llvm::Instruction::IntToPtr: {
    this->inttoptr_codegen(instr_list,
                           llvm::dyn_cast<llvm::IntToPtrInst>(&inst));
    break;
  }
  // case llvm::Instruction::GetElementPtr: {
  //   break;
  // }
  case llvm::Instruction::Store: {
    this->store_codegen(instr_list, llvm::dyn_cast<llvm::StoreInst>(&inst));
    break;
  }
  case llvm::Instruction::ZExt: {
    this->zext_codegen(instr_list, llvm::dyn_cast<llvm::ZExtInst>(&inst));
    break;
  }
  // case llvm::Instruction::Call: {
  //   break;
  // }
  // case llvm::Instruction::Ret: {
  //   break;
  // }
  // case llvm::Instruction::Br: {
  //   break;
  // }
  // case llvm::Instruction::ICmp: {
  //   break;
  // }
  // case llvm::Instruction::PHI: {
  //   break;
  // }
  default:
    std::cout << inst.getOpcodeName() << std::endl;
    throw std::runtime_error(std::string("Unknown instruction: ") +
                             inst.getOpcodeName());
  }
}

void CodeGen::load_codegen(assem::InstrList *instr_list,
                           llvm::LoadInst *load_inst) {
  // %result = load i32, i32* %ptr
  // 两种情况：
  // 1. 从全局变量加载: movq global_var(%rip), %rbx  ; result存在rbx中
  // 2. 从内存地址加载: movq (%rax), %rbx            ;
  // %ptr在rax中，result存在rbx中

  auto dst = this->temp_map_->find(load_inst);

  if (llvm::dyn_cast<llvm::GlobalVariable>(load_inst->getPointerOperand())) {
    // 情况1：从全局变量加载
    std::string global_name =
        std::string(load_inst->getPointerOperand()->getName());
    instr_list->Append(new assem::MoveInstr(
        "movq " + global_name + "(%rip),`d0", new temp::TempList(dst->second),
        new temp::TempList()));
  } else {
    // 情况2：从内存地址加载
    auto src = this->temp_map_->find(load_inst->getPointerOperand());
    instr_list->Append(new assem::MoveInstr("movq (`s0),`d0",
                                            new temp::TempList(dst->second),
                                            new temp::TempList(src->second)));
  }
}

void CodeGen::store_codegen(assem::InstrList *instr_list,
                            llvm::StoreInst *store_inst) {
  // 两种情况：
  // 1. 存储一个立即数: movq $<constant>,(%rbx)    ; %ptr在rbx中
  // 2. 存储一个寄存器值: movq %rax,(%rbx)        ; %value在rax中，%ptr在rbx中

  if (llvm::ConstantInt *constant_int =
          llvm::dyn_cast<llvm::ConstantInt>(store_inst->getValueOperand())) {
    // 情况1：存储一个立即数
    auto dst_ptr = this->temp_map_->find(store_inst->getPointerOperand());
    instr_list->Append(new assem::MoveInstr(
        "movq $" + std::to_string(constant_int->getSExtValue()) + ",(`s0)",
        new temp::TempList(), new temp::TempList(dst_ptr->second)));
  } else {
    // 情况2：存储一个寄存器值
    auto src_value = this->temp_map_->find(store_inst->getValueOperand());
    auto dst_ptr = this->temp_map_->find(store_inst->getPointerOperand());
    instr_list->Append(new assem::MoveInstr(
        "movq `s0,(`s1)", new temp::TempList(),
        new temp::TempList({src_value->second, dst_ptr->second})));
  }
}

void CodeGen::add_sub_mul_codegen(assem::InstrList *instr_list,
                                  llvm::LoadInst &inst) {}
void CodeGen::sdiv_codegen(assem::InstrList *instr_list, llvm::LoadInst &inst) {
}
void CodeGen::ptrtoint_codegen(assem::InstrList *instr_list,
                               llvm::PtrToIntInst *inst) {
  // %result = ptrtoint i32* %ptr to i64 => movq %rax, %rbx   ;
  // %ptr在rax中，result存在rbx中
  auto src = this->temp_map_->find(inst->getPointerOperand());
  auto dst = this->temp_map_->find(inst);
  instr_list->Append(new assem::MoveInstr("movq `s0,`d0",
                                          new temp::TempList(dst->second),
                                          new temp::TempList(src->second)));
}
void CodeGen::inttoptr_codegen(assem::InstrList *instr_list,
                               llvm::IntToPtrInst *inst) {
  // %result = inttoptr i64 %val to i32* => movq %rax, %rbx   ;
  // %val在rax中，result存在rbx中
  auto src = this->temp_map_->find(inst->getOperand(0));
  auto dst = this->temp_map_->find(inst);
  instr_list->Append(new assem::MoveInstr("movq `s0,`d0",
                                          new temp::TempList(dst->second),
                                          new temp::TempList(src->second)));
}

void CodeGen::getelementptr_codegen(assem::InstrList *instr_list,
                                    llvm::LoadInst &inst) {}
void CodeGen::zext_codegen(assem::InstrList *instr_list, llvm::ZExtInst *inst) {
  // %result = zext i1 %bool_val to i32 => movq %source, %dest
  instr_list->Append(new assem::MoveInstr(
      "movq `s0,`d0", new temp::TempList(temp_map_->find(inst)->second),
      new temp::TempList(temp_map_->find(inst->getOperand(0))->second)));
}
void CodeGen::call_codegen(assem::InstrList *instr_list, llvm::LoadInst &inst) {
}
void CodeGen::ret_codegen(assem::InstrList *instr_list, llvm::LoadInst &inst) {}
void CodeGen::br_codegen(assem::InstrList *instr_list, llvm::LoadInst &inst) {}
void CodeGen::icmp_codegen(assem::InstrList *instr_list, llvm::LoadInst &inst) {
}
void CodeGen::phi_codegen(assem::InstrList *instr_list, llvm::LoadInst &inst) {}

} // namespace cg