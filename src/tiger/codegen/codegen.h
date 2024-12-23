#ifndef TIGER_CODEGEN_CODEGEN_H_
#define TIGER_CODEGEN_CODEGEN_H_

#include "tiger/canon/canon.h"
#include "tiger/codegen/assem.h"
#include "tiger/frame/x64frame.h"
#include <llvm-14/llvm/IR/Instructions.h>

// Forward Declarations
namespace frame {
class RegManager;
class Frame;
} // namespace frame

namespace assem {
class Instr;
class InstrList;
} // namespace assem

namespace canon {
class Traces;
} // namespace canon

namespace cg {

class AssemInstr {
public:
  AssemInstr() = delete;
  explicit AssemInstr(assem::InstrList *instr_list) : instr_list_(instr_list) {}

  void Print(FILE *out, temp::Map *map) const;

  [[nodiscard]] assem::InstrList *GetInstrList() const { return instr_list_; }

private:
  assem::InstrList *instr_list_;
};

class CodeGen {
public:
  CodeGen(std::unique_ptr<canon::Traces> traces)
      : traces_(std::move(traces)), phi_temp_(temp::TempFactory::NewTemp()) {}

  void Codegen();

  // check if the value is %sp in llvm
  bool IsRsp(llvm::Value *val, std::string_view function_name) const {
    // TODO: your lab5 code here
    if (val->getName() == std::string(function_name) + "_sp") {
      return true;
    }
    return false;
  }

  // bb is to add move instruction to record which block it jumps from
  // function_name can be used to construct return or exit label
  void InstrSel(assem::InstrList *instr_list, llvm::Instruction &inst,
                std::string_view function_name, llvm::BasicBlock *bb);
  std::unique_ptr<AssemInstr> TransferAssemInstr() {
    return std::move(assem_instr_);
  }

private:
  // record mapping from llvm value to temp
  std::unordered_map<llvm::Value *, temp::Temp *> *temp_map_;
  // for phi node, record mapping from llvm basic block to index of the block,
  // to check which block it jumps from
  std::unordered_map<llvm::BasicBlock *, int> *bb_map_;
  // for phi node, use a temp to record which block it jumps from
  temp::Temp *phi_temp_;
  std::unique_ptr<canon::Traces> traces_;
  std::unique_ptr<AssemInstr> assem_instr_;
  void load_codegen(assem::InstrList *instr_list, llvm::LoadInst *inst);
  void store_codegen(assem::InstrList *instr_list, llvm::StoreInst *inst);
  void add_sub_mul_codegen(assem::InstrList *instr_list,
                           llvm::BinaryOperator &inst,
                           std::string_view function_name, std::string oper);
  void div_codegen(assem::InstrList *instr_list, llvm::BinaryOperator &inst,
                   std::string_view function_name);
  void ptrtoint_codegen(assem::InstrList *instr_list, llvm::PtrToIntInst *inst);
  void inttoptr_codegen(assem::InstrList *instr_list, llvm::IntToPtrInst *inst);
  void zext_codegen(assem::InstrList *instr_list, llvm::ZExtInst *inst);
  void call_codegen(assem::InstrList *instr_list, llvm::LoadInst &inst);
  void ret_codegen(assem::InstrList *instr_list, llvm::ReturnInst *inst,
                   std::string_view function_name, llvm::BasicBlock *bb);
  void br_codegen(assem::InstrList *instr_list, llvm::LoadInst &inst);
  void icmp_codegen(assem::InstrList *instr_list, llvm::ICmpInst *inst);
  void phi_codegen(assem::InstrList *instr_list, llvm::LoadInst &inst);
};

} // namespace cg
#endif