#include "tiger/codegen/codegen.h"
#include "tiger/codegen/assem.h"
#include "tiger/frame/temp.h"

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
    // my
  case llvm::Instruction::Load: {
    this->load_codegen(instr_list, llvm::dyn_cast<llvm::LoadInst>(&inst));
    break;
  }
  case llvm::Instruction::Add: {
    this->add_sub_mul_codegen(instr_list,
                              llvm::cast<llvm::BinaryOperator>(inst),
                              function_name, "addq ");
    break;
  }
  case llvm::Instruction::Sub: {
    this->add_sub_mul_codegen(instr_list,
                              llvm::cast<llvm::BinaryOperator>(inst),
                              function_name, "subq ");
    break;
  }
  case llvm::Instruction::Mul: {
    this->add_sub_mul_codegen(instr_list,
                              llvm::cast<llvm::BinaryOperator>(inst),
                              function_name, "imulq ");
    break;
  }
  case llvm::Instruction::SDiv: {
    this->div_codegen(instr_list, llvm::cast<llvm::BinaryOperator>(inst),
                      function_name);
    break;
  }
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
  case llvm::Instruction::GetElementPtr: {
    llvm::GetElementPtrInst *gep_inst =
        llvm::dyn_cast<llvm::GetElementPtrInst>(&inst);
    temp::Temp *final_temp;
    auto dst_value = temp_map_->find(gep_inst)->second;
    auto basic_value = temp_map_->find(gep_inst->getOperand(0))->second;
    auto dst_temp_list = new temp::TempList(dst_value);
    auto basic_temp_list = new temp::TempList(basic_value);
    auto move_instr =
        new assem::MoveInstr("movq `s0,`d0", dst_temp_list, basic_temp_list);
    instr_list->Append(move_instr);
    int scale_factor = 8; // 初始为元素大小
    llvm::Type *current_type =
        gep_inst->getPointerOperandType()->getPointerElementType();

    for (int i = 1; i < gep_inst->getNumOperands(); i++) {
      auto temp = temp::TempFactory::NewTemp();

      // 加载索引值
      if (llvm::ConstantInt *constant_int =
              llvm::dyn_cast<llvm::ConstantInt>(gep_inst->getOperand(i))) {
        instr_list->Append(new assem::MoveInstr(
            "movq $" + std::to_string(constant_int->getSExtValue()) + ",`d0",
            new temp::TempList(temp), new temp::TempList()));
      } else {
        instr_list->Append(new assem::MoveInstr(
            "movq `s0,`d0", new temp::TempList(temp),
            new temp::TempList(
                temp_map_->find(gep_inst->getOperand(i))->second)));
      }

      // 动态调整 scale_factor
      if (auto *array_type = llvm::dyn_cast<llvm::ArrayType>(current_type)) {
        scale_factor *= array_type->getNumElements(); // 当前维度的元素数量
        current_type = array_type->getElementType(); // 更新为下一层类型
      }

      instr_list->Append(new assem::OperInstr(
          "imulq $" + std::to_string(scale_factor) + ",`d0",
          new temp::TempList(temp), new temp::TempList(temp), nullptr));
      final_temp = temp;
    }
    instr_list->Append(new assem::OperInstr(
        "addq `s0,`d0", new temp::TempList(temp_map_->find(gep_inst)->second),
        new temp::TempList({final_temp, temp_map_->find(gep_inst)->second}),
        nullptr));
    break;
  }
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
  case llvm::Instruction::Ret: {
    this->ret_codegen(instr_list, llvm::dyn_cast<llvm::ReturnInst>(&inst),
                      function_name, bb);
    break;
  }
  // case llvm::Instruction::Br: {
  //   break;
  // }
  case llvm::Instruction::ICmp: {
    this->icmp_codegen(instr_list, llvm::dyn_cast<llvm::ICmpInst>(&inst));
    break;
  }
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
                                  llvm::BinaryOperator &inst,
                                  std::string_view function_name,
                                  std::string op_str) {
  //  对终值的类型进行讨论
  temp::TempList *dst_list = new temp::TempList();
  if (IsRsp(&inst, function_name)) {
    /**
     *addq或subq可能更改%rsp，在输入或退出功能时更改。
     *ir中的前两行更改了%rsp，但ProcEntryExit3已经更改了
     */
    return;
  }
  dst_list->Append(temp_map_->at(&inst));
  // 对左右值的类型进行讨论
  temp::TempList *left_list = new temp::TempList();
  std::string left_string = "";
  auto left_value = inst.getOperand(0);
  llvm::ConstantInt *left_const = llvm::dyn_cast<llvm::ConstantInt>(left_value);
  if (left_const) {
    // 左值是常数
    left_string = "$" + std::to_string(left_const->getSExtValue());
  }
  if (IsRsp(left_value, function_name)) {
    left_string = "`s0";
    left_list->Append(reg_manager->GetRegister(frame::X64RegManager::Reg::RSP));
  }
  if (left_string == "") {
    //说明既不是常数，也不是rsp
    left_string = "`s0";
    left_list->Append(this->temp_map_->at(left_value));
  }
  temp::TempList *right_list = new temp::TempList();
  std::string right_string = "";
  auto right_value = inst.getOperand(1);
  llvm::ConstantInt *right_const =
      llvm::dyn_cast<llvm::ConstantInt>(right_value);

  if (right_const) {
    // 右值是常数
    right_string = "$" + std::to_string(right_const->getSExtValue());
  }

  if (IsRsp(right_value, function_name)) {
    right_string = "`s0";
    right_list->Append(
        reg_manager->GetRegister(frame::X64RegManager::Reg::RSP));
  }

  if (right_string == "") {
    // 说明既不是常数，也不是rsp
    right_string = "`s0";
    right_list->Append(this->temp_map_->at(right_value));
  }
  instr_list->Append(new assem::MoveInstr("movq " + left_string + "," + "`d0",
                                          dst_list, left_list));
  instr_list->Append(new assem::OperInstr(op_str + right_string + "," + "`d0",
                                          dst_list, right_list, nullptr));
}

void CodeGen::div_codegen(assem::InstrList *instr_list,
                          llvm::BinaryOperator &inst,
                          std::string_view function_name) {
  temp::TempList *dst_list = new temp::TempList();
  dst_list->Append(temp_map_->at(&inst));
  // 对左操作数（分子）进行分析
  temp::TempList *left_list = new temp::TempList();
  std::string left_string = "";
  auto left_value = inst.getOperand(0);
  llvm::ConstantInt *left_const = llvm::dyn_cast<llvm::ConstantInt>(left_value);

  if (left_const) {
    // 如果左操作数是常量
    left_string = "$" + std::to_string(left_const->getSExtValue());
  }
  if (IsRsp(left_value, function_name)) {
    // 如果左操作数是%rsp
    left_string = "`s0";
    left_list->Append(reg_manager->GetRegister(frame::X64RegManager::Reg::RSP));
  }
  if (left_string == "") {
    // 如果左操作数既不是常量也不是%rsp
    left_string = "`s0";
    left_list->Append(this->temp_map_->at(left_value));
  }

  // 对右操作数（除数）进行分析
  temp::TempList *right_list = new temp::TempList();
  std::string right_string = "";
  auto right_value = inst.getOperand(1);
  llvm::ConstantInt *right_const =
      llvm::dyn_cast<llvm::ConstantInt>(right_value);

  if (right_const) {
    // 如果右操作数是常量
    right_string = "$" + std::to_string(right_const->getSExtValue());
  }
  if (IsRsp(right_value, function_name)) {
    // 如果右操作数是%rsp
    right_string = "`s0";
    right_list->Append(
        reg_manager->GetRegister(frame::X64RegManager::Reg::RSP));
  }
  if (right_string == "") {
    // 如果右操作数既不是常量也不是%rsp
    right_string = "`s0";
    right_list->Append(this->temp_map_->at(right_value));
  }

  // 生成指令：将分子加载到%RAX
  instr_list->Append(
      new assem::MoveInstr("movq " + left_string + "," + "`d0",
                           new temp::TempList(reg_manager->GetRegister(
                               frame::X64RegManager::Reg::RAX)),
                           left_list));

  // 生成指令：符号扩展 %RAX 到 %RDX
  instr_list->Append(new assem::OperInstr("cqto", new temp::TempList(),
                                          new temp::TempList(), nullptr));

  // 生成指令：执行有符号除法
  instr_list->Append(new assem::OperInstr(
      "idivq " + right_string, new temp::TempList(), right_list, nullptr));

  // 将结果存储到目标寄存器
  std::string tmp = "movq `s0,";
  tmp = tmp + "`d0";
  instr_list->Append(
      new assem::MoveInstr(tmp, dst_list,
                           new temp::TempList(reg_manager->GetRegister(
                               frame::X64RegManager::Reg::RAX))));
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
void CodeGen::zext_codegen(assem::InstrList *instr_list, llvm::ZExtInst *inst) {
  // %result = zext i1 %bool_val to i32 => movq %source, %dest
  instr_list->Append(new assem::MoveInstr(
      "movq `s0,`d0", new temp::TempList(temp_map_->find(inst)->second),
      new temp::TempList(temp_map_->find(inst->getOperand(0))->second)));
}
void CodeGen::call_codegen(assem::InstrList *instr_list, llvm::LoadInst &inst) {
}
void CodeGen::ret_codegen(assem::InstrList *instr_list, llvm::ReturnInst *inst,
                          std::string_view function_name,
                          llvm::BasicBlock *bb) {
  if (inst->getNumOperands() == 1) {
    // 说明返回值存在，不是void
    // 将返回值movq到 %rax
    auto rax_reg = reg_manager->GetRegister(frame::X64RegManager::Reg::RAX);
    temp::TempList *dst_list = new temp::TempList();
    dst_list->Append(rax_reg);
    std::string src_str = "";
    temp::TempList *src_list = new temp::TempList();
    llvm::ConstantInt *ret_value =
        llvm::dyn_cast<llvm::ConstantInt>(inst->getOperand(0));
    if (ret_value) {
      src_str = "$" + std::to_string(ret_value->getSExtValue());
    }
    if (IsRsp(inst->getOperand(0), function_name)) {
      src_str = "`s0";
      src_list->Append(
          reg_manager->GetRegister(frame::X64RegManager::Reg::RSP));
    }
    if (src_str == "") {
      src_str = "`s0";
      src_list->Append(temp_map_->at(inst->getOperand(0)));
    }

    instr_list->Append(
        new assem::MoveInstr("movq " + src_str + ",`d0", dst_list, src_list));
  }
  instr_list->Append(new assem::MoveInstr(
      "movq $" + std::to_string(bb_map_->at(inst->getParent())) + ",`d0",
      new temp::TempList(phi_temp_), new temp::TempList()));
  std::string end_label_name = std::string(function_name) + "_end";
  auto target_labels =
      std::make_unique<std::vector<temp::Label *>>(std::vector<temp::Label *>{
          temp::LabelFactory::NamedLabel(end_label_name)});
  std::string jmp_instr_str = "jmp " + end_label_name;
  auto jmp_instr = std::make_unique<assem::OperInstr>(
      jmp_instr_str, new temp::TempList(), new temp::TempList(),
      new assem::Targets(target_labels.release()));
  instr_list->Append(jmp_instr.release());
}
void CodeGen::br_codegen(assem::InstrList *instr_list, llvm::LoadInst &inst) {}
void CodeGen::icmp_codegen(assem::InstrList *instr_list, llvm::ICmpInst *inst) {
  auto value_1 = inst->getOperand(0);
  auto value_2 = inst->getOperand(1);
  std::string str_1;
  std::string str_2;
  temp::TempList *src_list = new temp::TempList();
  auto *constInt1 = llvm::dyn_cast<llvm::ConstantInt>(value_1);
  auto *constInt2 = llvm::dyn_cast<llvm::ConstantInt>(value_2);
  if (constInt1 && constInt2) {
    // icmp eq i32 5, 3 => cmpq $3, $5
    instr_list->Append(new assem::OperInstr(
        "cmpq $" + std::to_string(constInt2->getSExtValue()) + ", $" +
            std::to_string(constInt1->getSExtValue()),
        new temp::TempList(), new temp::TempList(), nullptr));
  }

  if (constInt1 && !constInt2) {
    // icmp eq i32 %reg, 5 => cmpq `s0, $5
    instr_list->Append(new assem::OperInstr(
        "cmpq `s0, $" + std::to_string(constInt1->getSExtValue()),
        new temp::TempList(),
        new temp::TempList(temp_map_->find(value_2)->second), nullptr));
  }

  if (!constInt1 && constInt2) {
    // icmp eq i32 5, %reg => cmpq $5, `s0
    instr_list->Append(new assem::OperInstr(
        "cmpq $" + std::to_string(constInt2->getSExtValue()) + ", `s0",
        new temp::TempList(),
        new temp::TempList(temp_map_->find(value_1)->second), nullptr));
  }

  if (!constInt1 && !constInt2) {
    // icmp eq i32 %reg1, %reg2 => cmpq `s0, `s1
    instr_list->Append(new assem::OperInstr(
        "cmpq `s0, `s1", new temp::TempList(),
        new temp::TempList({temp_map_->find(value_2)->second,
                            temp_map_->find(value_1)->second}),
        nullptr));
  }
  static const std::unordered_map<llvm::CmpInst::Predicate, std::string>
      predicateToAssem = {{llvm::CmpInst::Predicate::ICMP_EQ, "sete"},
                          {llvm::CmpInst::Predicate::ICMP_NE, "setne"},
                          {llvm::CmpInst::Predicate::ICMP_SLT, "setl"},
                          {llvm::CmpInst::Predicate::ICMP_SLE, "setle"},
                          {llvm::CmpInst::Predicate::ICMP_SGT, "setg"},
                          {llvm::CmpInst::Predicate::ICMP_SGE, "setge"}};
  auto it = predicateToAssem.find(inst->getPredicate());
  if (it != predicateToAssem.end()) {
    // icmp eq i32 %reg1, %reg2 => sete `d0
    instr_list->Append(new assem::OperInstr(
        it->second + " `d0", new temp::TempList(temp_map_->find(inst)->second),
        new temp::TempList(), nullptr));
  } else {
    // Handle unsupported predicate (optional)
    throw std::runtime_error("Unsupported comparison predicate");
  }
}
void CodeGen::phi_codegen(assem::InstrList *instr_list, llvm::LoadInst &inst) {}

} // namespace cg