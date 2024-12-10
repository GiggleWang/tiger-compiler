#include "tiger/translate/translate.h"

#include <tiger/absyn/absyn.h>

#include "tiger/env/env.h"
#include "tiger/errormsg/errormsg.h"
#include "tiger/frame/x64frame.h"

#include "llvm/Support/FileSystem.h"
#include "llvm/Support/raw_ostream.h"
#include <iostream>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <stack>

extern frame::Frags *frags;
extern frame::RegManager *reg_manager;
extern llvm::IRBuilder<> *ir_builder;
extern llvm::Module *ir_module;
std::stack<llvm::Function *> func_stack;
std::stack<llvm::BasicBlock *> loop_stack;
llvm::Function *alloc_record;
llvm::Function *init_array;
llvm::Function *string_equal;
std::vector<std::pair<std::string, frame::Frame *>> frame_info;

bool CheckBBTerminatorIsBranch(llvm::BasicBlock *bb) {
  auto inst = bb->getTerminator();
  if (inst) {
    llvm::BranchInst *branchInst = llvm::dyn_cast<llvm::BranchInst>(inst);
    if (branchInst && !branchInst->isConditional()) {
      return true;
    }
  }
  return false;
}

int getActualFramesize(tr::Level *level) {
  return level->frame_->calculateActualFramesize();
}

namespace tr {

Access *Access::AllocLocal(Level *level, bool escape) {
  return new Access(level, level->frame_->AllocLocal(escape));
}

class ValAndTy {
public:
  type::Ty *ty_;
  llvm::Value *val_;
  llvm::BasicBlock *last_bb_;

  ValAndTy(llvm::Value *val, type::Ty *ty) : val_(val), ty_(ty) {}
};

void ProgTr::OutputIR(std::string_view filename) {
  std::string llvmfile = std::string(filename) + ".ll";
  std::error_code ec;
  llvm::raw_fd_ostream out(llvmfile, ec, llvm::sys::fs::OpenFlags::OF_Text);
  ir_module->print(out, nullptr);
}

// FINISHED
void ProgTr::Translate() {
  FillBaseVEnv();
  FillBaseTEnv();
  /* TODO: Put your lab5-part1 code here */
  // declare i64 @alloc_record(i32)
  llvm::FunctionType *alloc_record_func_type = llvm::FunctionType::get(
      ir_builder->getInt64Ty(), {ir_builder->getInt32Ty()}, false);
  alloc_record = llvm::Function::Create(alloc_record_func_type,
                                        llvm::Function::ExternalLinkage,
                                        "alloc_record", ir_module);

  // declare i64 @init_array(i32, i64)???
  llvm::FunctionType *init_array_func_type = llvm::FunctionType::get(
      ir_builder->getInt64Ty(),
      {ir_builder->getInt32Ty(), ir_builder->getInt64Ty()}, false);
  init_array = llvm::Function::Create(init_array_func_type,
                                      llvm::Function::ExternalLinkage,
                                      "init_array", ir_module);
  // declare i1 @string_equal(% string *, % string *)
  llvm::FunctionType *string_equal_func_type =
      llvm::FunctionType::get(ir_builder->getInt1Ty(),
                              {type::StringTy::Instance()->GetLLVMType(),
                               type::StringTy::Instance()->GetLLVMType()},
                              false);
  string_equal = llvm::Function::Create(string_equal_func_type,
                                        llvm::Function::ExternalLinkage,
                                        "string_equal", ir_module);

  // define i32 @tigermain(i64 %0, i64 %1)
  llvm::FunctionType *tiger_main_func_type = llvm::FunctionType::get(
      ir_builder->getInt32Ty(),
      {ir_builder->getInt64Ty(), ir_builder->getInt64Ty()}, false);
  llvm::Function *tiger_main_func = llvm::Function::Create(
      tiger_main_func_type, llvm::Function::ExternalLinkage, "tigermain",
      ir_module);

  llvm::GlobalVariable *global_frame_size = new llvm::GlobalVariable(
      llvm::Type::getInt64Ty(ir_builder->getContext()), true,
      llvm::GlobalValue::InternalLinkage,
      llvm::ConstantInt::get(llvm::Type::getInt64Ty(ir_builder->getContext()),
                             0),
      "tigermain_frame_size");
  ir_module->getGlobalList().push_back(global_frame_size);
  func_stack.push(tiger_main_func);
  auto tiger_main_block = llvm::BasicBlock::Create(
      ir_builder->getContext(), "tigermain", tiger_main_func);
  ir_builder->SetInsertPoint(tiger_main_block);
  llvm::Function::arg_iterator real_arg_it = tiger_main_func->arg_begin();
  main_level_->set_sp(real_arg_it);
  main_level_->frame_->framesize_global = global_frame_size;

  auto main_res = this->absyn_tree_->Translate(
      venv_.get(), tenv_.get(), main_level_.get(), errormsg_.get());
  global_frame_size->setInitializer(
      llvm::ConstantInt::get(llvm::Type::getInt64Ty(ir_builder->getContext()),
                             main_level_->frame_->calculateActualFramesize()));
  if (main_res->ty_ != type::VoidTy::Instance())
    ir_builder->CreateRet(main_res->val_);
  else
    ir_builder->CreateRet(llvm::ConstantInt::get(ir_builder->getInt32Ty(), 0));
  func_stack.pop();
}

} // namespace tr

namespace absyn {
// FINISHED
tr::ValAndTy *AbsynTree::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                   tr::Level *level,
                                   err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  return this->root_->Translate(venv, tenv, level, errormsg);
}
// FINISHED
void TypeDec::Translate(env::VEnvPtr venv, env::TEnvPtr tenv, tr::Level *level,
                        err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  // 第一遍：将所有类型名注册到环境中
  for (NameAndTy *nameAndTy : types_->GetList()) {
    tenv->Enter(nameAndTy->name_, new type::NameTy(nameAndTy->name_, nullptr));
  }

  // 第二遍：处理实际的类型定义
  for (NameAndTy *nameAndTy : types_->GetList()) {
    type::Ty *type = nameAndTy->ty_->Translate(tenv, errormsg);
    type::NameTy *nameTy =
        static_cast<type::NameTy *>(tenv->Look(nameAndTy->name_));
    nameTy->ty_ = type;
  }
}
// FINISHED
void FunctionDec::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                            tr::Level *currentLevel,
                            err::ErrorMsg *errormsg) const {
  /* First pass: register functions */
  for (auto funcDecl : this->functions_->GetList()) {
    type::TyList *formalTypes =
        funcDecl->params_->MakeFormalTyList(tenv, errormsg);
    type::Ty *returnType = type::VoidTy::Instance();
    if (funcDecl->result_) {
      returnType = tenv->Look(funcDecl->result_)->ActualTy();
    }
    std::vector<type::Ty *> paramTypes;
    std::list<bool> paramEscapes;
    // 添加静态链和帧指针参数
    std::vector<llvm::Type *> llvmParamTypes = {
        ir_builder->getInt64Ty(), // 静态链
        ir_builder->getInt64Ty()  // 帧指针
    };
    for (type::Ty *ty : formalTypes->GetList())
      llvmParamTypes.push_back(ty->GetLLVMType());

    for (const Field *param : funcDecl->params_->GetList()) {
      paramEscapes.push_back(param->escape_);
    }
    tr::Level *newLevel = currentLevel->NewLevel(
        nullptr, temp::LabelFactory::NamedLabel(funcDecl->name_->Name()),
        paramEscapes);
    frame_info.push_back({funcDecl->name_->Name(), newLevel->frame_});

    llvm::FunctionType *llvmFuncType = llvm::FunctionType::get(
        returnType->GetLLVMType(), llvmParamTypes, false);
    llvm::Function *llvmFunc =
        llvm::Function::Create(llvmFuncType, llvm::Function::ExternalLinkage,
                               funcDecl->name_->Name(), ir_module);

    venv->Enter(funcDecl->name_,
                new env::FunEntry(newLevel, formalTypes, returnType,
                                  llvmFuncType, llvmFunc));
  }
  llvm::BasicBlock *currentBB = ir_builder->GetInsertBlock();
  /* Second pass: translate function bodies */
  for (FunDec *funcDecl : functions_->GetList()) {
    env::FunEntry *funcEntry =
        static_cast<env::FunEntry *>(venv->Look(funcDecl->name_));
    funcEntry->level_->frame_->framesize_global = new llvm::GlobalVariable(
        ir_builder->getInt64Ty(), false, llvm::GlobalValue::InternalLinkage,
        ir_builder->getInt64(0), funcDecl->name_->Name() + "_framesize_global");
    ir_module->getGlobalList().push_back(
        funcEntry->level_->frame_->framesize_global);

    func_stack.push(funcEntry->func_);

    llvm::BasicBlock *entryBB = llvm::BasicBlock::Create(
        ir_module->getContext(), "entry", funcEntry->func_);
    ir_builder->SetInsertPoint(entryBB);

    // 开始新的作用域
    venv->BeginScope();
    tenv->BeginScope();

    auto formalTypeIt = funcEntry->formals_->GetList().begin();
    auto paramIt = funcDecl->params_->GetList().begin();
    auto accessIt = funcEntry->level_->frame_->Formals()->begin();
    auto argIt = funcEntry->func_->arg_begin();
    funcEntry->level_->set_sp(argIt++);
    llvm::Value *staticLinkAddr =
        (*(accessIt++))->ToLLVMVal(funcEntry->level_->get_sp());
    llvm::Value *staticLinkPtr = ir_builder->CreateIntToPtr(
        staticLinkAddr, ir_builder->getInt64Ty()->getPointerTo());
    ir_builder->CreateStore(argIt++, staticLinkPtr);

    while (paramIt != funcDecl->params_->GetList().end()) {
      // 获取参数地址
      llvm::Value *paramAddr =
          (*accessIt)->ToLLVMVal(funcEntry->level_->get_sp());
      llvm::Type *paramLLVMType = (*formalTypeIt)->GetLLVMType();

      // 将参数地址转换为指针类型
      llvm::Value *paramPtr =
          ir_builder->CreateIntToPtr(paramAddr, paramLLVMType->getPointerTo());

      // 存储参数值
      ir_builder->CreateStore(argIt, paramPtr);

      // 创建一个新的 Access 实例，表示该参数的访问方式
      tr::Access *paramAccess = new tr::Access(funcEntry->level_, *accessIt);

      // 将参数添加到符号表中
      venv->Enter((*paramIt)->name_,
                  new env::VarEntry(paramAccess, (*formalTypeIt)->ActualTy()));

      // 更新迭代器
      ++formalTypeIt;
      ++accessIt;
      ++paramIt;
      ++argIt;
    }

    tr::ValAndTy *result =
        funcDecl->body_->Translate(venv, tenv, funcEntry->level_, errormsg);
    // 如果函数的返回类型是 void，直接返回 void
    if (!funcDecl->result_ || dynamic_cast<type::VoidTy *>(result->ty_)) {
      ir_builder->CreateRetVoid();
    } else {
      // 如果有返回值并且类型需要转换
      llvm::Value *returnValue = result->val_;
      // 检查返回值类型是否与函数的返回类型一致
      if (returnValue &&
          funcEntry->func_->getReturnType() != returnValue->getType()) {
        // 如果返回类型是 int 类型，进行扩展转换
        if (result->ty_->IsSameType(type::IntTy::Instance())) {
          returnValue = ir_builder->CreateZExt(
              returnValue, funcEntry->func_->getReturnType());
        }
      }
      // 执行返回
      ir_builder->CreateRet(returnValue);
    }

    venv->EndScope();
    tenv->EndScope();
    func_stack.pop();
    funcEntry->level_->frame_->framesize_global->setInitializer(
        ir_builder->getInt64(getActualFramesize(funcEntry->level_)));
  }
  ir_builder->SetInsertPoint(currentBB);
}

// FINISHED
void VarDec::Translate(env::VEnvPtr venv, env::TEnvPtr tenv, tr::Level *level,
                       err::ErrorMsg *errormsg) const {
  auto init = this->init_->Translate(venv, tenv, level, errormsg);
  auto init_val = init->val_;
  auto init_type = init->ty_;
  auto new_access = tr::Access::AllocLocal(level, this->escape_);
  auto value = new_access->access_->ToLLVMVal(level->get_sp());
  auto value_ptr = ir_builder->CreateIntToPtr(
      value, init_type->GetLLVMType()->getPointerTo());
  ir_builder->CreateStore(init_val, value_ptr);
  venv->Enter(this->var_, new env::VarEntry(new_access, init_type->ActualTy()));
}
// FINISHED
type::Ty *NameTy::Translate(env::TEnvPtr tenv, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  // 避免产生循环，如type list = {first: int, rest: list}
  auto look_res = tenv->Look(this->name_);
  return new type::NameTy(this->name_, look_res);
}
// FINISHED
type::Ty *RecordTy::Translate(env::TEnvPtr tenv,
                              err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  type::FieldList *fields = new type::FieldList();
  // 遍历记录中的每个字段
  for (Field *field : record_->GetList()) {
    // 翻译字段的类型
    type::Ty *field_type = tenv->Look(field->typ_);
    // 创建新的类型字段，并添加到字段列表中
    fields->Append(new type::Field(field->name_, field_type));
  }
  return new type::RecordTy(fields);
}
// FINISHED
type::Ty *ArrayTy::Translate(env::TEnvPtr tenv, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  auto look_res = tenv->Look(this->array_);
  return new type::ArrayTy(look_res->ActualTy());
}
// FINISHED
tr::ValAndTy *SimpleVar::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                   tr::Level *level,
                                   err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  auto entry = venv->Look(this->sym_);
  env::VarEntry *var_entry = static_cast<env::VarEntry *>(entry);
  auto var_access = var_entry->access_;
  auto var_level = var_entry->access_->level_;
  auto cur_sp = level->get_sp(); // level的%sp
  while (level != var_level) {
    auto sl_formal = level->frame_->formals_->begin();
    llvm::Value *static_link_addr = (*sl_formal)->ToLLVMVal(cur_sp);
    llvm::Value *static_link_ptr = ir_builder->CreateIntToPtr(
        static_link_addr, llvm::Type::getInt64PtrTy(ir_builder->getContext()));
    cur_sp = ir_builder->CreateLoad(ir_builder->getInt64Ty(), static_link_ptr);
    level = level->parent_;
  }
  auto llvm_val = var_access->access_->ToLLVMVal(cur_sp);
  llvm::Value *val = ir_builder->CreateIntToPtr(
      llvm_val, llvm::PointerType::get(var_entry->ty_->GetLLVMType(), 0));
  return new tr::ValAndTy(val, var_entry->ty_->ActualTy());
}
// FINISHED
tr::ValAndTy *FieldVar::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                  tr::Level *level,
                                  err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  tr::ValAndTy *var = var_->Translate(venv, tenv, level, errormsg);
  type::RecordTy *record_ty =
      static_cast<type::RecordTy *>(var->ty_->ActualTy());
  llvm::Type *struct_type = var->ty_->GetLLVMType()->getPointerElementType();
  llvm::Value *struct_ptr =
      ir_builder->CreateLoad(var->ty_->GetLLVMType(), var->val_);
  int field_index = 0;
  type::Ty *field_type = nullptr;
  for (const auto &field : record_ty->fields_->GetList()) {
    if (field->name_ == sym_) {
      field_type = field->ty_;
      break;
    }
    field_index++;
  }
  std::vector<llvm::Value *> indices = {
      llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_builder->getContext()),
                             0),
      llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_builder->getContext()),
                             field_index)};
  llvm::Value *field_ptr =
      ir_builder->CreateGEP(struct_type, struct_ptr, indices, "field");
  return new tr::ValAndTy(field_ptr, field_type);
}
// FINISHED
tr::ValAndTy *SubscriptVar::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                      tr::Level *level,
                                      err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  tr::ValAndTy *var = this->var_->Translate(venv, tenv, level, errormsg);
  type::ArrayTy *array_ty = dynamic_cast<type::ArrayTy *>(var->ty_);
  tr::ValAndTy *subscript = subscript_->Translate(venv, tenv, level, errormsg);
  // 检查下标类型是否为整数
  if (!subscript->ty_->IsSameType(type::IntTy::Instance())) {
    errormsg->Error(pos_, "integer required");
    return nullptr;
  }
  llvm::Type *elem_ty = array_ty->ty_->ActualTy()->GetLLVMType();
  auto array_ptr = ir_builder->CreateLoad(var->ty_->GetLLVMType(), var->val_);
  auto target_val =
      ir_builder->CreateGEP(var->ty_->GetLLVMType()->getPointerElementType(),
                            array_ptr, subscript->val_);
  return new tr::ValAndTy(target_val, array_ty->ty_->ActualTy());
}
// FINISHED
tr::ValAndTy *VarExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level,
                                err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  tr::ValAndTy *var = var_->Translate(venv, tenv, level, errormsg);
  if (!var)
    return nullptr;
  llvm::Value *val = ir_builder->CreateLoad(var->ty_->GetLLVMType(), var->val_);
  return new tr::ValAndTy(val, var->ty_->ActualTy());
}
// FINISHED
tr::ValAndTy *NilExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level,
                                err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  // 返回空指针值和 nil 类型
  return new tr::ValAndTy(nullptr, type::NilTy::Instance());
}
// FINISHED
tr::ValAndTy *IntExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level,
                                err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  llvm::Value *value = ir_builder->getInt32(this->val_);
  return new tr::ValAndTy(value, type::IntTy::Instance());
}
// FINISHED
tr::ValAndTy *StringExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                   tr::Level *level,
                                   err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  auto value = type::StringTy::CreateGlobalStringStructPtr(this->str_);
  auto type = type::StringTy::Instance();
  return new tr::ValAndTy(value, type);
}
// FINISHED
tr::ValAndTy *CallExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                 tr::Level *currentLevel,
                                 err::ErrorMsg *errormsg) const {
  // 获取函数入口
  env::EnvEntry *envEntry = venv->Look(func_);
  env::FunEntry *functionEntry = dynamic_cast<env::FunEntry *>(envEntry);
  if (!functionEntry) {
    errormsg->Error(pos_, "undefined function");
    return nullptr;
  }

  // 准备参数列表
  std::vector<llvm::Value *> paramValues;
  // 翻译并添加实际参数
  for (Exp *arg : args_->GetList()) {
    tr::ValAndTy *argResult =
        arg->Translate(venv, tenv, currentLevel, errormsg);
    paramValues.push_back(argResult->val_);
  }

  if (functionEntry->level_->parent_) {
    // 如果不是最外层函数，需要传递静态链
    // 为函数调用分配栈空间，包括静态链
    currentLevel->frame_->AllocOutgoSpace(
        (functionEntry->formals_->GetList().size() + 1) *
        reg_manager->WordSize());
    llvm::Value *frameSizeGlobal = ir_builder->CreateLoad(
        ir_builder->getInt64Ty(), currentLevel->frame_->framesize_global);
    llvm::Value *newSP =
        ir_builder->CreateSub(currentLevel->get_sp(), frameSizeGlobal);
    paramValues.insert(paramValues.begin(), newSP);

    if (functionEntry->level_->parent_ == currentLevel) {
      // 如果函数的父级就是当前层级，直接使用当前SP
      paramValues.insert(std::next(paramValues.begin()),
                         currentLevel->get_sp());
    } else {
      // 计算需要追踪的静态链层数
      int cnt = 0;
      tr::Level *tempLevel = currentLevel;
      tr::Level *targetParent = functionEntry->level_->parent_;

      // 计算从当前层级到目标函数父级的层数
      while (tempLevel && tempLevel != targetParent) {
        cnt++;
        tempLevel = tempLevel->parent_;
      }

      // 如果找不到对应的父级，说明层级关系有误
      if (!tempLevel) {
        errormsg->Error(pos_, "invalid static link");
        return nullptr;
      }
      // 根据计算出的层数追踪静态链
      llvm::Value *staticLinkVal = currentLevel->get_sp();
      for (int i = 0; i < cnt; i++) {
        auto slFormal = currentLevel->frame_->Formals()->begin();
        llvm::Value *slAddr = (*slFormal)->ToLLVMVal(staticLinkVal);
        llvm::Value *slPtr = ir_builder->CreateIntToPtr(
            slAddr, ir_builder->getInt64Ty()->getPointerTo());
        staticLinkVal = ir_builder->CreateLoad(ir_builder->getInt64Ty(), slPtr);
      }
      paramValues.insert(std::next(paramValues.begin()), staticLinkVal);
    }
  } else {
    // 如果是最外层函数，只需要分配栈空间
    currentLevel->frame_->AllocOutgoSpace(
        functionEntry->formals_->GetList().size() * reg_manager->WordSize());
  }

  // 创建函数调用
  llvm::Value *callResult =
      ir_builder->CreateCall(functionEntry->func_, paramValues);

  // 如果函数返回值是void，返回0
  if (!functionEntry->result_ ||
      dynamic_cast<type::VoidTy *>(functionEntry->result_)) {
    return new tr::ValAndTy(nullptr, type::VoidTy::Instance());
  }

  // 返回函数调用结果
  return new tr::ValAndTy(callResult, functionEntry->result_);
}

// FINISHED
tr::ValAndTy *OpExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                               tr::Level *level,
                               err::ErrorMsg *errormsg) const {
  auto left = this->left_->Translate(venv, tenv, level, errormsg);
  auto right = this->right_->Translate(venv, tenv, level, errormsg);
  auto left_val = left->val_;
  auto right_val = right->val_;
  auto left_type = left->ty_;
  auto right_type = right->ty_;
  llvm::errs() << "Operation (op): " << this->oper_ << "\n";
  // llvm::errs() << "Left value type: ";
  // left_val->getType()->print(llvm::errs());
  // llvm::errs() << "\n";

  // llvm::errs() << "Right value type: ";
  // right_val->getType()->print(llvm::errs());
  // llvm::errs() << "\n";

  if (left_type == type::NilTy::Instance() ||
      right_type == type::NilTy::Instance()) {
    if (left_type == type::NilTy::Instance() &&
        right_type == type::NilTy::Instance()) {
      if (this->oper_ == EQ_OP) {
        return new tr::ValAndTy(
            llvm::ConstantInt::get(ir_builder->getInt32Ty(), 1),
            type::IntTy::Instance());
      } else if (this->oper_ == NEQ_OP) {
        return new tr::ValAndTy(
            llvm::ConstantInt::get(ir_builder->getInt32Ty(), 0),
            type::IntTy::Instance());
      }
    }
    llvm::Value *res;
    llvm::Value *val;
    if (left_type == type::NilTy::Instance()) {
      val = ir_builder->CreatePtrToInt(right_val, ir_builder->getInt64Ty());
    } else {
      val = ir_builder->CreatePtrToInt(left_val, ir_builder->getInt64Ty());
    }
    if (this->oper_ == EQ_OP) {
      res = ir_builder->CreateICmpEQ(
          val, llvm::ConstantInt::get(ir_builder->getInt64Ty(), 0));
    } else if (this->oper_ == NEQ_OP) {
      res = ir_builder->CreateICmpNE(
          val, llvm::ConstantInt::get(ir_builder->getInt64Ty(), 0));
    }
    llvm::Value *ret = ir_builder->CreateZExt(res, ir_builder->getInt32Ty());
    return new tr::ValAndTy(ret, type::IntTy::Instance());
  }

  llvm::Value *res;
  switch (this->oper_) {
  case EQ_OP:
    if (dynamic_cast<type::StringTy *>(left->ty_) &&
        dynamic_cast<type::StringTy *>(right->ty_)) {

      llvm::Value *result =
          ir_builder->CreateCall(string_equal, {left->val_, right->val_});
      result = ir_builder->CreateICmpEQ(result, ir_builder->getInt1(1));
      llvm::Value *val =
          ir_builder->CreateZExt(result, ir_builder->getInt32Ty());
      return new tr::ValAndTy(val, type::IntTy::Instance());
    } else {
      res = ir_builder->CreateICmpEQ(left_val, right_val);
      break;
    }
  case NEQ_OP:
    if (dynamic_cast<type::StringTy *>(left->ty_) &&
        dynamic_cast<type::StringTy *>(right->ty_)) {
      llvm::Value *result =
          ir_builder->CreateCall(string_equal, {left->val_, right->val_});
      result = ir_builder->CreateICmpEQ(result, ir_builder->getInt1(0));
      llvm::Value *val =
          ir_builder->CreateZExt(result, ir_builder->getInt32Ty());
      return new tr::ValAndTy(val, type::IntTy::Instance());
    } else {
      res = ir_builder->CreateICmpNE(left_val, right_val);
      break;
    }
  case LT_OP:
    res = ir_builder->CreateICmpSLT(left_val, right_val);
    break;
  case LE_OP:
    res = ir_builder->CreateICmpSLE(left_val, right_val);
    break;
  case GT_OP:
    res = ir_builder->CreateICmpSGT(left_val, right_val);
    break;
  case GE_OP:
    res = ir_builder->CreateICmpSGE(left_val, right_val);
    break;
  case PLUS_OP:
    res = ir_builder->CreateAdd(left_val, right_val);
    break;
  case MINUS_OP:
    res = ir_builder->CreateSub(left_val, right_val);
    break;
  case TIMES_OP:
    res = ir_builder->CreateMul(left_val, right_val);
    break;
  case DIVIDE_OP:
    res = ir_builder->CreateSDiv(left_val, right_val);
    break;
  case AND_OP: {
    llvm::Function *curr_func = func_stack.top();
    auto and_left_test_bb = llvm::BasicBlock::Create(
        ir_builder->getContext(), "and_left_test", curr_func);
    auto and_right_test_bb = llvm::BasicBlock::Create(
        ir_builder->getContext(), "and_right_test", curr_func);
    auto and_next_bb = llvm::BasicBlock::Create(ir_builder->getContext(),
                                                "and_next", curr_func);
    ir_builder->CreateBr(and_left_test_bb);
    ir_builder->SetInsertPoint(and_left_test_bb);
    std::cout << "AND" << std::endl;
    auto left_val_int = ir_builder->CreateICmpNE(
        left_val, llvm::ConstantInt::get(left_val->getType(), 0));
    std::cout << "AND1" << std::endl;
    ir_builder->CreateCondBr(left_val_int, and_right_test_bb, and_next_bb);

    ir_builder->SetInsertPoint(and_right_test_bb);
    auto right_val_int = ir_builder->CreateICmpNE(
        right_val, llvm::ConstantInt::get(right_val->getType(), 0));
    std::cout << "AND2" << std::endl;
    ir_builder->CreateBr(and_next_bb);

    ir_builder->SetInsertPoint(and_next_bb);
    llvm::PHINode *and_phi = ir_builder->CreatePHI(ir_builder->getInt1Ty(), 2);
    and_phi->addIncoming(
        llvm::ConstantInt::get(llvm::Type::getInt1Ty(ir_builder->getContext()),
                               0),
        and_left_test_bb);
    and_phi->addIncoming(right_val_int, and_right_test_bb);
    std::cout << "AND3" << std::endl;
    llvm::Value *ret =
        ir_builder->CreateZExt(and_phi, ir_builder->getInt32Ty());

    return new tr::ValAndTy(ret, type::IntTy::Instance());
  }
  case OR_OP: {
    llvm::Function *curr_func = func_stack.top();
    auto left_test_bb = llvm::BasicBlock::Create(ir_builder->getContext(),
                                                 "left_test", curr_func);
    auto right_test_bb = llvm::BasicBlock::Create(ir_builder->getContext(),
                                                  "right_test", curr_func);
    auto next_bb = llvm::BasicBlock::Create(ir_builder->getContext(),
                                            "next_block", curr_func);
    ir_builder->CreateBr(left_test_bb);
    ir_builder->SetInsertPoint(left_test_bb);
    auto left_val_int = ir_builder->CreateICmpNE(
        left_val, llvm::ConstantInt::get(left_val->getType(), 0));
    ir_builder->CreateCondBr(left_val_int, next_bb, right_test_bb);

    ir_builder->SetInsertPoint(right_test_bb);
    auto right_val_int = ir_builder->CreateICmpNE(
        right_val, llvm::ConstantInt::get(right_val->getType(), 0));
    ir_builder->CreateBr(next_bb);

    ir_builder->SetInsertPoint(next_bb);
    llvm::PHINode *phi = ir_builder->CreatePHI(ir_builder->getInt1Ty(), 2);
    phi->addIncoming(llvm::ConstantInt::get(
                         llvm::Type::getInt1Ty(ir_builder->getContext()), 1),
                     left_test_bb);
    phi->addIncoming(right_val_int, right_test_bb);
    llvm::Value *ret = ir_builder->CreateZExt(phi, ir_builder->getInt32Ty());

    return new tr::ValAndTy(ret, type::IntTy::Instance());
  }
  default:
    assert(0);
  }
  llvm::Value *val = ir_builder->CreateZExt(res, ir_builder->getInt32Ty());
  return new tr::ValAndTy(val, type::IntTy::Instance());
}

// FINISHED
tr::ValAndTy *RecordExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                   tr::Level *level,
                                   err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  // 获取记录类型
  auto actual_ty = tenv->Look(typ_)->ActualTy();
  type::RecordTy *record_ty = static_cast<type::RecordTy *>(actual_ty);

  // 计算需要分配的内存大小
  size_t total_fields = record_ty->fields_->GetList().size();
  size_t memory_size = total_fields * reg_manager->WordSize();

  // 分配内存
  llvm::Value *malloc_size = llvm::ConstantInt::get(
      llvm::Type::getInt32Ty(ir_builder->getContext()), memory_size);
  llvm::Value *raw_ptr = ir_builder->CreateCall(alloc_record, {malloc_size});

  // 转换为正确的指针类型
  llvm::Value *typed_ptr =
      ir_builder->CreateIntToPtr(raw_ptr, record_ty->GetLLVMType());

  // 存储字段值
  size_t field_idx = 0;
  for (EField *field : fields_->GetList()) {
    // 翻译字段表达式
    tr::ValAndTy *field_result =
        field->exp_->Translate(venv, tenv, level, errormsg);

    // 计算字段地址
    std::vector<llvm::Value *> indices = {
        llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_builder->getContext()),
                               0),
        llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_builder->getContext()),
                               field_idx)};

    // 获取字段指针并存储值
    llvm::Value *field_addr = ir_builder->CreateInBoundsGEP(
        record_ty->GetLLVMType()->getPointerElementType(), typed_ptr, indices);

    ir_builder->CreateStore(field_result->val_, field_addr);
    field_idx++;
  }

  return new tr::ValAndTy(typed_ptr, record_ty);
}

// FINISHED
tr::ValAndTy *SeqExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level,
                                err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  if (!seq_ || seq_->GetList().empty()) {
    return new tr::ValAndTy(
        llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_builder->getContext()),
                               0),
        type::VoidTy::Instance());
  }
  tr::ValAndTy *result = nullptr;
  for (Exp *exp : seq_->GetList()) {
    if (result) {
      delete result;
    }
    result = exp->Translate(venv, tenv, level, errormsg);
  }
  // 只返回最后一个exp的结果
  return result;
}
// FINISHED
tr::ValAndTy *AssignExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                   tr::Level *level,
                                   err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  tr::ValAndTy *var = var_->Translate(venv, tenv, level, errormsg);
  tr::ValAndTy *exp = exp_->Translate(venv, tenv, level, errormsg);
  ir_builder->CreateStore(exp->val_, var->val_);
  return new tr::ValAndTy(nullptr, type::VoidTy::Instance());
}

// FINISHED
tr::ValAndTy *IfExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                               tr::Level *level,
                               err::ErrorMsg *errormsg) const {
  llvm::Function *curr = func_stack.top();
  llvm::BasicBlock *branch_true =
      llvm::BasicBlock::Create(ir_builder->getContext(), "branch.true", curr);
  llvm::BasicBlock *branch_false =
      llvm::BasicBlock::Create(ir_builder->getContext(), "branch.false", curr);
  llvm::BasicBlock *branch_merge =
      llvm::BasicBlock::Create(ir_builder->getContext(), "branch.merge", curr);
  auto cond_result = test_->Translate(venv, tenv, level, errormsg);
  llvm::Value *branch_cond = ir_builder->CreateICmpNE(
      cond_result->val_,
      llvm::ConstantInt::get(cond_result->val_->getType(), 0));
  ir_builder->CreateCondBr(branch_cond, branch_true, branch_false);
  // branch_true
  ir_builder->SetInsertPoint(branch_true);
  auto true_result = then_->Translate(venv, tenv, level, errormsg);
  ir_builder->CreateBr(branch_merge);
  auto true_block = ir_builder->GetInsertBlock();

  // branch_false
  ir_builder->SetInsertPoint(branch_false);
  tr::ValAndTy *false_result =
      elsee_ ? elsee_->Translate(venv, tenv, level, errormsg)
             : new tr::ValAndTy(nullptr, true_result->ty_);
  auto false_block = ir_builder->GetInsertBlock();
  ir_builder->CreateBr(branch_merge);
  ir_builder->SetInsertPoint(branch_merge);

  // 处理两个分支都有返回值的情况
  if (true_result->val_ && false_result->val_) {
    auto merge_node = ir_builder->CreatePHI(true_result->ty_->GetLLVMType(), 2);
    merge_node->addIncoming(true_result->val_, true_block);
    merge_node->addIncoming(false_result->val_, false_block);
    return new tr::ValAndTy(merge_node, false_result->ty_);
  }

  // 处理类型相同但可能存在空值的情况
  if (true_result->ty_->IsSameType(false_result->ty_)) {
    if (!true_result->val_ && !false_result->val_) {
      return new tr::ValAndTy(nullptr, type::VoidTy::Instance());
    }

    auto merge_node = ir_builder->CreatePHI(true_result->ty_->GetLLVMType(), 2);

    // 处理true分支的值
    if (true_result->val_) {
      merge_node->addIncoming(true_result->val_, true_block);
    } else {
      auto null_ptr = llvm::ConstantPointerNull::get(
          llvm::dyn_cast<llvm::PointerType>(false_result->ty_->GetLLVMType()));
      merge_node->addIncoming(null_ptr, true_block);
    }

    // 处理false分支的值
    if (false_result->val_) {
      merge_node->addIncoming(false_result->val_, false_block);
    } else {
      auto null_ptr = llvm::ConstantPointerNull::get(
          llvm::dyn_cast<llvm::PointerType>(true_result->ty_->GetLLVMType()));
      merge_node->addIncoming(null_ptr, false_block);
    }

    return new tr::ValAndTy(merge_node, true_result->ty_);
  }

  // 其他情况返回void类型
  return new tr::ValAndTy(nullptr, type::VoidTy::Instance());
}

// FINISHED
tr::ValAndTy *WhileExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                  tr::Level *level,
                                  err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  // assert(!func_stack.empty());
  if (func_stack.empty()) {
    errormsg->Error(this->pos_, "[WhileExp::Translate] func_stack is empty");
    return nullptr;
  }
  llvm::Function *curr_func = func_stack.top();
  llvm::BasicBlock *test_bb = llvm::BasicBlock::Create(ir_builder->getContext(),
                                                       "while.test", curr_func);
  llvm::BasicBlock *body_bb = llvm::BasicBlock::Create(ir_builder->getContext(),
                                                       "while.body", curr_func);
  llvm::BasicBlock *end_bb = llvm::BasicBlock::Create(ir_builder->getContext(),
                                                      "while.end", curr_func);

  ir_builder->CreateBr(test_bb);
  ir_builder->SetInsertPoint(test_bb);
  auto test = test_->Translate(venv, tenv, level, errormsg);
  llvm::Value *test_cond =
      ir_builder->CreateICmpNE(test->val_, ir_builder->getInt32(0));
  ir_builder->CreateCondBr(test_cond, body_bb, end_bb);
  ir_builder->SetInsertPoint(body_bb);
  // 将循环结束块压入栈中，用于处理break语句
  loop_stack.push(end_bb);
  venv->BeginScope();
  tenv->BeginScope();
  body_->Translate(venv, tenv, level, errormsg);
  venv->EndScope();
  tenv->EndScope();
  loop_stack.pop();
  ir_builder->CreateBr(test_bb);
  ir_builder->SetInsertPoint(end_bb);
  return new tr::ValAndTy(nullptr, type::VoidTy::Instance());
}
// FINISHED
tr::ValAndTy *ForExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level,
                                err::ErrorMsg *errormsg) const {
  assert(!func_stack.empty());
  llvm::Function *curr_func = func_stack.top();
  llvm::BasicBlock *body_bb =
      llvm::BasicBlock::Create(ir_builder->getContext(), "for.body", curr_func);
  llvm::BasicBlock *incre_bb = llvm::BasicBlock::Create(
      ir_builder->getContext(), "for.incre", curr_func);
  llvm::BasicBlock *end_bb =
      llvm::BasicBlock::Create(ir_builder->getContext(), "for.end", curr_func);

  // 添加作用域管理
  venv->BeginScope();
  tenv->BeginScope();

  // 正确处理循环变量
  tr::Access *tr_access = tr::Access::AllocLocal(level, escape_);
  env::VarEntry *var_entry =
      new env::VarEntry(tr_access, type::IntTy::Instance(), true);
  venv->Enter(var_, var_entry);

  // 获取变量地址
  llvm::Value *i_ptr = tr_access->access_->ToLLVMVal(level->get_sp());
  i_ptr = ir_builder->CreateIntToPtr(
      i_ptr, type::IntTy::Instance()->GetLLVMType()->getPointerTo());

  loop_stack.push(end_bb);

  tr::ValAndTy *lo_val = lo_->Translate(venv, tenv, level, errormsg);
  tr::ValAndTy *hi_val = hi_->Translate(venv, tenv, level, errormsg);

  // 初始化循环变量
  ir_builder->CreateStore(lo_val->val_, i_ptr);
  ir_builder->CreateBr(body_bb);

  // 循环体
  ir_builder->SetInsertPoint(body_bb);
  body_->Translate(venv, tenv, level, errormsg);
  ir_builder->CreateBr(incre_bb);

  // 增量部分
  ir_builder->SetInsertPoint(incre_bb);
  llvm::Value *curr_i = ir_builder->CreateLoad(
      llvm::Type::getInt32Ty(ir_builder->getContext()), i_ptr);
  llvm::Value *next_i = ir_builder->CreateAdd(
      curr_i, llvm::ConstantInt::get(
                  llvm::Type::getInt32Ty(ir_builder->getContext()), 1));
  ir_builder->CreateStore(next_i, i_ptr);
  llvm::Value *cond = ir_builder->CreateICmpSLE(next_i, hi_val->val_);
  ir_builder->CreateCondBr(cond, body_bb, end_bb);

  // 结束部分
  ir_builder->SetInsertPoint(end_bb);
  loop_stack.pop();

  // 结束作用域
  venv->EndScope();
  tenv->EndScope();

  // 返回void类型
  return new tr::ValAndTy(nullptr, type::VoidTy::Instance());
}

// FINISHED
tr::ValAndTy *BreakExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                  tr::Level *level,
                                  err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  // loop_stack不应该是空的
  if (loop_stack.empty()) {
    errormsg->Error(pos_, "break is not inside any loop");
    return nullptr;
  }
  ir_builder->CreateBr(loop_stack.top());
  loop_stack.pop();
  // break 语句的结果值是 0
  return new tr::ValAndTy(nullptr, type::VoidTy::Instance());
}
// FINISHED
tr::ValAndTy *LetExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level,
                                err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  venv->BeginScope();
  tenv->BeginScope();
  if (decs_) {
    for (Dec *dec : decs_->GetList()) {
      dec->Translate(venv, tenv, level, errormsg);
    }
  }
  auto body_res = body_->Translate(venv, tenv, level, errormsg);
  venv->EndScope();
  tenv->EndScope();
  return body_res;
}
// FINISHED
tr::ValAndTy *ArrayExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                  tr::Level *level,
                                  err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  tr::ValAndTy *size_res = this->size_->Translate(venv, tenv, level, errormsg);
  tr::ValAndTy *init_res = this->init_->Translate(venv, tenv, level, errormsg);
  auto init_val = init_res->val_;
  if (init_val->getType() != ir_builder->getInt64Ty()) {
    init_val = ir_builder->CreateSExt(init_val, ir_builder->getInt64Ty());
  }
  type::Ty *array_type = tenv->Look(this->typ_);
  type::ArrayTy *actual_array_type =
      static_cast<type::ArrayTy *>(array_type->ActualTy());
  llvm::Type *element_llvm_type = actual_array_type->ty_->GetLLVMType();
  auto array = ir_builder->CreateCall(init_array, {size_res->val_, init_val});
  auto array_ptr = ir_builder->CreateIntToPtr(array, array_type->GetLLVMType());
  return new tr::ValAndTy(array_ptr, array_type->ActualTy());
}
// FINISHED
tr::ValAndTy *VoidExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                 tr::Level *level,
                                 err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  return new tr::ValAndTy(nullptr, type::VoidTy::Instance());
}

} // namespace absyn