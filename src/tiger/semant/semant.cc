#include "tiger/semant/semant.h"
#include "tiger/absyn/absyn.h"
#include "tiger/env/env.h"
#include "tiger/errormsg/errormsg.h"
#include "tiger/semant/types.h"
#include <list>

namespace absyn {

void AbsynTree::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                           err::ErrorMsg *errormsg) const {
  root_->SemAnalyze(venv, tenv, 0, errormsg);
}

type::Ty *SimpleVar::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                                int labelcount, err::ErrorMsg *errormsg) const {
  env::EnvEntry *entry = venv->Look(sym_);
  if (entry && typeid(*entry) == typeid(env::VarEntry)) {
    return (static_cast<env::VarEntry *>(entry))->ty_->ActualTy();
  } else {
    errormsg->Error(pos_, "undefined variable %s", sym_->Name().data());
  }
  return type::VoidTy::Instance();
}

type::Ty *FieldVar::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                               int labelcount, err::ErrorMsg *errormsg) const {
  type::Ty *var_type =
      var_->SemAnalyze(venv, tenv, labelcount, errormsg)->ActualTy();
  if (typeid(*var_type) != typeid(type::RecordTy)) {
    errormsg->Error(var_->pos_, "not a record type");
    return type::VoidTy::Instance();
  }
  type::FieldList *fieldList = static_cast<type::RecordTy *>(var_type)->fields_;
  for (type::Field *f : fieldList->GetList()) {
    if (f->name_->Name() == sym_->Name()) {
      return f->ty_->ActualTy();
    }
  }
  errormsg->Error(pos_, "field %s doesn't exist", sym_->Name().data());
  return type::VoidTy::Instance();
}

type::Ty *SubscriptVar::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                                   int labelcount,
                                   err::ErrorMsg *errormsg) const {
  type::Ty *subscript_type =
      this->subscript_->SemAnalyze(venv, tenv, labelcount, errormsg)
          ->ActualTy();
  if (typeid(*subscript_type) != typeid(type::IntTy)) {
    errormsg->Error(subscript_->pos_, "ARRAY can only be subscripted by INT.");
    return type::VoidTy::Instance();
  }
  type::Ty *var_type =
      this->var_->SemAnalyze(venv, tenv, labelcount, errormsg)->ActualTy();
  if (typeid(*var_type) != typeid(type::ArrayTy)) {
    errormsg->Error(var_->pos_, "array type required");
    return type::VoidTy::Instance();
  }
  return static_cast<type::ArrayTy *>(var_type)->ty_;
}

type::Ty *VarExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                             int labelcount, err::ErrorMsg *errormsg) const {
  return var_->SemAnalyze(venv, tenv, labelcount, errormsg)->ActualTy();
}

type::Ty *NilExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                             int labelcount, err::ErrorMsg *errormsg) const {
  return type::NilTy::Instance();
}

type::Ty *IntExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                             int labelcount, err::ErrorMsg *errormsg) const {
  return type::IntTy::Instance();
}

type::Ty *StringExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                                int labelcount, err::ErrorMsg *errormsg) const {
  return type::StringTy::Instance();
}

type::Ty *CallExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                              int labelcount, err::ErrorMsg *errormsg) const {
  env::EnvEntry *env_entry = venv->Look(this->func_);
  if (!env_entry) {
    errormsg->Error(pos_, "undefined function %s", func_->Name().data());
    return type::VoidTy::Instance();
  }
  if (typeid(*env_entry) != typeid(env::FunEntry)) {
    errormsg->Error(pos_, "undefined function %s", func_->Name().data());
    return type::VoidTy::Instance();
  }
  env::FunEntry *fun_entry = static_cast<env::FunEntry *>(env_entry);
  const std::list<Exp *> argu_list = this->args_->GetList();
  const std::list<type::Ty *> formal_list = fun_entry->formals_->GetList();
  int min_size = std::min(formal_list.size(), argu_list.size());
  auto argu_ptr = argu_list.begin();
  auto formal_ptr = formal_list.begin();
  for (int i = min_size; i > 0; i--, argu_ptr++, formal_ptr++) {
    type::Ty *tmp_ty =
        (*argu_ptr)->SemAnalyze(venv, tenv, labelcount, errormsg);
    if (!tmp_ty->IsSameType(*formal_ptr)) {
      errormsg->Error(pos_, "para type mismatch");
      return type::VoidTy::Instance();
    }
  }
  if (argu_list.size() > formal_list.size()) {
    errormsg->Error(pos_, "too many params in function %s",
                    func_->Name().c_str());
    return type::IntTy::Instance();
  }
  if (argu_list.size() < formal_list.size()) {
    errormsg->Error(pos_, "too little params in function %s",
                    func_->Name().c_str());
    return type::IntTy::Instance();
  }
  if (!fun_entry->result_)
    return type::VoidTy::Instance();

  return fun_entry->result_->ActualTy();
}

type::Ty *OpExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                            int labelcount, err::ErrorMsg *errormsg) const {
  type::Ty *left_type =
      this->left_->SemAnalyze(venv, tenv, labelcount, errormsg)->ActualTy();
  type::Ty *right_type =
      this->right_->SemAnalyze(venv, tenv, labelcount, errormsg)->ActualTy();
  if (oper_ == absyn::PLUS_OP || oper_ == absyn::MINUS_OP ||
      oper_ == absyn::TIMES_OP || oper_ == absyn::DIVIDE_OP) {
    if (typeid(*left_type) != typeid(type::IntTy)) {
      errormsg->Error(left_->pos_, "integer required");
      return type::IntTy::Instance();
    }
    if (typeid(*right_type) != typeid(type::IntTy)) {
      errormsg->Error(left_->pos_, "integer required");
    }
  } else {
    if (!left_type->IsSameType(right_type)) {
      errormsg->Error(left_->pos_, "same type required");
    }
  }
  return type::IntTy::Instance();
}

type::Ty *RecordExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                                int labelcount, err::ErrorMsg *errormsg) const {
  type::Ty *type = tenv->Look(typ_);
  if (!type) {
    errormsg->Error(pos_, "undefined type %s", typ_->Name().data());
    return type::VoidTy::Instance();
  }
  return type;
}

type::Ty *SeqExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                             int labelcount, err::ErrorMsg *errormsg) const {
  type::Ty *type;
  for (Exp *exp : this->seq_->GetList()) {
    type = exp->SemAnalyze(venv, tenv, labelcount, errormsg);
  }
  return type;
}

type::Ty *AssignExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                                int labelcount, err::ErrorMsg *errormsg) const {
  type::Ty *var_type = this->var_->SemAnalyze(venv, tenv, labelcount, errormsg);
  type::Ty *exp_type = this->exp_->SemAnalyze(venv, tenv, labelcount, errormsg);
  if (!exp_type->IsSameType(var_type)) {
    errormsg->Error(exp_->pos_, "unmatched assign exp");
    return type::VoidTy::Instance();
  }
  if (typeid(*var_) == typeid(SimpleVar)) {
    SimpleVar *simpleVariable = static_cast<SimpleVar *>(var_);
    if (venv->Look(simpleVariable->sym_)->readonly_) {
      errormsg->Error(var_->pos_, "loop variable can't be assigned");
      return type::VoidTy::Instance();
    }
  }
  return type::VoidTy::Instance();
}

type::Ty *IfExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                            int labelcount, err::ErrorMsg *errormsg) const {
  type::Ty *test_type = test_->SemAnalyze(venv, tenv, labelcount, errormsg);
  type::Ty *then_type = then_->SemAnalyze(venv, tenv, labelcount, errormsg);
  type::Ty *elsee_type = nullptr;
  if (elsee_) {
    elsee_type = elsee_->SemAnalyze(venv, tenv, labelcount, errormsg);
    // elsee语句存在，那么then和else的type要一致。
    if (!then_type->IsSameType(elsee_type)) {
      errormsg->Error(elsee_->pos_, "then exp and else exp type mismatch");
      return then_type;
    }
  } else {
    // elsee语句不存在，那么then的type应该是void
    if (typeid(*then_type) != typeid(type::VoidTy)) {
      errormsg->Error(then_->pos_, "if-then exp's body must produce no value");
      return type::VoidTy::Instance();
    }
  }
  return then_type;
}

type::Ty *WhileExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                               int labelcount, err::ErrorMsg *errormsg) const {
  type::Ty *test_type =
      this->test_->SemAnalyze(venv, tenv, labelcount, errormsg);
  if (typeid(*test_type) != typeid(type::IntTy)) {
    errormsg->Error(test_->pos_, "integer required");
    return type::VoidTy::Instance();
  }
  venv->BeginScope();
  tenv->BeginScope();
  type::Ty *body_type =
      this->body_->SemAnalyze(venv, tenv, labelcount, errormsg);
  if (typeid(*body_type) != typeid(type::VoidTy)) {
    errormsg->Error(body_->pos_, "while body must produce no value");
    return type::VoidTy::Instance();
  }
  venv->EndScope();
  tenv->EndScope();
  return type::VoidTy::Instance();
}

type::Ty *ForExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                             int labelcount, err::ErrorMsg *errormsg) const {
  venv->BeginScope();
  tenv->BeginScope();
  venv->Enter(this->var_, new env::VarEntry(type::IntTy::Instance(), true));
  type::Ty *lo_type =
      this->lo_->SemAnalyze(venv, tenv, labelcount, errormsg)->ActualTy();
  type::Ty *hi_type =
      this->hi_->SemAnalyze(venv, tenv, labelcount, errormsg)->ActualTy();

  if (typeid(*lo_type) != typeid(type::IntTy)) {
    errormsg->Error(lo_->pos_, "for exp's range type is not integer");
  }
  if (typeid(*hi_type) != typeid(type::IntTy)) {
    errormsg->Error(hi_->pos_, "for exp's range type is not integer");
  }
  type::Ty *body_type =
      this->body_->SemAnalyze(venv, tenv, labelcount, errormsg)->ActualTy();
  venv->EndScope();
  tenv->EndScope();
  return type::VoidTy::Instance();
}

type::Ty *BreakExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                               int labelcount, err::ErrorMsg *errormsg) const {
  if (labelcount != -1) {
    errormsg->Error(pos_, "break is not inside any loop");
  }
  return type::VoidTy::Instance();
}

type::Ty *LetExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                             int labelcount, err::ErrorMsg *errormsg) const {
  venv->BeginScope();
  tenv->BeginScope();
  for (Dec *dec : decs_->GetList())
    dec->SemAnalyze(venv, tenv, labelcount, errormsg);
  type::Ty *result;
  if (!body_)
    result = type::VoidTy::Instance();
  else
    result = body_->SemAnalyze(venv, tenv, labelcount, errormsg);

  tenv->EndScope();
  venv->EndScope();
  return result;
}

type::Ty *ArrayExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                               int labelcount, err::ErrorMsg *errormsg) const {
  type::Ty *look_ty = tenv->Look(this->typ_)->ActualTy();
  if (!look_ty) {
    errormsg->Error(this->pos_, "wrong at array_exp_1");
    return type::VoidTy::Instance();
  }
  type::Ty *size_type =
      this->size_->SemAnalyze(venv, tenv, labelcount, errormsg)->ActualTy();
  type::Ty *init_type =
      this->init_->SemAnalyze(venv, tenv, labelcount, errormsg)->ActualTy();
  if (typeid(*size_type) != typeid(type::IntTy)) {
    errormsg->Error(this->pos_, "wrong at array_exp_2");
    return type::VoidTy::Instance();
  }
  if (init_type != static_cast<type::ArrayTy *>(look_ty)->ty_->ActualTy()) {
    errormsg->Error(init_->pos_, "type mismatch");
    return type::VoidTy::Instance();
  }
  return look_ty;
}

type::Ty *VoidExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                              int labelcount, err::ErrorMsg *errormsg) const {
  return type::VoidTy::Instance();
}

void FunctionDec::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                             int labelcount, err::ErrorMsg *errormsg) const {
  const auto &fun_dec_list = this->functions_->GetList();
  //检查是否有重复的名字
  std::list<std::string> name_list;
  for (FunDec *fun_dec : fun_dec_list) {
    if (std::find(name_list.begin(), name_list.end(), fun_dec->name_->Name()) !=
        name_list.end()) {
      //存在相同的名称
      errormsg->Error(fun_dec->pos_, "two functions have the same name");
      return;
    }
    name_list.push_back(fun_dec->name_->Name());
    type::TyList *params_list =
        fun_dec->params_->MakeFormalTyList(tenv, errormsg);
    type::Ty *return_type = nullptr;
    if (fun_dec->result_) {
      return_type = tenv->Look(fun_dec->result_);
    }
    venv->Enter(fun_dec->name_, new env::FunEntry(params_list, return_type));
  }

  //检查函数体以及返回类型是否一致
  for (FunDec *fun_dec : fun_dec_list) {
    auto type_list =
        fun_dec->params_->MakeFormalTyList(tenv, errormsg)->GetList();
    auto params_list = fun_dec->params_->GetList();
    type::Ty *return_type = nullptr;
    if (fun_dec->result_) {
      return_type = tenv->Look(fun_dec->result_);
    }
    venv->BeginScope();
    tenv->BeginScope();
    auto paramIt = params_list.begin();
    auto typeIt = type_list.begin();

    for (; paramIt != params_list.end() && typeIt != type_list.end();
         ++paramIt, ++typeIt) {
      venv->Enter((*paramIt)->name_, new env::VarEntry(*typeIt));
    }

    auto body_type =
        fun_dec->body_->SemAnalyze(venv, tenv, labelcount, errormsg);
    if (!fun_dec->result_) {
      if (typeid(*body_type) != typeid(type::VoidTy)) {
        errormsg->Error(fun_dec->body_->pos_, "procedure returns value");
      }
    }

    venv->EndScope();
    tenv->EndScope();
  }
}

void VarDec::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv, int labelcount,
                        err::ErrorMsg *errormsg) const {
  if (typ_ && !tenv->Look(typ_)) {
    errormsg->Error(pos_, "undefined type %s", typ_->Name().data());
    return;
  }
  type::Ty *init_ty = init_->SemAnalyze(venv, tenv, labelcount, errormsg);
  if (typeid(*init_ty) == typeid(type::NilTy) && !this->typ_) {
    errormsg->Error(init_->pos_,
                    "init should not be nil without type specified");
    return;
  }
  if (typeid(*init_ty) == typeid(type::NilTy) &&
      typeid(tenv->Look(this->typ_)->ActualTy()) != typeid(type::RecordTy)) {
    errormsg->Error(init_->pos_,
                    "init should not be nil without type specified");
    return;
  }
  if (typ_ && !(init_ty->IsSameType(tenv->Look(this->typ_)))) {
    errormsg->Error(init_->pos_, "type mismatch");
    return;
  }
  venv->Enter(var_, new env::VarEntry(init_ty));
}

void TypeDec::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv, int labelcount,
                         err::ErrorMsg *errormsg) const {
  const auto &name_and_type_list = this->types_->GetList();
  //检查是否有重复的名字
  std::list<std::string> name_list;
  for (NameAndTy *name_and_ty : name_and_type_list) {
    if (std::find(name_list.begin(), name_list.end(),
                  name_and_ty->name_->Name()) != name_list.end()) {
      //存在相同的名称
      errormsg->Error(name_and_ty->ty_->pos_, "two types have the same name");
      return;
    }
    name_list.push_back(name_and_ty->name_->Name());
    tenv->Enter(name_and_ty->name_,
                new type::NameTy(name_and_ty->name_, nullptr));
  }

  //更新定义
  for (NameAndTy *name_and_ty : name_and_type_list) {
    type::Ty *current_type = tenv->Look(name_and_ty->name_);
    if (!current_type) {
      errormsg->Error(name_and_ty->ty_->pos_, "undefined type %s",
                      name_and_ty->name_->Name().data());
      return;
    }
    type::NameTy *name_type = (type::NameTy *)current_type;
    name_type->ty_ = name_and_ty->ty_->SemAnalyze(tenv, errormsg);
    tenv->Set(name_and_ty->name_, name_type);
  }

  //检查是否存在非法环
  for (auto name_and_ty : this->types_->GetList()) {
    auto current =
        static_cast<type::NameTy *>(tenv->Look(name_and_ty->name_))->ty_;
    while (typeid(*current) == typeid(type::NameTy)) {
      auto name_type = (type::NameTy *)current;
      if (name_type->sym_->Name() == name_and_ty->name_->Name()) {
        errormsg->Error(name_and_ty->ty_->pos_, "illegal type cycle");
        return;
      }
      current = name_type->ty_;
    }
  }
}

type::Ty *NameTy::SemAnalyze(env::TEnvPtr tenv, err::ErrorMsg *errormsg) const {
  return tenv->Look(name_);
}

type::Ty *RecordTy::SemAnalyze(env::TEnvPtr tenv,
                               err::ErrorMsg *errormsg) const {
  return new type::RecordTy(record_->MakeFieldList(tenv, errormsg));
}

type::Ty *ArrayTy::SemAnalyze(env::TEnvPtr tenv,
                              err::ErrorMsg *errormsg) const {
  return new type::ArrayTy(tenv->Look(array_));
}

} // namespace absyn

namespace sem {

void ProgSem::SemAnalyze() {
  FillBaseVEnv();
  FillBaseTEnv();
  absyn_tree_->SemAnalyze(venv_.get(), tenv_.get(), errormsg_.get());
}
} // namespace sem
