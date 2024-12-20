#include "tiger/escape/escape.h"
#include "tiger/absyn/absyn.h"

namespace esc {
void EscFinder::FindEscape() { absyn_tree_->Traverse(env_.get()); }
} // namespace esc

namespace absyn {

void AbsynTree::Traverse(esc::EscEnvPtr env) { this->root_->Traverse(env, 1); }

void SimpleVar::Traverse(esc::EscEnvPtr env, int depth) {
  auto look_res = env->Look(this->sym_);
  if (look_res->depth_ < depth) {
    *look_res->escape_ = true;
  }
}

void FieldVar::Traverse(esc::EscEnvPtr env, int depth) {
  this->var_->Traverse(env, depth);
}

void SubscriptVar::Traverse(esc::EscEnvPtr env, int depth) {
  this->var_->Traverse(env, depth);
  this->subscript_->Traverse(env, depth);
}

void VarExp::Traverse(esc::EscEnvPtr env, int depth) {
  this->var_->Traverse(env, depth);
}

void NilExp::Traverse(esc::EscEnvPtr env, int depth) { return; }

void IntExp::Traverse(esc::EscEnvPtr env, int depth) { return; }

void StringExp::Traverse(esc::EscEnvPtr env, int depth) { return; }

void CallExp::Traverse(esc::EscEnvPtr env, int depth) {
  auto args_list = this->args_->GetList();
  for (auto arg : args_list) {
    arg->Traverse(env, depth);
  }
}

void OpExp::Traverse(esc::EscEnvPtr env, int depth) {
  this->left_->Traverse(env, depth);
  this->right_->Traverse(env, depth);
}

void RecordExp::Traverse(esc::EscEnvPtr env, int depth) {
  auto field_list = this->fields_->GetList();
  for (auto efield : field_list) {
    efield->exp_->Traverse(env, depth);
  }
}

void SeqExp::Traverse(esc::EscEnvPtr env, int depth) {
  auto exp_list = this->seq_->GetList();
  for (auto exp : exp_list) {
    exp->Traverse(env, depth);
  }
}

void AssignExp::Traverse(esc::EscEnvPtr env, int depth) {
  this->var_->Traverse(env, depth);
  this->exp_->Traverse(env, depth);
}

void IfExp::Traverse(esc::EscEnvPtr env, int depth) {
  if (this->test_) {
    this->test_->Traverse(env, depth);
  }
  if (this->then_) {
    this->then_->Traverse(env, depth);
  }
  if (this->elsee_) {
    this->elsee_->Traverse(env, depth);
  }
}

void WhileExp::Traverse(esc::EscEnvPtr env, int depth) {
  if (this->test_) {
    this->test_->Traverse(env, depth);
  }
  if (this->body_) {
    this->body_->Traverse(env, depth);
  }
}

void ForExp::Traverse(esc::EscEnvPtr env, int depth) {
  bool *escape = new bool(false);
  esc::EscapeEntry *new_entry = new esc::EscapeEntry(depth, escape);
  env->Enter(this->var_, new_entry);
  this->lo_->Traverse(env, depth);
  this->hi_->Traverse(env, depth);
  this->body_->Traverse(env, depth);
}

void BreakExp::Traverse(esc::EscEnvPtr env, int depth) { return; }

void LetExp::Traverse(esc::EscEnvPtr env, int depth) {
  auto decs_list = this->decs_->GetList();
  for (auto dec : decs_list) {
    dec->Traverse(env, depth);
  }
  this->body_->Traverse(env, depth);
}

void ArrayExp::Traverse(esc::EscEnvPtr env, int depth) {
  this->size_->Traverse(env, depth);
  this->init_->Traverse(env, depth);
}

void VoidExp::Traverse(esc::EscEnvPtr env, int depth) { return; }

void FunctionDec::Traverse(esc::EscEnvPtr env, int depth) {
  auto fun_list = this->functions_->GetList();
  for (auto function : fun_list) {
    env->BeginScope();
    auto params_list = function->params_->GetList();
    for (auto param : params_list) {
      param->escape_ = false;
      esc::EscapeEntry *new_entry =
          new esc::EscapeEntry(depth + 1, &(param->escape_));
      env->Enter(param->name_, new_entry);
    }
    function->body_->Traverse(env, depth + 1);
    env->EndScope();
  }
}

void VarDec::Traverse(esc::EscEnvPtr env, int depth) {
  bool *escape = new bool(false);
  esc::EscapeEntry *new_entry = new esc::EscapeEntry(depth, escape);
  env->Enter(this->var_, new_entry);
  this->init_->Traverse(env, depth);
}

void TypeDec::Traverse(esc::EscEnvPtr env, int depth) { return; }

} // namespace absyn
