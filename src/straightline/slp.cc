#include "straightline/slp.h"

#include <iostream>

namespace A {
int A::CompoundStm::MaxArgs() const {
  // TODO: put your code here (lab1).
  return (std::max(stm1->MaxArgs(), stm2->MaxArgs()));
}

Table *A::CompoundStm::Interp(Table *t) const {
  // TODO: put your code here (lab1).
  Table *t1 = stm1->Interp(t);
  Table *t2 = stm2->Interp(t1);
  return t2;
}

int A::AssignStm::MaxArgs() const {
  // TODO: put your code here (lab1).
  return (exp->MaxArgs());
}

Table *A::AssignStm::Interp(Table *t) const {
  // TODO: put your code here (lab1).
  IntAndTable *int_and_table = exp->Interp(t);
  return int_and_table->t->Update(id, int_and_table->i);
}

int A::PrintStm::MaxArgs() const {
  // TODO: put your code here (lab1).
  return (std::max(exps->NumExps(), exps->MaxArgs()));
}

Table *A::PrintStm::Interp(Table *t) const {
  // TODO: put your code here (lab1).
  return this->exps->printExpList(t);
}

int A::IdExp::MaxArgs() const { return 0; }

IntAndTable *A::IdExp::Interp(Table *t) const {
  int number = t->Lookup(this->id);
  return new IntAndTable(number,t);
}

int A::NumExp::MaxArgs() const { return 0; }

IntAndTable *A::NumExp::Interp(Table *t) const {
  return new IntAndTable(num, t);
}

int A::OpExp::MaxArgs() const {
  return std::max(left->MaxArgs(), right->MaxArgs());
}

IntAndTable *A::OpExp::Interp(Table *t) const {
  Table *t1 = left->Interp(t)->t;
  int leftInt = left->Interp(t)->i;
  Table *t2 = right->Interp(t1)->t;
  int rightInt = right->Interp(t1)->i;
  int resultInt = 0;
  switch (oper) {
  case PLUS:
    resultInt = leftInt + rightInt;
    break;
  case MINUS:
    resultInt = leftInt - rightInt;
    break;
  case TIMES:
    resultInt = leftInt * rightInt;
    break;
  case DIV:
    resultInt = leftInt / rightInt;
    break;
  }
  return new IntAndTable(resultInt, t2);
}

int A::EseqExp::MaxArgs() const {
  return std::max(stm->MaxArgs(), exp->MaxArgs());
}

IntAndTable *A::EseqExp::Interp(Table *t) const {
  Table *t1 = stm->Interp(t);
  return exp->Interp(t1);
}

int A::PairExpList::MaxArgs() const {
  return (std::max(exp->MaxArgs(), tail->MaxArgs()));
}

int A::PairExpList::NumExps() const { return (tail->NumExps() + 1); }

int A::LastExpList::MaxArgs() const { return exp->MaxArgs(); }

int A::LastExpList::NumExps() const { return 1; }

Table *A::LastExpList::printExpList(Table *t) const {
  IntAndTable *int_and_table = this->exp->Interp(t);
  std::cout << int_and_table->i << std::endl;
  return (int_and_table->t);
}

Table *A::PairExpList::printExpList(Table *t) const {
  IntAndTable *int_and_table = this->exp->Interp(t);
  std::cout<<int_and_table->i<<" ";
  return(this->tail->printExpList(int_and_table->t));
}

IntAndTable *A::PairExpList::Interp(Table *) const {
  return NULL;
}

IntAndTable *A::LastExpList::Interp(Table *) const {
  return NULL;
}
int Table::Lookup(const std::string &key) const {
  if (id == key) {
    return value;
  } else if (tail != nullptr) {
    return tail->Lookup(key);
  } else {
    assert(false);
  }
}

Table *Table::Update(const std::string &key, int val) const {
  return new Table(key, val, this);
}
} // namespace A
