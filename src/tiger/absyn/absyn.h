#ifndef TIGER_ABSYN_ABSYN_H_
#define TIGER_ABSYN_ABSYN_H_

#include <cstdio>
#include <list>
#include <string>

#include "tiger/env/env.h"
#include "tiger/errormsg/errormsg.h"
#include "tiger/semant/types.h"
#include "tiger/symbol/symbol.h"

/**
 * Forward Declarations
 */
namespace tr {
class Exp;
class ExpAndTy;
} // namespace tr

namespace absyn {

class Var;
class Exp;
class Dec;
class Ty;

class ExpList;
class FieldList;
class FunDecList;
class NameAndTyList;
class DecList;
class EFieldList;

enum Oper {
  AND_OP,
  OR_OP,
  PLUS_OP,
  MINUS_OP,
  TIMES_OP,
  DIVIDE_OP,
  EQ_OP,
  NEQ_OP,
  LT_OP,
  LE_OP,
  GT_OP,
  GE_OP,
  ABSYN_OPER_COUNT,
};

/**
 * Abstract syntax tree root
 */
class AbsynTree {
public:
  AbsynTree() = delete;
  // AbsynTree(nullptr_t) = delete;
  explicit AbsynTree(absyn::Exp *root);
  AbsynTree(const AbsynTree &absyn_tree) = delete;
  AbsynTree(AbsynTree &&absyn_tree) = delete;
  AbsynTree &operator=(const AbsynTree &absyn_tree) = delete;
  AbsynTree &operator=(AbsynTree &&absyn_tree) = delete;
  ~AbsynTree();

  void Print(FILE *out) const;
  void SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                  err::ErrorMsg *errormsg) const;

private:
  absyn::Exp *root_;
};

/**
 * Variables
 */

class Var { // SimpleVar,FieldVar,SubscriptVar
public:
  int pos_;
  virtual ~Var() = default;
  virtual void Print(FILE *out, int d) const = 0;
  virtual type::Ty *SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                               int labelcount,
                               err::ErrorMsg *errormsg) const = 0;

protected:
  explicit Var(int pos) : pos_(pos) {}
};

class SimpleVar : public Var { // a
public:
  sym::Symbol *sym_; // a
  SimpleVar(int pos, sym::Symbol *sym) : Var(pos), sym_(sym) {}
  ~SimpleVar() override;

  void Print(FILE *out, int d) const override;
  type::Ty *SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv, int labelcount,
                       err::ErrorMsg *errormsg) const override;
};

class FieldVar : public Var { // person.age
public:
  Var *var_;         // person
  sym::Symbol *sym_; // age

  FieldVar(int pos, Var *var, sym::Symbol *sym)
      : Var(pos), var_(var), sym_(sym) {}
  ~FieldVar() override;

  void Print(FILE *out, int d) const override;
  type::Ty *SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv, int labelcount,
                       err::ErrorMsg *errormsg) const override;
};

class SubscriptVar : public Var { // array[index]
public:
  Var *var_;       // array
  Exp *subscript_; // index

  SubscriptVar(int pos, Var *var, Exp *exp)
      : Var(pos), var_(var), subscript_(exp) {}
  ~SubscriptVar() override;

  void Print(FILE *out, int d) const override;
  type::Ty *SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv, int labelcount,
                       err::ErrorMsg *errormsg) const override;
};

/**
 * Expressions
 */

class Exp {
public:
  int pos_;
  virtual ~Exp() = default;
  virtual void Print(FILE *out, int d) const = 0;
  virtual type::Ty *SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                               int labelcount,
                               err::ErrorMsg *errormsg) const = 0;

protected:
  explicit Exp(int pos) : pos_(pos) {}
};

class VarExp : public Exp { // x , array[index] , person.age
public:
  Var *var_; // x , array[index] , person.age

  VarExp(int pos, Var *var) : Exp(pos), var_(var) {}
  ~VarExp() override;

  void Print(FILE *out, int d) const override;
  type::Ty *SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv, int labelcount,
                       err::ErrorMsg *errormsg) const override;
};

class NilExp : public Exp { //空
public:
  explicit NilExp(int pos) : Exp(pos) {}
  ~NilExp() override;

  void Print(FILE *out, int d) const override;
  type::Ty *SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv, int labelcount,
                       err::ErrorMsg *errormsg) const override;
};

class IntExp : public Exp { // 12345
public:
  int val_; // 12345

  IntExp(int pos, int val) : Exp(pos), val_(val) {}
  ~IntExp() override;

  void Print(FILE *out, int d) const override;
  type::Ty *SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv, int labelcount,
                       err::ErrorMsg *errormsg) const override;
};

class StringExp : public Exp { //"hello,world!"
public:
  std::string str_; //"hello,world!"

  StringExp(int pos, std::string *str) : Exp(pos), str_(*str) {}
  ~StringExp() override;

  void Print(FILE *out, int d) const override;
  type::Ty *SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv, int labelcount,
                       err::ErrorMsg *errormsg) const override;
};

class CallExp : public Exp { // func(x,y)
public:
  sym::Symbol *func_; // func
  ExpList *args_;     // x,y

  CallExp(int pos, sym::Symbol *func, ExpList *args)
      : Exp(pos), func_(func), args_(args) {
    assert(args);
  }
  ~CallExp() override;

  void Print(FILE *out, int d) const override;
  type::Ty *SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv, int labelcount,
                       err::ErrorMsg *errormsg) const override;
};

class OpExp : public Exp { // x+y
public:
  Oper oper_;
  Exp *left_, *right_; // x //y

  OpExp(int pos, Oper oper, Exp *left, Exp *right)
      : Exp(pos), oper_(oper), left_(left), right_(right) {}
  ~OpExp() override;

  void Print(FILE *out, int d) const override;
  type::Ty *SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv, int labelcount,
                       err::ErrorMsg *errormsg) const override;
};

class RecordExp : public Exp { // Person { name = "Alice", age = 30 }
public:
  sym::Symbol *typ_;   // Person
  EFieldList *fields_; // name = "Alice", age = 30

  RecordExp(int pos, sym::Symbol *typ, EFieldList *fields)
      : Exp(pos), typ_(typ), fields_(fields) {}
  ~RecordExp() override;

  void Print(FILE *out, int d) const override;
  type::Ty *SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv, int labelcount,
                       err::ErrorMsg *errormsg) const override;
};

class SeqExp : public Exp { //{ expr1; expr2; expr3 }
public:
  ExpList *seq_; // expr1; expr2; expr3

  SeqExp(int pos, ExpList *seq) : Exp(pos), seq_(seq) {}
  ~SeqExp() override;

  void Print(FILE *out, int d) const override;
  type::Ty *SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv, int labelcount,
                       err::ErrorMsg *errormsg) const override;
};

class AssignExp : public Exp { // x = a + b
public:
  Var *var_; // x
  Exp *exp_; // a + b

  AssignExp(int pos, Var *var, Exp *exp) : Exp(pos), var_(var), exp_(exp) {}
  ~AssignExp() override;

  void Print(FILE *out, int d) const override;
  type::Ty *SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv, int labelcount,
                       err::ErrorMsg *errormsg) const override;
};

class IfExp : public Exp { // if (x > 0) then { y = 1; } else { y = -1; }
public:
  Exp *test_, *then_,
      *elsee_; // test_ : x > 0    // then_ : y = 1   // elsee_ : y = -1

  IfExp(int pos, Exp *test, Exp *then, Exp *elsee)
      : Exp(pos), test_(test), then_(then), elsee_(elsee) {}
  ~IfExp() override;

  void Print(FILE *out, int d) const override;
  type::Ty *SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv, int labelcount,
                       err::ErrorMsg *errormsg) const override;
};

class WhileExp : public Exp { // while (x > 0) do x = x - 1
public:
  Exp *test_, *body_; // test_ : x > 0    // body_ : x = x - 1

  WhileExp(int pos, Exp *test, Exp *body)
      : Exp(pos), test_(test), body_(body) {}
  ~WhileExp() override;

  void Print(FILE *out, int d) const override;
  type::Ty *SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv, int labelcount,
                       err::ErrorMsg *errormsg) const override;
};

class ForExp : public Exp { // for i := 1 to 10 do { sum = sum + i; }
public:
  sym::Symbol *var_;      // i
  Exp *lo_, *hi_, *body_; // 1 10 sum = sum + i;
  bool escape_;

  ForExp(int pos, sym::Symbol *var, Exp *lo, Exp *hi, Exp *body)
      : Exp(pos), var_(var), lo_(lo), hi_(hi), body_(body), escape_(true) {}
  ~ForExp() override;

  void Print(FILE *out, int d) const override;
  type::Ty *SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv, int labelcount,
                       err::ErrorMsg *errormsg) const override;
};

class BreakExp : public Exp {
public:
  explicit BreakExp(int pos) : Exp(pos) {}
  ~BreakExp() override;

  void Print(FILE *out, int d) const override;
  type::Ty *SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv, int labelcount,
                       err::ErrorMsg *errormsg) const override;
};

class LetExp : public Exp { // let var x := 10 in x + 1 end
public:
  DecList *decs_; // var x := 10
  Exp *body_;     // x + 1

  LetExp(int pos, DecList *decs, Exp *body)
      : Exp(pos), decs_(decs), body_(body) {}
  ~LetExp() override;

  void Print(FILE *out, int d) const override;
  type::Ty *SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv, int labelcount,
                       err::ErrorMsg *errormsg) const override;
};

class ArrayExp : public Exp { // int[10] of 0
public:
  sym::Symbol *typ_;  // int
  Exp *size_, *init_; // 10 0

  ArrayExp(int pos, sym::Symbol *typ, Exp *size, Exp *init)
      : Exp(pos), typ_(typ), size_(size), init_(init) {}
  ~ArrayExp() override;

  void Print(FILE *out, int d) const override;
  type::Ty *SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv, int labelcount,
                       err::ErrorMsg *errormsg) const override;
};

class VoidExp : public Exp {
public:
  explicit VoidExp(int pos) : Exp(pos) {}
  ~VoidExp() override;

  void Print(FILE *out, int d) const override;
  type::Ty *SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv, int labelcount,
                       err::ErrorMsg *errormsg) const override;
};

/**
 * Declarations
 */

class Dec { // FunctionDec,VarDec,TypeDec
public:
  int pos_;
  virtual ~Dec() = default;
  virtual void Print(FILE *out, int d) const = 0;
  virtual void SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv, int labelcount,
                          err::ErrorMsg *errormsg) const = 0;

protected:
  explicit Dec(int pos) : pos_(pos) {}
};

class FunctionDec : public Dec {
public:
  FunDecList *functions_;
  // function foo(x: int): int = x + 1
  // function bar(y: string): string = y + "!"
  FunctionDec(int pos, FunDecList *functions)
      : Dec(pos), functions_(functions) {}
  ~FunctionDec() override;

  void Print(FILE *out, int d) const override;
  void SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv, int labelcount,
                  err::ErrorMsg *errormsg) const override;
};

class VarDec : public Dec { // var x: int := 10
public:
  sym::Symbol *var_; // x
  sym::Symbol *typ_; // int
  Exp *init_;        // 10
  bool escape_;      //标记变量是否会逃逸到外层作用域。

  VarDec(int pos, sym::Symbol *var, sym::Symbol *typ, Exp *init)
      : Dec(pos), var_(var), typ_(typ), init_(init), escape_(true) {}
  ~VarDec() override;

  void Print(FILE *out, int d) const override;
  void SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv, int labelcount,
                  err::ErrorMsg *errormsg) const override;
};

class TypeDec : public Dec {
public:
  NameAndTyList *types_;

  TypeDec(int pos, NameAndTyList *types) : Dec(pos), types_(types) {}
  ~TypeDec() override;

  void Print(FILE *out, int d) const override;
  void SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv, int labelcount,
                  err::ErrorMsg *errormsg) const override;
};

/**
 * Types
 */

class Ty {
public:
  int pos_;
  virtual ~Ty() = default;
  virtual void Print(FILE *out, int d) const = 0;
  virtual type::Ty *SemAnalyze(env::TEnvPtr tenv,
                               err::ErrorMsg *errormsg) const = 0;

protected:
  explicit Ty(int pos) : pos_(pos) {}
};

class NameTy : public Ty {
public:
  sym::Symbol *name_;

  NameTy(int pos, sym::Symbol *name) : Ty(pos), name_(name) {}
  ~NameTy() override;

  void Print(FILE *out, int d) const override;
  type::Ty *SemAnalyze(env::TEnvPtr tenv,
                       err::ErrorMsg *errormsg) const override;
};

class RecordTy : public Ty {
public:
  FieldList *record_;

  RecordTy(int pos, FieldList *record) : Ty(pos), record_(record) {}
  ~RecordTy() override;

  void Print(FILE *out, int d) const override;
  type::Ty *SemAnalyze(env::TEnvPtr tenv,
                       err::ErrorMsg *errormsg) const override;
};

class ArrayTy : public Ty {
public:
  sym::Symbol *array_;

  ArrayTy(int pos, sym::Symbol *array) : Ty(pos), array_(array) {}
  ~ArrayTy() override;

  void Print(FILE *out, int d) const override;
  type::Ty *SemAnalyze(env::TEnvPtr tenv,
                       err::ErrorMsg *errormsg) const override;
};

/**
 * Linked lists and nodes of lists
 */

class Field { // name: string
public:
  int pos_;
  sym::Symbol *name_, *typ_; // name string
  bool escape_;

  Field(int pos, sym::Symbol *name, sym::Symbol *typ)
      : pos_(pos), name_(name), typ_(typ), escape_(true) {}

  void Print(FILE *out, int d) const;
};

class FieldList { //{name: string;age : int}
public:
  FieldList() = default;
  explicit FieldList(Field *field) : field_list_({field}) { assert(field); }

  FieldList *Prepend(Field *field) {
    field_list_.push_front(field);
    return this;
  }
  [[nodiscard]] const std::list<Field *> &GetList() const {
    return field_list_;
  }
  void Print(FILE *out, int d) const;
  type::TyList *MakeFormalTyList(env::TEnvPtr tenv,
                                 err::ErrorMsg *errormsg) const;
  type::FieldList *MakeFieldList(env::TEnvPtr tenv,
                                 err::ErrorMsg *errormsg) const;

private:
  std::list<Field *> field_list_;
};

class ExpList {
public:
  ExpList() = default;
  explicit ExpList(Exp *exp) : exp_list_({exp}) { assert(exp); }

  ExpList *Prepend(Exp *exp) {
    exp_list_.push_front(exp);
    return this;
  }
  [[nodiscard]] const std::list<Exp *> &GetList() const { return exp_list_; }
  void Print(FILE *out, int d) const;

private:
  std::list<Exp *> exp_list_;
};

class FunDec { // function foo(x: int, y: string): int = x + 1
public:
  int pos_;
  sym::Symbol *name_;   // foo
  FieldList *params_;   //{x: int, y: string}
  sym::Symbol *result_; // int
  Exp *body_;           // x + 1

  FunDec(int pos, sym::Symbol *name, FieldList *params, sym::Symbol *result,
         Exp *body)
      : pos_(pos), name_(name), params_(params), result_(result), body_(body) {
    assert(params);
  }

  void Print(FILE *out, int d) const;
};

class FunDecList {
  // function foo(x: int): int = x + 1
  // function bar(y: string): string = y + "!"
public:
  explicit FunDecList(FunDec *fun_dec) : fun_dec_list_({fun_dec}) {
    assert(fun_dec);
  }

  FunDecList *Prepend(FunDec *fun_dec) {
    fun_dec_list_.push_front(fun_dec);
    return this;
  }
  [[nodiscard]] const std::list<FunDec *> &GetList() const {
    return fun_dec_list_;
  }
  void Print(FILE *out, int d) const;

private:
  std::list<FunDec *> fun_dec_list_;
};

class DecList {
public:
  DecList() = default;
  explicit DecList(Dec *dec) : dec_list_({dec}) { assert(dec); }

  DecList *Prepend(Dec *dec) {
    dec_list_.push_front(dec);
    return this;
  }
  [[nodiscard]] const std::list<Dec *> &GetList() const { return dec_list_; }
  void Print(FILE *out, int d) const;

private:
  std::list<Dec *> dec_list_;
};

class NameAndTy { // type alias = int
public:
  sym::Symbol *name_; // alias
  Ty *ty_;            // int

  NameAndTy(sym::Symbol *name, Ty *ty) : name_(name), ty_(ty) {}

  void Print(FILE *out, int d) const;
};

class NameAndTyList {
public:
  explicit NameAndTyList(NameAndTy *name_and_ty)
      : name_and_ty_list_({name_and_ty}) {}

  NameAndTyList *Prepend(NameAndTy *name_and_ty) {
    name_and_ty_list_.push_front(name_and_ty);
    return this;
  }
  [[nodiscard]] const std::list<NameAndTy *> &GetList() const {
    return name_and_ty_list_;
  }
  void Print(FILE *out, int d) const;

private:
  std::list<NameAndTy *> name_and_ty_list_;
};

class EField {
public:
  sym::Symbol *name_;
  Exp *exp_;

  EField(sym::Symbol *name, Exp *exp) : name_(name), exp_(exp) {}
  EField(const EField &efield) = delete;
  EField(EField &&efield) = delete;
  EField &operator=(const EField &efield) = delete;
  EField &operator=(EField &&efield) = delete;
  ~EField();

  void Print(FILE *out, int d) const;
};

class EFieldList {
public:
  EFieldList() = default;
  explicit EFieldList(EField *efield) : efield_list_({efield}) {}

  EFieldList *Prepend(EField *efield) {
    efield_list_.push_front(efield);
    return this;
  }
  [[nodiscard]] const std::list<EField *> &GetList() const {
    return efield_list_;
  }
  void Print(FILE *out, int d) const;

private:
  std::list<EField *> efield_list_;
};

}; // namespace absyn

#endif // TIGER_ABSYN_ABSYN_H_
