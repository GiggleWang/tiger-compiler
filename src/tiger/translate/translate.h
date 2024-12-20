#ifndef TIGER_TRANSLATE_TRANSLATE_H_
#define TIGER_TRANSLATE_TRANSLATE_H_

#include <list>
#include <memory>

#include "tiger/absyn/absyn.h"
#include "tiger/env/env.h"
#include "tiger/errormsg/errormsg.h"
#include "tiger/frame/frame.h"

namespace tr {

class Exp;
class ValAndTy;
class Level;

class Access {
public:
  Level *level_;
  frame::Access *access_;

  Access(Level *level, frame::Access *access)
      : level_(level), access_(access) {}
  static Access *AllocLocal(Level *level, bool escape);
};

class Level {
public:
  frame::Frame *frame_;
  Level *parent_;
  // llvm::Value *sp;

  Level(frame::Frame *frame, Level *parent) : frame_(frame), parent_(parent) {}

  llvm::Value *get_sp() { return frame_->sp; }
  void set_sp(llvm::Value *sp) { frame_->sp = sp; }

  /* TODO: Put your lab5-part1 code here */
  Level *NewLevel(tr::Level *parent, temp::Label *name,
                  std::list<bool> formals) {
    formals.push_front(true);
    frame::Frame *new_frame = frame::NewFrame(name, formals);
    Level *new_level = new Level(new_frame, this);
    return new_level;
  }
};

class ProgTr {
public:
  ProgTr(std::unique_ptr<absyn::AbsynTree> absyn_tree,
         std::unique_ptr<err::ErrorMsg> errormsg)
      : absyn_tree_(std::move(absyn_tree)), errormsg_(std::move(errormsg)),
        main_level_(std::make_unique<Level>(
            frame::NewFrame(temp::LabelFactory::NamedLabel("tigermain"),
                            std::list<bool>()),
            nullptr)),
        tenv_(std::make_unique<env::TEnv>()),
        venv_(std::make_unique<env::VEnv>()) {}

  /**
   * Translate IR tree
   */
  void Translate();

  /**
   * Transfer the ownership of errormsg to outer scope
   * @return unique pointer to errormsg
   */
  std::unique_ptr<err::ErrorMsg> TransferErrormsg() {
    return std::move(errormsg_);
  }

  void OutputIR(std::string_view filename);

private:
  std::unique_ptr<absyn::AbsynTree> absyn_tree_;
  std::unique_ptr<err::ErrorMsg> errormsg_;
  std::unique_ptr<Level> main_level_;
  std::unique_ptr<env::TEnv> tenv_;
  std::unique_ptr<env::VEnv> venv_;

  // Fill base symbol for var env and type env
  void FillBaseVEnv();
  void FillBaseTEnv();
};

} // namespace tr

#endif
