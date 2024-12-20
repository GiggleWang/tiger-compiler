#ifndef TIGER_FRAME_FRAME_H_
#define TIGER_FRAME_FRAME_H_

#include <list>
#include <memory>
#include <string>

#include "tiger/codegen/assem.h"
#include "tiger/frame/temp.h"

#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>

namespace frame {
class Frame;
class InFrameAccess;
class RegManager {
public:
  RegManager() : temp_map_(temp::Map::Empty()) {}

  temp::Temp *GetRegister(int regno) { return regs_[regno]; }

  /**
   * 获取除了RSI之外的所有寄存器
   * Get general-purpose registers except RSI
   * NOTE: returned temp list should be in the order of calling convention
   * @return general-purpose registers
   */
  [[nodiscard]] virtual temp::TempList *Registers() = 0;

  /**
   * 获取可以用来存放参数的寄存器
   * Get registers which can be used to hold arguments
   * NOTE: returned temp list must be in the order of calling convention
   * @return argument registers
   */
  [[nodiscard]] virtual temp::TempList *ArgRegs() = 0;

  /**
   * 获取caller-saved寄存器
   * Get caller-saved registers
   * NOTE: returned registers must be in the order of calling convention
   * @return caller-saved registers
   */
  [[nodiscard]] virtual temp::TempList *CallerSaves() = 0;

  /**
   * 获取caller-saved寄存器
   * Get callee-saved registers
   * NOTE: returned registers must be in the order of calling convention
   * @return callee-saved registers
   */
  [[nodiscard]] virtual temp::TempList *CalleeSaves() = 0;

  /**
   * 获取返回接收器寄存器
   * Get return-sink registers
   * @return return-sink registers
   */
  [[nodiscard]] virtual temp::TempList *ReturnSink() = 0;

  /**
   * Get word size
   */
  [[nodiscard]] virtual int WordSize() = 0;

  [[nodiscard]] virtual temp::Temp *FramePointer() = 0;

  temp::Map *temp_map_;

protected:
  std::vector<temp::Temp *> regs_;
};

class Access {
public:
  /* TODO: Put your lab5-part1 code here */
  // 生成访问这个变量的 LLVM IR 代码
  virtual llvm::Value *ToLLVMVal(llvm::Value *frame_addr_ptr) = 0;

  virtual ~Access() = default;
};

class Frame {
public:
  // outgo区域是在当前函数的栈帧中预留的一块空间，专门用于为即将调用的其他函数准备参数。
  int outgo_size_;
  int offset_;
  temp::Label *name_;
  std::list<frame::Access *> *formals_;
  llvm::GlobalVariable *framesize_global;
  llvm::Value *sp;

  Frame(int outgo_size, int offset, temp::Label *name,
        std::list<frame::Access *> *formals)
      : outgo_size_(outgo_size), offset_(offset), name_(name),
        formals_(formals) {}

  // 获取函数标签字符串
  [[nodiscard]] virtual std::string GetLabel() const = 0;
  // 获取函数名标签对象
  [[nodiscard]] virtual temp::Label *Name() const = 0;
  // 获取形参列表
  [[nodiscard]] virtual std::list<frame::Access *> *Formals() const = 0;
  /**
   * @brief 在当前栈帧中为本地变量分配空间
   *
   * @param escape 变量是否需要逃逸处理
   *              - true：变量可能被其他函数访问，需要在内存（栈）中分配
   *              - false：变量仅在当前函数中使用，可以分配在寄存器中
   *
   * @return frame::Access* 返回一个 Access 对象指针，表示如何访问这个变量
   */
  virtual frame::Access *AllocLocal(bool escape) = 0;
  // 分配外部调用参数空间
  virtual void AllocOutgoSpace(int size) = 0;
  /**
   * @brief 计算整个栈帧所需的总空间
   * -offset_：本地变量区域大小
   * outgo_size_：参数传递区域大小
   * +8：返回地址
   * @return int 整个栈帧所需的总空间
   */
  int calculateActualFramesize() { return (-offset_ + outgo_size_) + 8; }
};

/**
 * Fragments
 */

class Frag {
public:
  virtual ~Frag() = default;

  enum OutputPhase {
    Proc,
    String,
    FrameSize,
  };

  /**
   *Generate assembly for main program
   * @param out FILE object for output assembly file
   */
  virtual void OutputAssem(FILE *out, OutputPhase phase,
                           bool need_ra) const = 0;
};

class StringFrag : public Frag {
public:
  llvm::Value *str_val_;
  std::string str_;

  StringFrag(llvm::Value *str_val, std::string str)
      : str_val_(str_val), str_(std::move(str)) {}

  void OutputAssem(FILE *out, OutputPhase phase, bool need_ra) const override;
};

class FrameSizeFrag : public Frag {
public:
  llvm::Value *framesize_val_;
  int framesize_;

  FrameSizeFrag(llvm::Value *framesize_val, int framesize)
      : framesize_val_(framesize_val), framesize_(framesize) {}

  void OutputAssem(FILE *out, OutputPhase phase, bool need_ra) const override;
};

class ProcFrag : public Frag {
public:
  llvm::Function *body_;

  ProcFrag(llvm::Function *body) : body_(body) {}

  void OutputAssem(FILE *out, OutputPhase phase, bool need_ra) const override;
};

class Frags {
public:
  Frags() = default;
  void PushBack(Frag *frag);
  const std::list<Frag *> &GetList() { return frags_; }

private:
  std::list<Frag *> frags_;
};

frame::Frame *NewFrame(temp::Label *name, std::list<bool> formals);

assem::InstrList *ProcEntryExit1(std::string_view fs, assem::InstrList *body);
assem::InstrList *ProcEntryExit2(assem::InstrList *body);
assem::Proc *ProcEntryExit3(std::string_view fs, assem::InstrList *body);

} // namespace frame

#endif