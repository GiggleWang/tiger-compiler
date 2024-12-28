#ifndef TIGER_REGALLOC_REGALLOC_H_
#define TIGER_REGALLOC_REGALLOC_H_

#include "tiger/codegen/assem.h"
#include "tiger/codegen/codegen.h"
#include "tiger/frame/frame.h"
#include "tiger/frame/temp.h"
#include "tiger/liveness/liveness.h"
#include "tiger/regalloc/color.h"
#include "tiger/util/graph.h"

namespace ra {

class Result {
public:
  temp::Map *coloring_;
  assem::InstrList *il_;

  Result() : coloring_(nullptr), il_(nullptr) {}
  Result(temp::Map *coloring, assem::InstrList *il)
      : coloring_(coloring), il_(il) {}
  Result(const Result &result) = delete;
  Result(Result &&result) = delete;
  Result &operator=(const Result &result) = delete;
  Result &operator=(Result &&result) = delete;
  // ~Result();
};

class RegAllocator {
  /* TODO: Put your lab6 code here */
  public:
    RegAllocator(std::string function_name, std::unique_ptr<cg::AssemInstr> assem_instr);
    void reg_main();
    ~RegAllocator();
    void LivenessAnalysis();
    void Reset();
    void Build();
    bool AddEdge(live::INodePtr src, live::INodePtr dst);
    void MakeWorklist();
    live::MoveList *NodeMoves(live::INodePtr node);
    live::INodeListPtr Adjacent(live::INodePtr node);
    live::INodePtr GetAlias(live::INodePtr node);
    bool MoveRelated(live::INodePtr node);
    void DecrementDegree(live::INodePtr node);
    void Simplify();
    void EnableMoves(live::INodeListPtr nodes);
    void Coalesce();
    void AddWorkList(live::INodePtr node);
    bool OK(live::INodePtr t, live::INodePtr r);
    bool Conservative(live::INodeListPtr nodes);
    void Combine(live::INodePtr u, live::INodePtr v);
    void Freeze();
    void FreezeMoves(live::INodePtr u);
    void SelectSpill();
    void AssignColors();
    void RewriteProgram();
    void DeleteRepeatMoves();
    std::unique_ptr<Result> TransferResult();
    temp::Map * AssignRegisters();
    void cleanup();
  
  private:
    const int K = 15; // registers函数中只含有15个寄存器，但是rsp也要被分配，只是要确保它和谁都不可能合并
    // frame::Frame *frame_;
    std::string func_name_;
    int offset = 0; // 这个是全局的，因为spill的node在不断增加，所以offset也在不断增加
    std::unique_ptr<cg::AssemInstr> assem_instr_;
    live::LiveGraphFactory* liveGraphFactory;
    // live::LiveGraph live_graph_;
    std::unique_ptr<Result> result_; // 存放分配的结果
    live::INodeListPtr *initial;

    /*
      需要维护的工作表：
        低度数的传送无关节点：simplifyWorklist
        有可能合并的传送指令：worklistMoves
        低度数的传送有关指令：freezeWorklist
        高度数的节点：spillWorklist
    */
    live::INodeListPtr simplifyWorklist;
    live::INodeListPtr freezeWorklist;
    live::INodeListPtr spillWorklist; // 
    live::MoveList * worklistMoves; // 有可能合并的传送指令集合

    // 还未做好合并准备的传送指令集合
    live::MoveList *activeMoves;

    // 已经被合并的传送指令集合
    live::MoveList *coalescedMoves;

    // 被限制的传送指令，即原操作数和目标操作数冲突的传送指令集合
    live::MoveList *constrainedMoves;

    // 已经被冻结的传送指令，即不再考虑合并的传送指令：
    live::MoveList *frozenMoves;

    // 本轮中要被溢出的结点集合，初始为空
    live::INodeListPtr spilledNodes;

    // 被上色成功的节点：
    live::INodeListPtr coloredNodes;

    // 预上色的节点
    live::INodeListPtr precoloredNodes;

    // 被合并的节点：
    live::INodeListPtr coalescedNodes;

    // 一个包含从图中删除的临时变量的栈
    live::INodeListPtr selectStack;

    // 颜色与节点的对应，即算法为节点选择的颜色
    std::map<live::INodePtr, temp::Temp *> color;

    // 节点与它的movelist的映射：
    std::map<live::INodePtr, live::MoveList *> move_list_map;

    // 包含每个节点当前度的数组
    std::map<live::INodePtr, int> degree;

    // 当一条传送指令被合并时，并且v已经放入被合并节点中时，有alias(v) = u
    std::map<live::INodePtr, live::INodePtr> alias;

    // 还有图的冲突边和邻接表表示，这两者都在liveness里完成了初步的
};

} // namespace ra

#endif