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
#define K 15
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
  RegAllocator(std::string function_name,
               std::unique_ptr<cg::AssemInstr> assem_instr);
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
  std::unique_ptr<Result> TransferResult();
  void cleanup();

private:
  std::string _function_name;
  std::unique_ptr<cg::AssemInstr> assem_instr_;
  live::LiveGraphFactory *liveGraphFactory;
  std::unique_ptr<Result> result_;
  live::INodeListPtr *initial;
  live::INodeListPtr simplifyWorklist;
  live::INodeListPtr freezeWorklist;
  live::INodeListPtr spillWorklist;
  live::MoveList *worklistMoves;
  live::MoveList *activeMoves;
  live::MoveList *coalescedMoves;
  live::MoveList *constrainedMoves;
  live::MoveList *frozenMoves;
  live::INodeListPtr spilledNodes;
  live::INodeListPtr coloredNodes;
  live::INodeListPtr precoloredNodes;
  live::INodeListPtr coalescedNodes;
  live::INodeListPtr selectStack;
  std::map<live::INodePtr, temp::Temp *> color;
  std::map<live::INodePtr, live::MoveList *> _moveList;
  std::map<live::INodePtr, int> degree;
  std::map<live::INodePtr, live::INodePtr> alias;
};

} // namespace ra

#endif