#include "tiger/regalloc/regalloc.h"

#include "tiger/frame/temp.h"
#include "tiger/liveness/liveness.h"
#include "tiger/output/logger.h"
#include <iostream>

extern frame::RegManager *reg_manager;
extern std::map<std::string, std::pair<int, int>> frame_info_map;

namespace ra {
/* TODO: Put your lab6 code here */
RegAllocator::RegAllocator(std::string function_name,
                           std::unique_ptr<cg::AssemInstr> assem_instr) {
  this->_function_name = function_name;
  this->assem_instr_ = std::move(assem_instr);
  this->result_ = std::make_unique<Result>();
}
void RegAllocator::cleanup() {
  delete liveGraphFactory;
  delete simplifyWorklist;
  delete freezeWorklist;
  delete spillWorklist;
  delete activeMoves;
  delete coalescedMoves;
  delete constrainedMoves;
  delete frozenMoves;
  delete spilledNodes;
  delete coloredNodes;
  delete precoloredNodes;
  delete coalescedNodes;
  delete selectStack;
  for (auto &[_, moveList] : _moveList) {
    delete moveList;
  }
  liveGraphFactory = nullptr;
  simplifyWorklist = nullptr;
  freezeWorklist = nullptr;
  spillWorklist = nullptr;
  activeMoves = nullptr;
  coalescedMoves = nullptr;
  constrainedMoves = nullptr;
  frozenMoves = nullptr;
  spilledNodes = nullptr;
  coloredNodes = nullptr;
  precoloredNodes = nullptr;
  coalescedNodes = nullptr;
  selectStack = nullptr;
  _moveList.clear();
}
RegAllocator::~RegAllocator() { this->cleanup(); }

/**
 * @brief 将 RegAllocator 对象内部存储的 Result 对象的所有权转移给调用者。
 *
 * @return std::unique_ptr<Result>
 */
std::unique_ptr<Result> RegAllocator::TransferResult() {
  return std::move(result_);
}
void RegAllocator::Reset() { this->cleanup(); }
/**
 * @brief
    procedure Main()
        LivenessAnalysis()    // 活跃性分析
        Build()              // 构建干涉图
        MakeWorklist()       // 创建工作列表
        repeat
            if simplifyWorklist ≠ {} then
                Simplify()   // 简化
            else if worklistMoves ≠ {} then
                Coalesce()   // 合并
            else if freezeWorklist ≠ {} then
                Freeze()     // 冻结
            else if spillWorklist ≠ {} then
                SelectSpill() // 选择溢出
        until simplifyWorklist = {} ∧ worklistMoves = {}∧ freezeWorklist = {} ∧
 spillWorklist = {} AssignColors() if spilledNodes ≠ {} then
            RewriteProgram(spilledNodes)
            Main()

 */
void RegAllocator::reg_main() {
  this->LivenessAnalysis();
  this->Build();
  this->MakeWorklist();
  while (true) {
    if (this->simplifyWorklist->GetList().empty() &&
        this->worklistMoves->GetList().empty() &&
        this->freezeWorklist->GetList().empty() &&
        this->spillWorklist->GetList().empty()) {
      break;
    }
    if (!this->simplifyWorklist->GetList().empty()) {
      this->Simplify();
    } else if (!this->worklistMoves->GetList().empty()) {
      this->Coalesce();
    } else if (!this->freezeWorklist->GetList().empty()) {
      this->Freeze();
    } else if (!this->spillWorklist->GetList().empty()) {
      this->SelectSpill();
    }
  }
  this->AssignColors();
  if (spilledNodes->GetList().empty()) {
    // 说明一切正常
    // 删除重复的move
    std::vector<assem::Instr *> _delete;
    auto temp_map = liveGraphFactory->GetTempNodeMap();
    for (auto instr : assem_instr_->GetInstrList()->GetList()) {
      if (auto move_instr = dynamic_cast<assem::MoveInstr *>(instr)) {
        if (move_instr->src_->GetList().size() == 1 &&
            move_instr->dst_->GetList().size() == 1) {
          auto src_temp = move_instr->src_->GetList().front();
          auto dst_temp = move_instr->dst_->GetList().front();
          if (src_temp ==
                  reg_manager->GetRegister(frame::X64RegManager::Reg::RSP) ||
              dst_temp ==
                  reg_manager->GetRegister(frame::X64RegManager::Reg::RSP))
            continue;
          auto src_node = temp_map->Look(src_temp);
          auto dst_node = temp_map->Look(dst_temp);
          if (color[src_node] == color[dst_node]) {
            _delete.push_back(move_instr);
          }
        }
      }
    }

    for (auto _instr : _delete) {
      assem_instr_->GetInstrList()->Remove(_instr);
    }
    // 分配寄存器
    auto result = temp::Map::Empty();
    for (const auto &node : this->liveGraphFactory->GetLiveGraph()
                                .interf_graph->Nodes()
                                ->GetList()) {
      auto reg = reg_manager->temp_map_->Look(color[node]);
      if (reg) {
        result->Enter(node->NodeInfo(), reg);
      }
    }
    this->result_->coloring_ = result;
    this->result_->il_ = this->assem_instr_->GetInstrList();
  } else {
    this->RewriteProgram();
    this->_moveList.clear();
    this->reg_main();
  }
}

void RegAllocator::LivenessAnalysis() {
  activeMoves = new live::MoveList();
  coalescedMoves = new live::MoveList();
  constrainedMoves = new live::MoveList();
  frozenMoves = new live::MoveList();
  spilledNodes = new live::INodeList();
  coloredNodes = new live::INodeList();
  precoloredNodes = new live::INodeList();
  coalescedNodes = new live::INodeList();
  selectStack = new live::INodeList();
  simplifyWorklist = new live::INodeList();
  freezeWorklist = new live::INodeList();
  spillWorklist = new live::INodeList();
  auto flow_graph_factory =
      new fg::FlowGraphFactory(assem_instr_->GetInstrList());
  flow_graph_factory->AssemFlowGraph();
  liveGraphFactory =
      new live::LiveGraphFactory(flow_graph_factory->GetFlowGraph());
  liveGraphFactory->Liveness();
  worklistMoves = liveGraphFactory->GetLiveGraph().moves;
}

void RegAllocator::Build() {
  const auto &liveGraph = this->liveGraphFactory->GetLiveGraph();
  auto &interfGraphNodes = liveGraph.interf_graph->Nodes()->GetList();
  auto &moves = liveGraph.moves->GetList();
  const auto &tempNodeMap = this->liveGraphFactory->GetTempNodeMap();
  const auto &registers = reg_manager->Registers()->GetList();

  for (auto node : interfGraphNodes) {
    live::MoveList *moveList = new live::MoveList();
    // 过滤并添加与当前节点相关的移动操作
    for (auto move : moves) {
      if (move.first == node || move.second == node) {
        moveList->Append(move.first, move.second);
      }
    }
    _moveList[node] = moveList;
    degree[node] = node->Degree();
  }
  for (auto reg : registers) {
    auto regNode = tempNodeMap->Look(reg);
    precoloredNodes->Append(regNode);
    color[regNode] = reg;
  }
}
/**
 * @brief
    procedure MakeWorklist()
        forall n ∈ initial
            initial ← initial \ {n}
            if degree[n] ≥ K then
                spillWorklist ← spillWorklist ∪ {n}
            else if MoveRelated(n) then
                freezeWorklist ← freezeWorklist ∪ {n}
            else
                simplifyWorklist ← simplifyWorklist ∪ {n}
 */
void RegAllocator::MakeWorklist() {
  // 将节点分为三类
  // 1.可能需要溢出的变量 (spillWorklist)
  // 2.与移动指令相关的变量 (freezeWorklist)
  // 3.可以直接简化的变量 (simplifyWorklist)
  for (auto _node : this->liveGraphFactory->GetLiveGraph()
                        .interf_graph->Nodes()
                        ->GetList()) {
    if (this->precoloredNodes->Contain(_node)) {
      continue;
    }
    if (degree[_node] >= K) {
      this->spillWorklist->Append(_node);
    } else {
      if (this->MoveRelated(_node)) {
        this->freezeWorklist->Append(_node);
      } else {
        this->simplifyWorklist->Append(_node);
      }
    }
  }
}
/**
 * @brief 获取节点n的邻接节点
    function Adjacent(n)
        adjList[n] \ (selectStack ∪ coalescedNodes)
 */
live::INodeListPtr RegAllocator::Adjacent(live::INodePtr node) {
  return node->Adj()->Diff(selectStack->Union(coalescedNodes));
}
/**
 * @brief
    function NodeMoves(n)
        moveList[n] ∩ (activeMoves ∪ worklistMoves)
 */
live::MoveList *RegAllocator::NodeMoves(live::INodePtr node) {
  return _moveList[node]->Intersect(activeMoves->Union(worklistMoves));
}
/**
 * @brief 判断节点n是否与任何移动指令相关
    function MoveRelated(n)
        NodeMoves(n) ≠ {}
 *
 * @param node
 * @return true
 * @return false
 */
bool RegAllocator::MoveRelated(live::INodePtr node) {
  return !NodeMoves(node)->GetList().empty();
}
/**
 * @brief
    procedure AddEdge(u, v)
        if ((u, v) ∉ adjSet) ∧ (u ≠ v) then
            adjSet ← adjSet ∪ {(u, v), (v, u)}
            if u ∉ precolored then
                adjList[u] ← adjList[u] ∪ {v}
                degree[u] ← degree[u] + 1
            if v ∉ precolored then
                adjList[v] ← adjList[v] ∪ {u}
                degree[v] ← degree[v] + 1
 */
bool RegAllocator::AddEdge(live::INodePtr u, live::INodePtr v) {
  if (!u || !v) {
    return false;
  }
  if (u == v || u->Succ()->Contain(v)) {
    return false;
  }
  const auto &graph = liveGraphFactory->GetLiveGraph().interf_graph;
  graph->AddEdge(u, v);
  graph->AddEdge(v, u);
  degree[u]++;
  degree[v]++;
  return true;
}
/**
 * @brief
    procedure Simplify()
      let n ∈ simplifyWorklist
      simplifyWorklist ← simplifyWorklist \ {n}
      push(n, selectStack)
      forall m ∈ Adjacent(n)
          DecrementDegree(m)

 */
void RegAllocator::Simplify() {
  if (this->simplifyWorklist->GetList().empty()) {
    return;
  }
  auto _to_delete_node = this->simplifyWorklist->GetList().back();
  this->simplifyWorklist->DeleteNode(_to_delete_node);
  this->selectStack->Prepend(_to_delete_node);
  for (auto adj_node : this->Adjacent(_to_delete_node)->GetList()) {
    this->DecrementDegree(adj_node);
  }
}

/**
 * @brief
    procedure DecrementDegree(m)
      let d = degree[m]
      degree[m] ← d - 1
      if d = K then
        EnableMoves({m} ∪ Adjacent(m))
        spillWorklist ← spillWorklist \ {m}
        if MoveRelated(m) then
          freezeWorklist ← freezeWorklist ∪ {m}
        else
          simplifyWorklist ← simplifyWorklist ∪ {m}
 * @param node
 */
void RegAllocator::DecrementDegree(live::INodePtr m) {
  auto d = degree[m];
  degree[m] = d - 1;
  if (d == K) {
    this->Adjacent(m)->Append(m);
    auto m_and_adj_m = this->Adjacent(m);
    this->EnableMoves(m_and_adj_m);
    this->spillWorklist->DeleteNode(m);
    if (this->MoveRelated(m)) {
      this->freezeWorklist->Append(m);
    } else {
      this->simplifyWorklist->Append(m);
    }
  }
}

/**
 * @brief
    procedure EnableMoves(nodes)
      forall n ∈ nodes
        forall m ∈ NodeMoves(n)
          if m ∈ activeMoves then
            activeMoves ← activeMoves \ {m}
            worklistMoves ← worklistMoves ∪ {m}
 * @param nodes
 */
void RegAllocator::EnableMoves(live::INodeListPtr nodes) {
  for (auto n : nodes->GetList()) {
    for (auto m : NodeMoves(n)->GetList()) {
      if (this->activeMoves->Contain(m.first, m.second)) {
        this->activeMoves->Delete(m.first, m.second);
        this->worklistMoves->Append(m.first, m.second);
      }
    }
  }
}

/**
 * @brief
 尝试合并（或“合并”）两个被标记为“移动相关”的寄存器，从而减少需要的寄存器总数。
 * procedure Coalesce()
      let m(=copy(x,y)) ∈ worklistMoves
      x ← GetAlias(x)
      y ← GetAlias(y)
      if y ∈ precolored then
        let (u, v) = (y, x)
      else
        let (u, v) = (x, y)
      worklistMoves ← worklistMoves \ {m}
      if (u = v) then
        coalescedMoves ← coalescedMoves ∪ {m}
        AddWorkList(u)
      else if v ∈ precolored ∨ (u, v) ∈ adjSet then
        constrainedMoves ← constrainedMoves ∪ {m}
        AddWorkList(u)
        AddWorkList(v)
      else if u ∈ precolored ∧ (∀t ∈ Adjacent(v), OK(t, u))
            ∨ u ∉ precolored ∧ Conservative(Adjacent(u) ∪ Adjacent(v)) then
        coalescedMoves ← coalescedMoves ∪ {m}
        Combine(u,v)
        AddWorkList(u)
      else
        activeMoves ← activeMoves ∪ {m}
 */
void RegAllocator::Coalesce() {
  auto m = this->worklistMoves->GetList().back();
  auto x = this->GetAlias(m.first);
  auto y = this->GetAlias(m.second);
  auto u = x;
  auto v = y;
  if (this->precoloredNodes->Contain(y)) {
    u = y;
    v = x;
  }
  this->worklistMoves->Delete(m.first, m.second);
  if (u == v) {
    this->coalescedMoves->Append(m.first, m.second);
    this->AddWorkList(u);
  } else if (this->precoloredNodes->Contain(v) || u->Succ()->Contain(v)) {
    this->constrainedMoves->Append(m.first, m.second);
    this->AddWorkList(u);
    this->AddWorkList(v);
  } else {
    auto cond_1 = this->precoloredNodes->Contain(u);
    auto adj_v = this->Adjacent(v);
    auto cond_2 = true;
    for (auto _adj : adj_v->GetList()) {
      if (this->OK(_adj, u)) {
        cond_2 = false;
        break;
      }
    }
    auto cond_3 = !this->precoloredNodes->Contain(u);
    auto cond_4 =
        this->Conservative(this->Adjacent(u)->Union(this->Adjacent(v)));
    if (cond_1 && cond_2 || cond_3 && cond_4) {
      this->coalescedMoves->Append(m.first, m.second);
      this->Combine(u, v);
      this->AddWorkList(u);
    } else {
      this->activeMoves->Append(m.first, m.second);
    }
  }
}

/**
 * @brief
 * procedure AddWorkList(u)
    if (u ∉ precolored ∧ not(MoveRelated(u)) ∧ degree[u] < K) then
      freezeWorklist ← freezeWorklist \ {u}
      simplifyWorklist ← simplifyWorklist ∪ {u}
  * @param node
 */
void RegAllocator::AddWorkList(live::INodePtr node) {
  if (!this->precoloredNodes->Contain(node) && !this->MoveRelated(node) &&
      degree[node] < K) {
    this->freezeWorklist->DeleteNode(node);
    this->simplifyWorklist->Append(node);
  }
}

/**
 * @brief
  function OK(t,r)
    degree[t] < K ∨ t ∈ precolored ∨ (t, r) ∈ adjSet
 */
bool RegAllocator::OK(live::INodePtr t, live::INodePtr r) {
  if (degree[t] < K || precoloredNodes->Contain(t) ||
      t->Succ()->Contain(r) && t->Pred()->Contain(r)) {
    return true;
  }
  return false;
}

/**
 * @brief
    function Conservative(nodes)
      let k = 0
      forall n ∈ nodes
        if degree[n] ≥ K then k ← k + 1
      return (k < K)
 */
bool RegAllocator::Conservative(live::INodeListPtr nodes) {
  auto k = 0;
  for (auto node : nodes->GetList()) {
    if (degree[node] >= K) {
      k = k + 1;
    }
  }
  return k < K;
}

/**
 * @brief
    function GetAlias(n)
      if n ∈ coalescedNodes then
        GetAlias(alias[n])
      else
        n
 * @param node
 * @return live::INodePtr
 */
live::INodePtr RegAllocator::GetAlias(live::INodePtr node) {
  return this->coalescedNodes->Contain(node) ? this->GetAlias(alias[node])
                                             : node;
}

/**
 * @brief
    procedure Combine(u,v)
      if v ∈ freezeWorklist then
        freezeWorklist ← freezeWorklist \ {v}
      else
        spillWorklist ← spillWorklist \ {v}
        coalescedNodes ← coalescedNodes ∪ {v}
      alias[v] ← u
      moveList[u] ← moveList[u] ∪ moveList[v]
      EnableMoves(v)
      forall t ∈ Adjacent(v)
        AddEdge(t,u)
        DecrementDegree(t)
      if degree[u] ≥ K ∧ u ∈ freezeWorklist then
        freezeWorklist ← freezeWorklist \ {u}
        spillWorklist ← spillWorklist ∪ {u}
 * @param u
 * @param v
 */
void RegAllocator::Combine(live::INodePtr u, live::INodePtr v) {
  if (this->freezeWorklist->Contain(v)) {
    this->freezeWorklist->DeleteNode(v);
  } else {
    this->spilledNodes->DeleteNode(v);
  }
  this->coalescedNodes->Append(v);
  alias[v] = u;
  _moveList[u] = _moveList[u]->Union(_moveList[v]);
  graph::NodeList<temp::Temp> _v;
  _v.Clear();
  _v.Append(v);
  this->EnableMoves(&_v);
  for (auto t : this->Adjacent(v)->GetList()) {
    this->AddEdge(t, u);
    this->DecrementDegree(t);
  }
  if (degree[u] >= K && this->freezeWorklist->Contain(u)) {
    this->freezeWorklist->DeleteNode(u);
    this->spillWorklist->Append(u);
  }
}
/**
 * @brief
    procedure Freeze()
      let u ∈ freezeWorklist
      freezeWorklist ← freezeWorklist \ {u}
      simplifyWorklist ← simplifyWorklist ∪ {u}
      FreezeMoves(u)
 *
 */
void RegAllocator::Freeze() {
  auto u = this->freezeWorklist->GetList().back();
  this->freezeWorklist->DeleteNode(u);
  this->simplifyWorklist->Append(u);
  FreezeMoves(u);
}

/**
 * @brief
    procedure FreezeMoves(u)
      forall m(=copy(x,y)) ∈ NodeMoves(u)
        if GetAlias(y) = GetAlias(u) then
          v ← GetAlias(x)
        else
          v ← GetAlias(y)
        activeMoves ← activeMoves \ {m}
        frozenMoves ← frozenMoves ∪ {m}
        if NodeMoves(v) = {} ∧ degree[v] < K then
          freezeWorklist ← freezeWorklist \ {v}
          simplifyWorklist ← simplifyWorklist ∪ {v}
 * @param u
 */
void RegAllocator::FreezeMoves(live::INodePtr u) {
  for (auto m : this->NodeMoves(u)->GetList()) {
    auto x = m.first;
    auto y = m.second;
    auto v = this->GetAlias(y);
    if (this->GetAlias(y) == this->GetAlias(u)) {
      v = this->GetAlias(x);
    }
    this->activeMoves->Delete(x, y);
    this->frozenMoves->Append(x, y);
    if (this->NodeMoves(v)->GetList().empty() && degree[v] < K) {
      freezeWorklist->DeleteNode(v);
      simplifyWorklist->Append(v);
    }
  }
}

/**
 * @brief
    procedure SelectSpill()
      let m ∈ spillWorklist  // m 是用所喜好的启发式从这个集合中选择出来的
      // 注意：要避免选择那种由读取前面已溢出的寄存器产生的，活跃范围很小的结点
      spillWorklist ← spillWorklist \ {m}
      simplifyWorklist ← simplifyWorklist ∪ {m}
      FreezeMoves(m)
 *
 */
void RegAllocator::SelectSpill() {
  if (spillWorklist->GetList().empty()) {
    return;
  }
  // 直接选择生命周期最长的节点
  auto spillNodes = spillWorklist->GetList();
  // 使用 lambda 表达式计算节点的生命周期
  auto calculateLife = [this](const auto &node) {
    int start = -1;
    int end = -1;
    int pos = 0;
    for (const auto &instr : assem_instr_->GetInstrList()->GetList()) {
      if (instr->Def()->Contain(node->NodeInfo())) {
        if (start == -1)
          start = pos;
      }
      if (instr->Use()->Contain(node->NodeInfo())) {
        end = pos;
      }
      pos++;
    }

    if (start == -1 || end == -1)
      return 0; // 如果没有定义或使用，返回生命周期为0
    return end - start;
  };
  // 使用 std::max_element 配合 lambda 表达式找出生命周期最长的节点
  auto m = *std::max_element(spillNodes.begin(), spillNodes.end(),
                             [&](const auto &a, const auto &b) {
                               return calculateLife(a) < calculateLife(b);
                             });
  this->spillWorklist->DeleteNode(m);
  this->simplifyWorklist->Append(m);
  FreezeMoves(m);
}

/**
 * @brief
    procedure AssignColors()
      while SelectStack not empty
        let n = pop(SelectStack)
        okColors ← {0, ..., K-1}
        forall w ∈ adjList[n]
          if GetAlias(w) ∈ (coloredNodes ∪ precolored) then
            okColors ← okColors \ {color[GetAlias(w)]}
        if okColors = {} then
          spilledNodes ← spilledNodes ∪ {n}
        else
          coloredNodes ← coloredNodes ∪ {n}
          let c ∈ okColors
          color[n] ← c
        forall n ∈ coalescedNodes
          color[n] ← color[GetAlias(n)]
 *
 */
void RegAllocator::AssignColors() {
  while (!this->selectStack->GetList().empty()) {
    auto n = this->selectStack->GetList().front();
    this->selectStack->DeleteNode(n);
    auto ok_colors = reg_manager->Registers();
    auto union_color = coloredNodes->Union(precoloredNodes);
    for (auto w : n->Adj()->GetList()) {
      if (union_color->Contain(GetAlias(w))) {
        ok_colors->Delete(color[GetAlias(w)]);
      }
    }
    if (ok_colors->GetList().empty()) {
      this->spilledNodes->Append(n);
    } else {
      this->coloredNodes->Append(n);
      color[n] = ok_colors->GetList().front();
    }
  }
  for (auto n : coalescedNodes->GetList()) {
    color[n] = color[GetAlias(n)];
  }
}

/**
 * @brief
    procedure RewriteProgram()
        为每一个v ∈ spilledNodes分配一个存储单元，
        为每一个定值和每一个使用创建一个新的临时变量vi，
        在程序中（指令序列中）vi的每一个定值之后插入一条存储指令，vi的每一个使用之前插入一条取数指令。
        将所有的vi放入集合newTemps。
        spilledNodes ← {}
        initial ← coloredNodes ∪ coalescedNodes ∪ newTemps
        coloredNodes ← {}
        coalescedNodes ← {}

 *
 */
void RegAllocator::RewriteProgram() {

  for (auto _spill_node : this->spilledNodes->GetList()) {
    // 为每一个v ∈ spilledNodes分配一个存储单元
    auto _spill_tmp = temp::TempFactory::NewTemp();
    // 为每一个定值和每一个使用创建一个新的临时变量vi，
    // 在程序中（指令序列中）vi的每一个定值之后插入一条存储指令，vi的每一个使用之前插入一条取数指令。
    // 将所有的vi放入集合newTemps。
    for (auto instr_it = assem_instr_->GetInstrList()->GetList().begin();
         instr_it != assem_instr_->GetInstrList()->GetList().end();
         instr_it++) {
      auto &[x, y] = frame_info_map[_function_name];
      if ((*instr_it)->Use() != nullptr) {
        if ((*instr_it)->Use()->Contain(_spill_node->NodeInfo())) {
          (*instr_it)->Use()->Replace(_spill_node->NodeInfo(), _spill_tmp);
          if (typeid(*(*instr_it)) == typeid(assem::MoveInstr))
            static_cast<assem::MoveInstr *>(*instr_it)->src_ =
                (*instr_it)->Use();
          else if (typeid(*(*instr_it)) == typeid(assem::OperInstr))
            static_cast<assem::OperInstr *>(*instr_it)->src_ =
                (*instr_it)->Use();
          auto assem_str = "movq " + std::string(_function_name) +
                           "_framesize_local" + std::to_string(x) + "(`s0),`d0";
          assem_instr_->GetInstrList()->Insert(
              instr_it,
              new assem::MoveInstr(assem_str, new temp::TempList(_spill_tmp),
                                   new temp::TempList(reg_manager->GetRegister(
                                       frame::X64RegManager::Reg::RSP))));
        }
        if ((*instr_it)->Def() != nullptr &&
            (*instr_it)->Def()->Contain(_spill_node->NodeInfo())) {
          (*instr_it)->Def()->Replace(_spill_node->NodeInfo(), _spill_tmp);
          if (assem::MoveInstr *move_instr =
                  dynamic_cast<assem::MoveInstr *>(*instr_it))
            move_instr->dst_ = (*instr_it)->Def();
          else if (assem::OperInstr *oper_instr =
                       dynamic_cast<assem::OperInstr *>(*instr_it))
            oper_instr->dst_ = (*instr_it)->Def();
          x -= 8;
          y += 8;
          auto assem_str = "movq `s0," + std::string(_function_name) +
                           "_framesize_local" + std::to_string(x) + "(`d0)";
          assem_instr_->GetInstrList()->Insert(
              std::next(instr_it),
              new assem::MoveInstr(assem_str,
                                   new temp::TempList(reg_manager->GetRegister(
                                       frame::X64RegManager::Reg::RSP)),
                                   new temp::TempList(_spill_tmp)));
        }
      }
    }
  }
}

} // namespace ra