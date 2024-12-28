#include "tiger/regalloc/regalloc.h"

#include "tiger/liveness/liveness.h"
#include "tiger/output/logger.h"
#include <iostream>

extern frame::RegManager *reg_manager;
extern std::map<std::string, std::pair<int, int>> frame_info_map;

namespace ra {
/* TODO: Put your lab6 code here */
RegAllocator::RegAllocator(std::string function_name,
                           std::unique_ptr<cg::AssemInstr> assem_instr) {
  this->func_name_ = function_name;
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
  for (auto &[_, moveList] : move_list_map) {
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
  move_list_map.clear();
}
RegAllocator::~RegAllocator() { this->cleanup(); }
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
    this->DeleteRepeatMoves();
    this->result_->coloring_ = AssignRegisters();
    this->result_->il_ = this->assem_instr_->GetInstrList();
  } else {
    this->RewriteProgram();
    this->move_list_map.clear();
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
    move_list_map[node] = moveList;
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
  return move_list_map[node]->Intersect(activeMoves->Union(worklistMoves));
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
  move_list_map[u] = move_list_map[u]->Union(move_list_map[v]);
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

void RegAllocator::SelectSpill() {
  std::cerr << "select spill start" << std::endl;
  // m是使用所喜欢的启发是从spill中拿出来的，但是随便什么方法都行应该（？）
  auto m = spillWorklist->GetList().front();
  // 最好是选取一个拥有最长生命周期的节点进行取出：
  int max_life = -1;
  int pos = 0, start, end, life;

  for (auto node : spillWorklist->GetList()) {
    pos = 0;
    for (auto instr : assem_instr_->GetInstrList()->GetList()) {
      if (instr->Def()->Contain(node->NodeInfo())) {
        start = pos;
      }
      if (instr->Use()->Contain(node->NodeInfo())) {
        // end = pos;
        life = pos - start;
        if (life > max_life) {
          max_life = life;
          m = node;
        }
      }
      pos++;
    }
  }

  spillWorklist->DeleteNode(m);
  simplifyWorklist->Append(m);
  FreezeMoves(m);
}

void RegAllocator::AssignColors() {
  std::cerr << "assign colors start" << std::endl;
  while (!selectStack->GetList().empty()) {
    // 用这两步模拟pop的操作
    auto node = selectStack->GetList().front();
    selectStack->DeleteNode(node);
    auto ok_colors = reg_manager->Registers();
    // auto rsp = reg_manager->GetRegister(frame::X64RegManager::Reg::RSP);
    // ok_colors->Insert(rsp, 7);

    auto adj_nodes = node->Adj();
    auto union_color = coloredNodes->Union(precoloredNodes);
    for (auto adj_node : adj_nodes->GetList()) {
      if (union_color->Contain(GetAlias(adj_node))) {
        ok_colors->Delete(color[GetAlias(adj_node)]);
      }
    }
    if (ok_colors->GetList().empty()) {
      spilledNodes->Append(node);
    } else {
      coloredNodes->Append(node);
      // 任取一个颜色分配给他
      color[node] = ok_colors->GetList().front();
    }
    delete adj_nodes;
    delete union_color;
  }
  std::cerr << "ready to find alias" << std::endl;
  for (auto node : coalescedNodes->GetList()) {
    color[node] = color[GetAlias(node)];
  }
  std::cerr << "assign colors end" << std::endl;
}

void RegAllocator::RewriteProgram() {
  std::cerr << "rewrite program start" << std::endl;
  // 为每个溢出的vi分配一个存储单元
  // 为每一个定值和每一个使用创建一个新的临时变量
  // 在每一个vi的def指令后加入一个存储指令，在每一个use指令之前插一个load指令
  // 将所有vi放入newTemps，初始化的时候initial是coloredNodes和要被合并节点和newTemp的并集
  auto spilled_nodes = spilledNodes->GetList();
  for (auto node : spilled_nodes) {
    auto new_temp = temp::TempFactory::NewTemp(); // 要替换的新的临时变量
    // auto instr = node->Instr();
    // offset += 8 ; //
    // 每多一个溢出的node，frameszie就要多8个字节，同样那个变量的存储位置也要加8
    // auto access = static_cast<frame::InFrameAccess
    // *>(frame_->AllocLocal(true));

    auto instr_list = assem_instr_->GetInstrList();
    // 这里需要改成迭代器，否则insert函数会出问题
    auto instr_iter = instr_list->GetList().begin();
    auto end = instr_list->GetList().end();
    for (; instr_iter != end; ++instr_iter) {
      auto src = (*instr_iter)->Use();
      auto dst = (*instr_iter)->Def();
      if (src != nullptr && src->Contain(node->NodeInfo())) {
        // 如果是使用，首先要将temp进行更换
        src->Replace(node->NodeInfo(), new_temp);
        // 然后在使用的指令中也进行替换
        if (typeid(*(*instr_iter)) == typeid(assem::MoveInstr)) {
          auto move_instr = static_cast<assem::MoveInstr *>(*instr_iter);
          move_instr->src_ = src;
        } else if (typeid(*(*instr_iter)) == typeid(assem::OperInstr)) {
          auto oper_instr = static_cast<assem::OperInstr *>(*instr_iter);
          oper_instr->src_ = src;
        }
        // 然后要在使用前先使用movq将值load到temp中
        // 此时的地址应该是rsp+framesize+offset
        // 要先更新framesize_local
        // if(frame_info_map[func_name_].first == 0) {
        //     frame_info_map[func_name_].first = -8;
        // }
        // 之前一直搞反了，应该是要如果存在在src中，即要被使用，此时要取出，不需要加offset
        std::string assem;
        if (frame_info_map[func_name_].first == 0) {
          assem = "movq `s0, " + std::string(func_name_) + "_framesize_local-" +
                  std::to_string(frame_info_map[func_name_].first) + ")(%rsp)";
        }
        assem = "movq (" + std::string(func_name_) + "_framesize_local" +
                std::to_string(frame_info_map[func_name_].first) +
                ")(%rsp), `d0";
        instr_list->Insert(instr_iter, new assem::MoveInstr(
                                           assem, new temp::TempList(new_temp),
                                           new temp::TempList()));
      }
      if (dst != nullptr && dst->Contain(node->NodeInfo())) {
        // 如果是定值，首先要将temp进行更换
        dst->Replace(node->NodeInfo(), new_temp);
        // 然后在定值的指令中也进行替换
        if (typeid(*(*instr_iter)) == typeid(assem::MoveInstr)) {
          auto move_instr = static_cast<assem::MoveInstr *>(*instr_iter);
          move_instr->dst_ = dst;
        } else if (typeid(*(*instr_iter)) == typeid(assem::OperInstr)) {
          auto oper_instr = static_cast<assem::OperInstr *>(*instr_iter);
          oper_instr->dst_ = dst;
        }
        // 然后要在定值后先使用movq将值store到temp中，即插在下一条指令
        // 此时的地址应该是rsp+framesize+offset

        // 如果是在dst中，即要被定值，此时要存入，需要加offset

        std::string assem;
        frame_info_map[func_name_].first -= 8;
        frame_info_map[func_name_].second += 8;
        if (frame_info_map[func_name_].first == 0) {
          assem = "movq " + std::string(func_name_) + "_framesize_local-" +
                  std::to_string(frame_info_map[func_name_].first) +
                  ")(%rsp), `d0";
        }
        assem = "movq `s0, (" + std::string(func_name_) + "_framesize_local" +
                std::to_string(frame_info_map[func_name_].first) + ")(%rsp)";
        instr_list->Insert(std::next(instr_iter),
                           new assem::MoveInstr(assem, new temp::TempList(),
                                                new temp::TempList(new_temp)));
      }
    }
  }
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
  // 清空move_list_map
  for (auto iter = move_list_map.begin(); iter != move_list_map.end(); iter++) {
    delete iter->second;
  }
  std::cerr << "rewrite program end" << std::endl;
}

std::unique_ptr<Result> RegAllocator::TransferResult() {
  std::cerr << "transfer result" << std::endl;
  return std::move(result_);
}

temp::Map *RegAllocator::AssignRegisters() { //根据color进行实际的分配
  auto result = temp::Map::Empty();
  auto regs_with_rsp = reg_manager->Registers();
  // auto rsp = reg_manager->GetRegister(frame::X64RegManager::Reg::RSP);
  for (auto node :
       liveGraphFactory->GetLiveGraph().interf_graph->Nodes()->GetList()) {
    result->Enter(node->NodeInfo(), reg_manager->temp_map_->Look(color[node]));
  }
  // regs_with_rsp->Insert(rsp, 7);
  // for(auto pair:color){
  //     result->Enter(pair.first->NodeInfo(),
  //     reg_manager->temp_map_->Look(pair.second));
  // }
  return result;
}

void RegAllocator::DeleteRepeatMoves() {
  // 遍历整个指令集，将所有mov的前后相同的指令全部删除
  std::vector<assem::Instr *> instrs_to_delete;
  auto temp_map = liveGraphFactory->GetTempNodeMap();
  auto instr_iter = assem_instr_->GetInstrList()->GetList().begin();
  auto end = assem_instr_->GetInstrList()->GetList().end();
  auto rsp = reg_manager->GetRegister(frame::X64RegManager::Reg::RSP);
  for (; instr_iter != end; ++instr_iter) {
    if (typeid(*(*instr_iter)) == typeid(assem::MoveInstr)) {
      auto move_instr = static_cast<assem::MoveInstr *>(*instr_iter);
      auto src = move_instr->src_;

      auto dst = move_instr->dst_;
      if (src->GetList().size() == 1 && dst->GetList().size() == 1) {

        auto src_temp = src->GetList().front();
        auto dst_temp = dst->GetList().front();
        if (src_temp == rsp || dst_temp == rsp) {
          continue;
        }
        auto src_node = temp_map->Look(src_temp);
        auto dst_node = temp_map->Look(dst_temp);
        if (color[src_node] == color[dst_node]) {
          instrs_to_delete.push_back(move_instr);
        }
      }
    }
  }

  for (auto instr_to_delete : instrs_to_delete) {
    assem_instr_->GetInstrList()->Remove(instr_to_delete);
  }
}

} // namespace ra