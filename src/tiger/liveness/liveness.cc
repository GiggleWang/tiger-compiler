#include "tiger/liveness/liveness.h"

extern frame::RegManager *reg_manager;

namespace live {

bool MoveList::Contain(INodePtr src, INodePtr dst) {
  return std::any_of(move_list_.cbegin(), move_list_.cend(),
                     [src, dst](std::pair<INodePtr, INodePtr> move) {
                       return move.first == src && move.second == dst;
                     });
}

void MoveList::Delete(INodePtr src, INodePtr dst) {
  assert(src && dst);
  auto move_it = move_list_.begin();
  for (; move_it != move_list_.end(); move_it++) {
    if (move_it->first == src && move_it->second == dst) {
      break;
    }
  }
  move_list_.erase(move_it);
}

MoveList *MoveList::Union(MoveList *list) {
  auto *res = new MoveList();
  for (auto move : move_list_) {
    res->move_list_.push_back(move);
  }
  for (auto move : list->GetList()) {
    if (!res->Contain(move.first, move.second))
      res->move_list_.push_back(move);
  }
  return res;
}

MoveList *MoveList::Intersect(MoveList *list) {
  auto *res = new MoveList();
  for (auto move : list->GetList()) {
    if (Contain(move.first, move.second))
      res->move_list_.push_back(move);
  }
  return res;
}

void LiveGraphFactory::LiveMap() {
  /* TODO: Put your lab6 code here */
  /*
   *大致思路：
   *  for each n
   *    in[n] ← {}
   *    out[n] ← {}
   *  repeat
   *    for each n
   *        in'[n] ← in[n]
   *        out'[n] ← out[n]
   *        in[n] ← use[n] ∪ (out[n] - def[n])
   *        out[n] ← ⋃ (in[s] for s ∈ succ[n])
   *
   *  until in'[n] = in[n] and out'[n] = out[n] for all n
   */
  // first step
  /*
   *  for each n
   *    in[n] ← {}
   *    out[n] ← {}
   */
  auto node_list = this->flowgraph_->Nodes()->GetList();
  for (auto &node : node_list) {
    this->in_->Enter(node, new temp::TempList());
    this->out_->Enter(node, new temp::TempList());
  }
  // second step
  /**
   *  repeat
   *    for each n
   *        in'[n] ← in[n]
   *        out'[n] ← out[n]
   *        in[n] ← use[n] ∪ (out[n] - def[n])
   *        out[n] ← ⋃ (in[s] for s ∈ succ[n])
   *
   *  until in'[n] = in[n] and out'[n] = out[n] for all n
   */
  bool equal = false;
  while (!equal) {
    equal = true;
    for (auto &node : node_list) {
      temp::TempList *current_in = this->in_->Look(node);
      temp::TempList *current_out = this->out_->Look(node);
      int current_in_size = current_in->GetList().size();
      int current_out_size = current_out->GetList().size();
      auto _use = node->NodeInfo()->Use();
      auto _def = node->NodeInfo()->Def();
      // in[n] ← use[n] ∪ (out[n] - def[n])
      // 在这里就是new_in <- _use U (current_out - _def)
      auto new_in = new temp::TempList();
      // 先计算current_out - _def
      auto out_minus_def = new temp::TempList();

      if (current_out != nullptr) {
        // 如果_def为空，那么直接用current_out
        if (_def == nullptr) {
          for (auto it : current_out->GetList()) {
            out_minus_def->Append(it);
          }
        } else {
          // 对current_out中的每个元素，如果不在_def里面，就放入到out_minus_def中
          for (auto it : current_out->GetList()) {
            bool is_in_def = false;
            // 检查当前元素是否在_def中
            for (auto def_it : _def->GetList()) {
              if (it == def_it) {
                is_in_def = true;
                break;
              }
            }
            // 如果不在_def中，则添加到out_minus_def
            if (!is_in_def) {
              out_minus_def->Append(it);
            }
          }
        }
      }

      // new_in <- out_minus_def U _use
      for (auto it : out_minus_def->GetList()) {
        new_in->Append(it);
      }
      for (auto it : _use->GetList()) {
        bool is_in_new_in = false;
        // 检查当前元素是否在_def中
        for (auto new_it : new_in->GetList()) {
          if (it == new_it) {
            is_in_new_in = true;
            break;
          }
        }
        // 如果不在_def中，则添加到out_minus_def
        if (!is_in_new_in) {
          new_in->Append(it);
        }
      }

      this->in_->Set(node, new_in);
      delete current_in;
      // out[n] ← ⋃ (in[s] for s ∈ succ[n])
      auto new_out = new temp::TempList();
      auto succ_list = node->Succ();
      for (auto succ_node : succ_list->GetList()) {
        auto succ_in_list = this->in_->Look(succ_node)->GetList();
        for (auto it : succ_in_list) {
          bool in_out = false;
          for (auto new_it : new_out->GetList()) {
            if (it == new_it) {
              in_out = true;
              break;
            }
          }
          if (!in_out) {
            new_out->Append(it);
          }
        }
      }

      this->out_->Set(node, new_out);
      delete current_out;
      if (new_in->GetList().size() > current_in_size ||
          new_out->GetList().size() > current_out_size) {
        equal = false;
      }
    }
  }
}

void LiveGraphFactory::InterfGraph() {
  /* TODO: Put your lab6 code here */
  // (1) 对于任何对变量 a 定值的非传送指令，以及在该指令处是出口活跃的变量
  // b₁,...,bⱼ，添加冲突边 (a,b₁),...,(a,bⱼ)。
  // (2) 对于传送指令 a←c，如果变量
  // b₁,...,bⱼ 在该指令处是出口活跃的，则对每一个不同于 c 的 b，添加冲突边
  // (a,b₁),...,(a,bⱼ)。

  // 首先，插入所有机器寄存器，并且两两冲突
  auto reg_list = reg_manager->Registers()->GetList();
  for (auto reg : reg_list) {
    if (!this->temp_node_map_->Look(reg)) {
      auto node = this->live_graph_.interf_graph->NewNode(reg);
      this->temp_node_map_->Enter(reg, node);
    }
  }
  for (auto reg1 : reg_list) {
    for (auto reg2 : reg_list) {
      if (reg1 != reg2) {
        auto node1 = this->temp_node_map_->Look(reg1);
        auto node2 = this->temp_node_map_->Look(reg2);
        this->live_graph_.interf_graph->AddEdge(node1, node2);
      }
    }
  }
  // flowgraph_中所有节点的out加入冲突图当节点
  for (auto node : this->flowgraph_->Nodes()->GetList()) {
    for (auto _temp : this->out_->Look(node)->GetList()) {
      if (!this->temp_node_map_->Look(_temp)) {
        auto new_node = this->live_graph_.interf_graph->NewNode(_temp);
        this->temp_node_map_->Enter(_temp, new_node);
      }
    }
  }

  for (auto _node : this->flowgraph_->Nodes()->GetList()) {
    auto instr_info = _node->NodeInfo();
    // (1) 对于任何对变量 a 定值的非传送指令，以及在该指令处是出口活跃的变量
    // b₁,...,bⱼ，添加冲突边 (a,b₁),...,(a,bⱼ)。
    if (typeid(*instr_info) != typeid(assem::MoveInstr)) {
      auto def_list = instr_info->Def()->GetList();
      for (auto def_it : def_list) {
        // 如果在图里面没有点，那么就插入
        if (!this->temp_node_map_->Look(def_it)) {
          this->temp_node_map_->Enter(
              def_it, this->live_graph_.interf_graph->NewNode(def_it));
        }
        if (def_it ==
            reg_manager->GetRegister(frame::X64RegManager::Reg::RSP)) {
          continue;
        }
        // 和该指令处是出口活跃的变量添加冲突边
        for (auto out_it : this->out_->Look(_node)->GetList()) {
          if (out_it ==
              reg_manager->GetRegister(frame::X64RegManager::Reg::RSP)) {
            continue;
          }
          auto node_1 = this->temp_node_map_->Look(def_it);
          auto node_2 = this->temp_node_map_->Look(out_it);
          if (out_it != def_it) {
            this->live_graph_.interf_graph->AddEdge(node_1, node_2);
            this->live_graph_.interf_graph->AddEdge(node_2, node_1);
          }
        }
      }
    } else {
      // (2) 对于传送指令 a←c，如果变量
      // b₁,...,bⱼ 在该指令处是出口活跃的，则对每一个不同于 c 的 b，添加冲突边
      // (a,b₁),...,(a,bⱼ)。
      // first 得到每一个不同于c的b
      auto out_list = this->out_->Look(_node);
      auto diff_out_list = out_list->Diff(_node->NodeInfo()->Use());
      for (auto def_it : _node->NodeInfo()->Def()->GetList()) {
        // 如果在图里面没有点，那么就插入
        if (!this->temp_node_map_->Look(def_it)) {
          this->temp_node_map_->Enter(
              def_it, this->live_graph_.interf_graph->NewNode(def_it));
        }
        if (def_it ==
            reg_manager->GetRegister(frame::X64RegManager::Reg::RSP)) {
          continue;
        }
        // 和该指令处是出口活跃的变量添加冲突边
        for (auto out_it : diff_out_list->GetList()) {
          if (out_it ==
              reg_manager->GetRegister(frame::X64RegManager::Reg::RSP)) {
            continue;
          }
          auto node_1 = this->temp_node_map_->Look(def_it);
          auto node_2 = this->temp_node_map_->Look(out_it);

          this->live_graph_.interf_graph->AddEdge(node_1, node_2);
          this->live_graph_.interf_graph->AddEdge(node_2, node_1);
        }
        // 将move指令的信息添加到live_graph中
        for (auto use_it : _node->NodeInfo()->Use()->GetList()) {
          if (use_it ==
              reg_manager->GetRegister(frame::X64RegManager::Reg::RSP)) {
            continue;
          }
          auto node_use = this->temp_node_map_->Look(use_it);
          auto node_def = this->temp_node_map_->Look(def_it);
          // 如果没有就添加
          if (!this->live_graph_.moves->Contain(node_def, node_use)) {
            this->live_graph_.moves->Append(node_def, node_use);
          }
        }
      }
    }
  }
}

void LiveGraphFactory::Liveness() {
  LiveMap();
  InterfGraph();
}

} // namespace live