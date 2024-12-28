#include "tiger/liveness/flowgraph.h"
#include <iostream>
namespace fg {

void FlowGraphFactory::AssemFlowGraph() {
  auto list = this->instr_list_->GetList();
  FNodePtr prev = nullptr;
  std::vector<FNodePtr> jump_nodes;
  for (auto instr : list) {
    auto current_node = this->flowgraph_->NewNode(instr);
    if (prev) {
      this->flowgraph_->AddEdge(prev, current_node);
    }
    prev = current_node;
    if (typeid(*instr) == typeid(assem::LabelInstr)) {
      auto label_instr = static_cast<assem::LabelInstr *>(instr);
      this->label_map_->insert(
          std::make_pair(label_instr->label_->Name(), current_node));
    }
    if (typeid(*instr) == typeid(assem::OperInstr)) {
      auto oper_instr = static_cast<assem::OperInstr *>(instr);
      // 查询是不是jump相关的指令
      if (oper_instr->jumps_) {
        jump_nodes.push_back(current_node);
        if (oper_instr->assem_.find("jmp") != std::string::npos) {
          // 如果是jmp，那么和下一个节点是不会有链接的
          prev = nullptr;
        }
      }
    }
  }
  // 第二步，处理和jump相关的边
  for (auto instr_node : jump_nodes) {
    auto oper_instr = static_cast<assem::OperInstr *>(instr_node->NodeInfo());
    for (temp::Label *label : *(oper_instr->jumps_->labels_)) {
      // 查找目标 label 对应的节点
      // auto it = label_map_node->find(label);
      auto it = this->label_map_->find(label->Name());
      if (it != label_map_->end()) {
        // 找到目标节点，添加边
        FNodePtr target_node = it->second;
        flowgraph_->AddEdge(instr_node, target_node);
      } else {
        // 处理 label 不存在的情况
        std::cerr << "Error: Label not found in label_map_node: "
                  << label->Name() << "\n";
      }
    }
  }
}

} // namespace fg

namespace assem {

temp::TempList *LabelInstr::Def() const { return new temp::TempList(); }

temp::TempList *MoveInstr::Def() const { return dst_; }

temp::TempList *OperInstr::Def() const { return dst_; }

temp::TempList *LabelInstr::Use() const { return new temp::TempList(); }

temp::TempList *MoveInstr::Use() const {
  return (src_) ? src_ : new temp::TempList();
}

temp::TempList *OperInstr::Use() const {
  return (src_) ? src_ : new temp::TempList();
}
} // namespace assem
