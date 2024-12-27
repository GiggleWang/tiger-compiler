#include "tiger/liveness/liveness.h"
#include <iostream>

extern frame::RegManager *reg_manager;

#define TYPECHECK(type1, type2) ((typeid(type1)) == (typeid(type2)))

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
  // 求出活跃信息的三个定理：
  /*
    如果一个变量属于use[n]，那么它在节点n是入口活跃的，如果一条语句使用了一个变量，这个变量在语句入口是活跃的
    如果一个变量在节点n是入口活跃的，那它在所有pred[n]中的节点m处都是出口活跃的
    如果一个变量在节点n是出口活跃的，且它不属于def[n]，那么它在节点n是入口活跃的
  */
  /*
   fg::FGraphPtr flowgraph_;
    LiveGraph live_graph_;

    std::unique_ptr<graph::Table<assem::Instr, temp::TempList>> in_;
    std::unique_ptr<graph::Table<assem::Instr, temp::TempList>> out_;
    tab::Table<temp::Temp, INode> *temp_node_map_;
  */
  // 1. 初始化in和out
  // 2. 初始化temp_node_map
  // 3. 初始化in和out的值
  // 4. 迭代计算in和out的值
  auto rsp = reg_manager->GetRegister(frame::X64RegManager::Reg::RSP);
  // 确保rsp在每个节点都是入口活跃且出口活跃的，即一定不会被其他temp使用
  for(auto node : flowgraph_->Nodes()->GetList()) {
    // in_->Enter(node, new temp::TempList(rsp));
    // out_->Enter(node, new temp::TempList(rsp));
    in_->Enter(node, new temp::TempList());
    out_->Enter(node, new temp::TempList());
  }
  // 活跃性是沿控制流箭头的反方向流动的（从out流向in），所以需要从后往前遍历，且每一遍都是先看out再看in
  // 反向进行迭代：
  bool changed = false; // 标记是否是最后一次迭代，要一直迭代直到不会发生改变
  auto iter_node = flowgraph_->Nodes()->GetList().rbegin();
  while(!changed){
    for(iter_node = flowgraph_->Nodes()->GetList().rbegin(); iter_node != flowgraph_->Nodes()->GetList().rend() ; iter_node++) {
      // 计算流程：out[4]->in[4]->out[3]->in[3]->out[2]->in[2]->out[1]->in[1]->out[0]->in[0]
      // 数据流方程中，out为该节点所有后继节点的in的并集，in为use[n]和(out-def[n])的并集
      // 先算出out，用out计算in，用in计算下一个out，以此类推
      // 要构建当前节点的out，需要先找到所有的后继节点，然后将所有后继节点的in的并集作为当前节点的out
      temp::TempList *old_out = out_->Look(*iter_node);
      temp::TempList *old_in = in_->Look(*iter_node);
      
      // 哪个节点的templist初始都含有rsp
      temp::TempList *cur_out = new temp::TempList(rsp);
      for(auto iter_succ = (*iter_node)->Succ()->GetList().begin(); iter_succ != (*iter_node)->Succ()->GetList().end(); iter_succ++) {
        auto succ_in = in_->Look(*iter_succ);
        cur_out = cur_out->Union(succ_in);
      }

      // 这里计算完成了out[4]，现在用它来计算in[4]
      auto cur_out_without_def = cur_out->Diff((*iter_node)->NodeInfo()->Def());
      auto cur_in = cur_out_without_def->Union((*iter_node)->NodeInfo()->Use());

      // 判断是否有变化，有一点变化则就会有下一轮
      if(!cur_out->Equal(old_out) || !cur_in->Equal(old_in)) {
        changed = true;
      }
      in_->Enter(*iter_node, cur_in);
      out_->Enter(*iter_node, cur_out);
      // delete old_out;
      // delete old_in;
    }
    if(!changed) {
      break;
    }
    // 如果到这里了，说明前面的changed是true，此时就要置为false重新进行下一轮迭代
    changed = false;
  }
}

void LiveGraphFactory::InterfGraph() {
  /* TODO: Put your lab6 code here */
  // 根据之前的in 和 out，构建冲突图
  /*
    对于流图中的每个节点，如果这个节点定义了一系列新的临时变量d<-def[n]
    且liveMap中该节点的所有出口活跃的临时变量t<-out[n]，d和t之间存在冲突，要添加冲突边(d,t)，无向边
    但是如果是move指令，此时这个def的方式是通过move指令进行def，假设是d<-m，那对每一个不同于m的t，添加冲突边
  */
  // 首先要加入预定义的机器寄存器节点，他们两两之间一定存在冲突
  auto regs_without_rsp = reg_manager->Registers();
  auto rsp = reg_manager->GetRegister(frame::X64RegManager::Reg::RSP);
  // 冲突图中必须有rsp，因为rsp是一个特殊的寄存器，不会被分配
  // regs_with_rsp->Insert(rsp,7);
  for(auto reg_iter : regs_without_rsp->GetList()) {
    if(!temp_node_map_->Look(reg_iter)) {
      auto reg_node = live_graph_.interf_graph->NewNode(reg_iter);
      temp_node_map_->Enter(reg_iter, reg_node);
      // live_graph_.degree_[reg_node] = 0x7fffffff;
      // live_graph_.precolored_->Append(reg_node);
    }
  }
  // auto rsp_node = live_graph_.interf_graph->NewNode(rsp);
  // temp_node_map_->Enter(rsp, rsp_node);
  // 小心，graph的默认addedge方法添加的是有向边
  // 在所有机器寄存器之间都加上冲突边
  std::cerr<<"add conflict between machine registers start"<<std::endl;
  for(auto reg_iter1 = regs_without_rsp->GetList().begin(); reg_iter1 != regs_without_rsp->GetList().end(); reg_iter1++) {
    // 每次添加的都是双向边
    // 注意这里不能添加reg_iter1的值，所以用next
    for(auto reg_iter2 = std::next(reg_iter1); reg_iter2 != regs_without_rsp->GetList().end(); reg_iter2++) {
      if(reg_iter1 != reg_iter2) {
        live_graph_.interf_graph->AddEdge(temp_node_map_->Look(*reg_iter1), temp_node_map_->Look(*reg_iter2));
        live_graph_.interf_graph->AddEdge(temp_node_map_->Look(*reg_iter2), temp_node_map_->Look(*reg_iter1));
      }
    }
  }
  std::cerr<<"add conflict between machine registers end"<<std::endl;

  // 类似地，先加入节点，再连接边
  // 将每个out中live的临时变量都加入
  auto all_nodes = flowgraph_->Nodes()->GetList();
  for(auto node_iter = all_nodes.begin(); node_iter != all_nodes.end(); node_iter++) {
    auto out_temp_list = out_->Look(*node_iter);
    // auto out_temp_list = (*node_iter)->NodeInfo()->Def() -> Union((*node_iter)->NodeInfo()->Use());
    for(auto temp_iter = out_temp_list->GetList().begin(); temp_iter != out_temp_list->GetList().end(); temp_iter++) {
      if(!temp_node_map_->Look(*temp_iter)) { // 如果没有该节点，就加入
        auto temp_node = live_graph_.interf_graph->NewNode(*temp_iter);
        temp_node_map_->Enter(*temp_iter, temp_node);
        // live_graph_.initial_->Append(temp_node);
      }
    }
  }
// 这个语句只是用来参考live_graph_.moves->Append(temp_node_map_->Look(def),temp_node_map_->Look(use));

  // 构建冲突边
  // auto rsp = reg_manager->GetRegister(frame::X64RegManager::Reg::RSP);
  // 遍历每个节点，找到def和out中的非use部分，为他们添加冲突边
  for(auto node_iter : all_nodes){
    auto instr = node_iter->NodeInfo();
    auto live_out = out_->Look(node_iter);

    // 因为move指令最特殊，连接的是虚线而不是冲突线，所以需要特殊处理
    if(TYPECHECK(*instr,assem::MoveInstr)){

      auto live_out_without_use = live_out->Diff(instr->Use());
      for(auto def : instr->Def()->GetList()){
        // 如果只被定义未用过，则将节点加入冲突图中
        if(!temp_node_map_->Look(def)){
          auto def_node = live_graph_.interf_graph->NewNode(def);
          temp_node_map_->Enter(def,def_node);
        }

        if(def == rsp){
          std::cerr<<"rsp be def"<<std::endl;
          continue;
        }

        // 为def和out中非use的部分添加冲突
        for(auto out : live_out_without_use->GetList()){
          if(out == rsp){
            std::cerr<<"rsp in out"<<std::endl;
            continue;
          }
          if(def != out){
            // std::cerr<<"add edge start"<<std::endl;
            live_graph_.interf_graph->AddEdge(temp_node_map_->Look(def),temp_node_map_->Look(out));
            live_graph_.interf_graph->AddEdge(temp_node_map_->Look(out),temp_node_map_->Look(def));
            // std::cerr<<"add edge end"<<std::endl;
            // AddEdge(temp_node_map_->Look(def),temp_node_map_->Look(out));
          }
          else{
            // std::cerr<<"def == out"<<std::endl;
          }
        }

        // 为该条move指令的def和use之间添加move
        for(auto use : instr->Use()->GetList()){
          // std::cerr<<"use: "<<use<<std::endl;
          if(use == rsp){
            std::cerr<<"rsp be used"<<std::endl;
            continue;
          }
          if(!live_graph_.moves->Contain(temp_node_map_->Look(def),temp_node_map_->Look(use)) && !live_graph_.moves->Contain(temp_node_map_->Look(use),temp_node_map_->Look(def))){
            auto def_node = temp_node_map_->Look(def);
            auto use_node = temp_node_map_->Look(use);
            // live_graph_.move_list_[def_node].Append(def_node, use_node);
            live_graph_.moves->Append(temp_node_map_->Look(def),temp_node_map_->Look(use));
            // live_graph_.move_list_[def_node].Append(def_node, use_node);
            // live_graph_.move_list_[use_node].Append(def_node, use_node);
          }
        }
      }
    }
    // 非move指令
    else{
      for(auto def: instr->Def()->GetList()){
        if(!temp_node_map_->Look(def)){
          auto def_node = live_graph_.interf_graph->NewNode(def);
          temp_node_map_->Enter(def,def_node);
        }
        if(def == rsp){
          continue;
        }
        for(auto out: live_out->GetList()){
          if(out == rsp){
            continue;
          }
          if(def != out){
            live_graph_.interf_graph->AddEdge(temp_node_map_->Look(def),temp_node_map_->Look(out));
            live_graph_.interf_graph->AddEdge(temp_node_map_->Look(out),temp_node_map_->Look(def));
            // AddEdge(temp_node_map_->Look(def),temp_node_map_->Look(out));
          }
        }
      }
    }
  }
  // for(fg::FNode *node : all_nodes) {
  //   assem::Instr *inst = node -> NodeInfo();
  //   temp::TempList *live = out_ -> Look(node);

  //   /**
  //    * Exclude global variable, string and %rsp
  //    * Because rsp does not have edge, so that cannot coalesce.
  //    */
  //   if(typeid(*inst) == typeid(assem::MoveInstr) &&
  //     inst -> Use() -> GetList().size() == 1 && inst -> Def() -> GetList().size() == 1 &&
  //     inst -> Use() -> NthTemp(0) != rsp) {

  //     live = live -> Diff(inst -> Use());

  //     INode *def_node = temp_node_map_ -> Look(inst -> Def() -> NthTemp(0));
  //     INode *use_node = temp_node_map_ -> Look(inst -> Use() -> NthTemp(0));
  //     live_graph_.move_list_[def_node].Append(def_node, use_node);
  //     live_graph_.move_list_[use_node].Append(def_node, use_node);

  //     live_graph_.moves -> Append(def_node, use_node);
  //   }

  //   assert(live -> Contain(rsp));
  //   // We still allocate for unused-after-defined temporary.
  //   live = live -> Union(inst -> Def());
  //   for(temp::Temp *def_temp : inst -> Def() -> GetList()) {
  //     INode *def_node = temp_node_map_ -> Look(def_temp);
  //     for(temp::Temp *live_temp : live -> GetList()) {
  //       INode *live_node = temp_node_map_ -> Look(live_temp);
  //       // live_graph_.interf_graph->AddEdge(def_node, live_node);
  //       AddEdge(def_node, live_node);
  //     }
  //   }
  // }
}

// void LiveGraphFactory::InterfGraph() {
//   // 加入预定义的机器寄存器节点
//   // 注意，rsp将不会放入冲突图中，我们使其单独独立
//   auto all_without_rsp = reg_manager->Registers();
//   for (auto temp : all_without_rsp->GetList()) {
    
//     // temp_node_map_[temp] = live_graph_.interf_graph->NewNode(temp);
//     auto reg_node = live_graph_.interf_graph->NewNode(temp);
//     temp_node_map_->Enter(temp, reg_node);
//   }

//   // 所有机器寄存器节点之间相互冲突
//   auto temp_it = all_without_rsp->GetList().begin();
//   auto temp_end = all_without_rsp->GetList().end();
//   for (; temp_it != temp_end; temp_it++) {
//     for (auto temp_inner_it = std::next(temp_it); temp_inner_it != temp_end;
//          temp_inner_it++) {
//       live_graph_.interf_graph->AddEdge(temp_node_map_->Look(*temp_it),
//                                         temp_node_map_->Look(*temp_inner_it));
//       live_graph_.interf_graph->AddEdge(temp_node_map_->Look(*temp_inner_it),
//                                         temp_node_map_->Look(*temp_it));
//     }
//   }
//   delete all_without_rsp;

//   // 将out包含的用过的临时变量节点加入
//   auto nodes = flowgraph_->Nodes()->GetList();
//   for (auto flow_node : nodes) {
//     auto out_temps = out_->Look(flow_node)->GetList();
//     for (auto temp : out_temps) {
//       if (!temp_node_map_->Look(temp)) {
//         auto temp_node = live_graph_.interf_graph->NewNode(temp);
//         temp_node_map_->Enter(temp, temp_node);
//     }
//   }

//   // 为临时加入变量构建冲突
//   auto rsp = reg_manager->GetRegister(frame::X64RegManager::Reg::RSP);
//   for (auto flow_node : nodes) {
//     auto assem = flow_node->NodeInfo();
//     auto live = out_->Look(flow_node);
//     if (typeid(*assem) == typeid(assem::MoveInstr)) {
//       // 传送指令，对新定义的temp做与非src的out之间的冲突检查
//       // 在codegen中，我们确保了MoveInstr指令的def和use均只有一个
//       // 传送指令中，def和use之间不应该添加冲突线，而是move的虚线
//       // auto out_minus_use = Subtract(live, assem->Use());
//       auto out_minus_use = live->Diff(assem->Use());
//       for (auto def : assem->Def()->GetList()) {
//         // 如果函数只定义而未用过，其将在之前未加入冲突图中，应该加入（虽然不会与除了本指令use的其他临时变量产生任何关系，因为其活跃区间仅限于这一条指令）
//         if (!temp_node_map_->Look(def)) {
//           auto def_node = live_graph_.interf_graph->NewNode(def);
//           temp_node_map_->Enter(def, def_node);
//           // temp_node_map_[def] = live_graph_.interf_graph->NewNode(def);
//         }
//         // 为非use的out与def间添加冲突
//         if (def == rsp)
//           continue;
//         for (auto out : out_minus_use->GetList()) {
//           if (out == rsp)
//             continue;
//           live_graph_.interf_graph->AddEdge(temp_node_map_->Look(def),
//                                             temp_node_map_->Look(out));
//           live_graph_.interf_graph->AddEdge(temp_node_map_->Look(out),
//                                             temp_node_map_->Look(def));
//         }
//         // 为该条MoveInstr的def和use添加move，方便以后尽可能分配同一个寄存器
//         for (auto use : assem->Use()->GetList()) {
//           if (use == rsp)
//             continue;
//           if (!live_graph_.moves->Contain(temp_node_map_->Look(def),
//                                           temp_node_map_->Look(use)) &&
//               !live_graph_.moves->Contain(temp_node_map_->Look(use),
//                                           temp_node_map_->Look(def))) {
//             live_graph_.moves->Append(temp_node_map_->Look(def), temp_node_map_->Look(use));
//           }
//         }
//       }
//     } else {
//       // 非传送指令，对新定义的temp做与out之间的冲突检查
//       for (auto def : assem->Def()->GetList()) {
//         if (def == rsp)
//           continue;
//         for (auto out : live->GetList()) {
//           if (out == rsp)
//             continue;
//           live_graph_.interf_graph->AddEdge(temp_node_map_->Look(def),
//                                             temp_node_map_->Look(out));
//           live_graph_.interf_graph->AddEdge(temp_node_map_->Look(out),
//                                             temp_node_map_->Look(def));
//         }
//       }
//     }
//   }
// }
// }


// void LiveGraphFactory::AddEdge(INodePtr src, INodePtr dst){
//   if(!src -> Adj(dst) && src != dst) {
//       live_graph_.interf_graph -> AddEdge(src, dst);
//       live_graph_.degree_[dst] ++;
//       live_graph_.degree_[src] ++;
//   }
// }

void LiveGraphFactory::Liveness() {
  std::cerr << "Liveness start" << std::endl;
  LiveMap();
  std::cerr << "LiveMap end" << std::endl;
  InterfGraph();
  std::cerr << "InterfGraph end" << std::endl;
}

} // namespace live