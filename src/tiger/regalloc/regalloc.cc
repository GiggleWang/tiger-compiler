#include "tiger/regalloc/regalloc.h"

#include "tiger/output/logger.h"
#include <iostream>

extern frame::RegManager *reg_manager;
extern std::map<std::string, std::pair<int, int>> frame_info_map;

namespace ra {
/* TODO: Put your lab6 code here */
    RegAllocator::RegAllocator(std::string function_name, std::unique_ptr<cg::AssemInstr> assem_instr)
        : func_name_(function_name), assem_instr_(std::move(assem_instr)) {
        result_ = std::make_unique<Result>();
        // simplifyWorklist = new live::INodeList();
        // freezeWorklist = new live::INodeList();
        // spillWorklist = new live::INodeList();
        // // activeMoves = new live::MoveList();
        // // coalescedMoves = new live::MoveList();
        // // constrainedMoves = new live::MoveList();
        // // frozenMoves = new live::MoveList();
        // spilledNodes = new live::INodeList();
        // coloredNodes = new live::INodeList();
        // precoloredNodes = new live::INodeList();
        // coalescedNodes = new live::INodeList();
        // selectStack = new live::INodeList();
    }

    Result::~Result() {
        delete coloring_;
        delete il_;
    }

    RegAllocator::~RegAllocator() {
        // 对所有内容都进行释放：
        delete live_graph_factory_;
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
        for(auto iter = move_list_map.begin(); iter != move_list_map.end(); iter++) {
            delete iter->second;
        }
    }

    // 清空所有状态，方便在这一轮溢出等结束后，重新从零开始下一轮的颜色分配
    void RegAllocator::Reset() {
        // 对所有内容都进行释放：
        delete live_graph_factory_;
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
        for(auto iter = move_list_map.begin(); iter != move_list_map.end(); iter++) {
            delete iter->second;
        }
    }

    // 各种不变式：
    /*
        preColored就是所有机器寄存器本身，所以在一开始就要把它们加入到coloredNodes中
        degree[u] = adjList[u] 交 (preColored 并 freezeWorklist 并 simplifyWorklist 并 spillWorklist)

        简化工作表的不变式：u是simplyWorklist中的一个节点，当且仅当：
        degree[u] < K 且 moveList[u] 交 (activeMoves 并 worklistMoves) = 空集

        冻结工作表的不变式：u是freezeWorklist中的一个节点，当且仅当：
        degree[u] < K 且 moveList[u] 交 (activeMoves 并 worklistMoves) != 空集

        所以简化和冻结工作表的并集应该是所有度数小于K的节点，且两个集互补

        溢出工作表的不变式：u是spillWorklist中的一个节点，当且仅当：
        degree[u] >= K
    */

   // 书上的main函数对应的是output.cc中调用的RegAlloc函数
   void RegAllocator::RegAlloc(){
    // 执行build部分的代码：
    Build();

    std::cerr<<"build end"<<std::endl;

    MakeWorklist();

    std::cerr<<"makeworklist end"<<std::endl;

    // 一直重复下面的循环直到四张表都为空
    while(!simplifyWorklist->GetList().empty() || !worklistMoves->GetList().empty() || !freezeWorklist->GetList().empty() || !spillWorklist->GetList().empty()) {
        if(!simplifyWorklist->GetList().empty()) {
            Simplify();
        }
        else if(!worklistMoves->GetList().empty()) {
            Coalesce();
        }
        else if(!freezeWorklist->GetList().empty()) {
            Freeze();
        }
        else if(!spillWorklist->GetList().empty()) {
            SelectSpill();
        }
    }
    std::cerr<<"ready to assign colors"<<std::endl;
    // 注意此时一定是四张表全空，所以也没有什么清空表的意义了
    // 四张表全空后进行着色
    AssignColors();

    // 如果此时还有溢出的node，那么就需要重写程序
    if(!spilledNodes->GetList().empty()) {
        RewriteProgram(); // rewrite结束之后记得顺便清掉所有状态
        // 重新分配寄存器
        move_list_map.clear();
        RegAlloc();
    }
    else {
        // 如果没有溢出的node，那么就可以得到结果
        DeleteRepeatMoves();
        result_->coloring_ = AssignRegisters();
        result_->il_ = assem_instr_->GetInstrList();
    }
   }

   // build之前的所有逻辑已经在liveness中实现，所以要从build开始实现后续逻辑：
    void RegAllocator::Build(){
        // 活跃性分析已经在liveness.cc中完成，调用即可，不需要再反复实现伪代码中的逻辑
        // 在这里先构造数据流图和冲突图
        std::cerr << "=== Starting Build ===" << std::endl;
        auto flow_graph_factory = new fg::FlowGraphFactory(assem_instr_->GetInstrList());
        flow_graph_factory->AssemFlowGraph();
        live_graph_factory_ = new live::LiveGraphFactory(flow_graph_factory->GetFlowGraph());
        live_graph_factory_->Liveness();
        std::cerr<<"get liveness end"<<std::endl;

        // 初始化各种工作表：worklistmoves不用初始化，他会直接指向moves
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

        // 初始化wordlistmoves，使得它包含所有move指令：
        // this->live_graph_ = live_graph_factory_->GetLiveGraph();
        worklistMoves = live_graph_factory_->GetLiveGraph().moves;

        // 初始化degree和move_list_map
        auto nodes = live_graph_factory_->GetLiveGraph().interf_graph->Nodes();
        for(auto node_iter : nodes->GetList()) {
            // degree[node_iter] = live_graph_factory_->GetLiveGraph().degree_[node_iter];
            // 遍历moveList，将每个node对应的movelist加入到move_list_map中
            live::MoveList *move_list = new live::MoveList();
            for(auto move_iter : live_graph_factory_->GetLiveGraph().moves->GetList()) {
                if(move_iter.first == node_iter || move_iter.second == node_iter) {
                    move_list->Append(move_iter.first, move_iter.second);
                }
            }
            move_list_map[node_iter] = move_list;
            degree[node_iter] = node_iter->Degree();
            // move_list_map[node_iter] = new live::MoveList(live_graph_factory_->GetLiveGraph().move_list_[node_iter]);
        }

        // 需要先对预节点进行着色,同时将regs中的寄存器的度设为最大值
        auto temp_node_map = live_graph_factory_->GetTempNodeMap();
        auto regs_with_rsp = reg_manager->Registers();
        // auto rsp = reg_manager->GetRegister(frame::X64RegManager::Reg::RSP);
        // regs_with_rsp->Insert(rsp,7);
        for(auto reg_iter : regs_with_rsp->GetList()) {
            auto reg_node = temp_node_map->Look(reg_iter);
            // coloredNodes->Append(reg_node);
            precoloredNodes->Append(reg_node);
            // 同样加入color中
            color[reg_node] = reg_iter;
            // degree[reg_node] = 0x7fffffff;
        }
        // delete flow_graph_factory;
        std::cerr << "=== Ending Build ===" << std::endl;
    }

    void RegAllocator::MakeWorklist(){
        std::cerr<<"make worklist start"<<std::endl;
        // 将所有度数小于K的节点加入到simplifyWorklist中，其余的加入到spillWorklist中
        auto nodes = live_graph_factory_->GetLiveGraph().interf_graph->Nodes();
        // // 要排除掉所有寄存器
        // auto regs_with_rsp = reg_manager->Registers();
        // auto rsp = reg_manager->GetRegister(frame::X64RegManager::Reg::RSP);
        // regs_with_rsp->Append(rsp);
        for(auto node_iter : nodes->GetList()) {
            // 要将节点中precolored部分无视掉，不然它也会进入splledWorklist
            if(precoloredNodes->Contain(node_iter)) {
                continue;
            }
            // for(auto reg_iter : regs_with_rsp->GetList()) {
            //     assert(node_iter != reg_iter);
            // }
            if(degree[node_iter]>=K) {
                spillWorklist->Append(node_iter);
            }
            else if(MoveRelated(node_iter)) {
                freezeWorklist->Append(node_iter);
            }
            else {
                simplifyWorklist->Append(node_iter);
            }
        }
    }

    /*
        简化工作表的不变式：u是simplyWorklist中的一个节点，当且仅当：
        degree[u] < K 且 moveList[u] 交 (activeMoves 并 worklistMoves) = 空集

        冻结工作表的不变式：u是freezeWorklist中的一个节点，当且仅当：
        degree[u] < K 且 moveList[u] 交 (activeMoves 并 worklistMoves) != 空集
    */
    // 用着额函数判断是否是moverelated，也就是是否交集是空集
    bool RegAllocator::MoveRelated(live::INodePtr node) {
        std::cerr<<"move related start"<<std::endl;
        auto result = NodeMoves(node);
        bool is_move_related = !result->GetList().empty();
        delete result;
        return is_move_related;
    }

    // 这个函数做了先取并集，再取交集的操作
    live::MoveList *RegAllocator::NodeMoves(live::INodePtr node) {
        auto move_list = move_list_map[node];
        auto union_list = activeMoves->Union(worklistMoves);
        auto result = move_list->Intersect(union_list);
        delete union_list;
        return result;
    }

    // 该函数的作用是在邻接表adjList中 去除 selectStack和coalescedNodes中的节点，拿到实际的邻接点表
    live::INodeListPtr RegAllocator::Adjacent(live::INodePtr node) {
        std::cerr<<"adjacent start"<<std::endl;
        auto adj_nodes = node->Adj(); // 该函数可以拿到所有的前驱和后继节点，也就是全部邻接点
        auto union_list = selectStack->Union(coalescedNodes);
        auto result = adj_nodes->Diff(union_list);
        delete union_list;
        return result;
    }

    bool RegAllocator::AddEdge(live::INodePtr u, live::INodePtr v) {
        std::cerr<<"add edge start"<<std::endl;
        if(u->Succ()->Contain(v)) {
            return false;
        }
        if(u == v) {
            return false;
        }
        live_graph_factory_->GetLiveGraph().interf_graph->AddEdge(u, v);
        live_graph_factory_->GetLiveGraph().interf_graph->AddEdge(v, u);
        // live_graph_factory_->AddEdge(u, v);
        degree[u]++;
        degree[v]++;
        return true;
    }

    void RegAllocator::Simplify(){
        // 从simplifyWorklist中取出一个节点，然后将它加入到selectStack中
        // 因为它是graph不是table，所以不能调用pop函数
        auto node = simplifyWorklist->GetList().front();
        simplifyWorklist->DeleteNode(node);
        // 因为stack是一个栈，所以要遵循先进先出，所以将队头视作栈顶，不能用append
        selectStack->Prepend(node);
        auto adj_nodes = Adjacent(node);
        for(auto adj_node : adj_nodes->GetList()) {
            DecrementDegree(adj_node);
        }
        delete adj_nodes;
    }

    void RegAllocator::DecrementDegree(live::INodePtr node) {
        std::cerr<<"decrement degree start"<<std::endl;
        int d = degree[node];
        degree[node] = d-1;
        if(d == K) {//  && !precoloredNodes->Contain(node)
            auto adj_nodes = Adjacent(node);
            adj_nodes->Append(node);// 将m与m的所有邻接点取并集
            EnableMoves(adj_nodes);
            spillWorklist->DeleteNode(node);
            if(MoveRelated(node)) {
                freezeWorklist->Append(node);
            }
            else {
                simplifyWorklist->Append(node);
            }
        }
    }

    void RegAllocator::EnableMoves(live::INodeListPtr nodes) {
        std::cerr<<"enable moves start"<<std::endl;
        for(auto node : nodes->GetList()) {
            auto move_list = NodeMoves(node);
            // 要记得区分开movelist和graph类的工具函数
            for(auto move : move_list->GetList()) {
                if(activeMoves->Contain(move.first, move.second)) {
                    activeMoves->Delete(move.first, move.second);
                    worklistMoves->Append(move.first, move.second);
                }
            }
            delete move_list;
        }
    }

    // 根据伪代码实现合并逻辑
    void RegAllocator::Coalesce(){
        std::cerr<<"coalesce start"<<std::endl;
        auto m = worklistMoves->GetList().front();
        auto x = GetAlias(m.first);
        auto y = GetAlias(m.second);
        // 声明要被合并的节点
        live::INodePtr u, v;
        if(precoloredNodes->Contain(y)) {
            u = y;
            v = x;
        }
        else {
            u = x;
            v = y;
        }
        worklistMoves->Delete(m.first, m.second);
        if(u == v) {
            coalescedMoves->Append(m.first, m.second);
            AddWorkList(u);
        }
        else if(precoloredNodes->Contain(v) || u->Succ()->Contain(v)) { //  || u->Pred()->Contain(v)这个是被限制的情况，就是move的两个临时变量之间同时存在冲突
            constrainedMoves->Append(m.first, m.second);
            AddWorkList(u);
            AddWorkList(v);
        }
        // 如果是precolored中的一员，且v的任意adjacent都满足OK条件；或者不是precolored中的一员，且u和v之间是consercative的
        else {
            // precoloredNodes->Contain(u) || (!precoloredNodes->Contain(u) && Conservative(u, v)
            bool is_else = false;
            if(precoloredNodes->Contain(u)){
                auto adj_v = Adjacent(v);
                for (auto adj_node : adj_v->GetList()) {
                    if(!OK(adj_node,u)){
                        is_else = true;
                    }
                }
                delete adj_v;
            }
            else {
                auto adj_u = Adjacent(u);
                auto adj_v = Adjacent(v);
                auto union_u_v = adj_u->Union(adj_v);
                if(!Conservative(union_u_v)){
                    is_else = true;
                }
                delete adj_u;
                delete adj_v;
                delete union_u_v;
            }
            if(!is_else){
                coalescedMoves->Append(m.first, m.second);
                Combine(u, v);
                AddWorkList(u);
            }
            else{
                activeMoves->Append(m.first, m.second);
            }
        }
        // else {
        //     activeMoves->Append(m.first, m.second);
        // }
    }

    live::INodePtr RegAllocator::GetAlias(live::INodePtr node) {
        // std::cerr<<"get alias start"<<std::endl;
        if(coalescedNodes->Contain(node)) {
            return GetAlias(alias[node]);
        }
        else {
            return node;
        }
    }

    // 合并中的addworklist，OK和,conserative,combine还没有实现
    void RegAllocator::AddWorkList(live::INodePtr node){
        std::cerr<<"add worklist start"<<std::endl;
        if(!precoloredNodes->Contain(node) && !MoveRelated(node) && degree[node] < K){
            freezeWorklist->DeleteNode(node);
            simplifyWorklist->Append(node);
        }
    }

    // 请帮我实现OK：
    bool RegAllocator::OK(live::INodePtr t, live::INodePtr r){
        std::cerr<<"ok start"<<std::endl;
        if (degree[t] < K || precoloredNodes->Contain(t) || t->Succ()->Contain(r) || t->Pred()->Contain(r)){ 
            return true;
        }
        else{
            return false;
        }
    }

    // conservative
    bool RegAllocator::Conservative(live::INodeListPtr nodes){
        std::cerr<<"conservative start"<<std::endl;
        int k = 0;
        for(auto node : nodes->GetList()){
            if (degree[node] >= K){
                k++;
            }
        }
        return k<K;
    }

    // combine
    void RegAllocator::Combine(live::INodePtr u, live::INodePtr v) {
        std::cerr<<"combine start"<<std::endl;
        if (freezeWorklist->Contain(v)) {
            freezeWorklist->DeleteNode(v);
        } else {
            spillWorklist->DeleteNode(v);
        }
        coalescedNodes->Append(v);
        alias[v] = u;
        auto move_list = move_list_map[u];
        move_list_map[u] = move_list->Union(move_list_map[v]);
        graph::NodeList<temp::Temp> v_in_list;
        v_in_list.Append(v);
        EnableMoves(&v_in_list);
        delete move_list;
        auto v_adj = Adjacent(v);
        for (auto t : v_adj->GetList()) {
            AddEdge(t, u);
            DecrementDegree(t);
        }
        if(degree[u] >= K && freezeWorklist->Contain(u)) {
            freezeWorklist->DeleteNode(u);
            spillWorklist->Append(u);
        }
    }

    void RegAllocator::Freeze(){
        std::cerr<<"freeze start"<<std::endl;
        auto u = freezeWorklist->GetList().front();
        freezeWorklist->DeleteNode(u);
        simplifyWorklist->Append(u);

        for(auto node : precoloredNodes->GetList()) {
            assert(node != u);
        }
        FreezeMoves(u);
    }

    void RegAllocator::FreezeMoves(live::INodePtr u){
        std::cerr<<"freeze moves start"<<std::endl;
        auto move_list = NodeMoves(u);
        for(auto move : move_list->GetList()) {
            live::INodePtr x = move.first;
            live::INodePtr y = move.second;
            live::INodePtr v;
            if(GetAlias(y) == GetAlias(u)) {
                v = GetAlias(x);
            }
            else {
                v = GetAlias(y);
            }
            activeMoves->Delete(x, y);
            frozenMoves->Append(x, y);
            if(NodeMoves(v)->GetList().empty() && degree[v] < K) {
                freezeWorklist->DeleteNode(v);
                simplifyWorklist->Append(v);
            }
        }
        delete move_list;
    }

    void RegAllocator::SelectSpill(){
        std::cerr<<"select spill start"<<std::endl;
        // m是使用所喜欢的启发是从spill中拿出来的，但是随便什么方法都行应该（？）
        auto m = spillWorklist->GetList().front();
        // 最好是选取一个拥有最长生命周期的节点进行取出：
        int max_life = -1;
        int pos = 0,start,end,life;

        for(auto node : spillWorklist->GetList()) {
            pos = 0;
            for(auto instr : assem_instr_->GetInstrList()->GetList()) {
                if(instr->Def()->Contain(node->NodeInfo())) {
                    start = pos;
                }
                if(instr->Use()->Contain(node->NodeInfo())) {
                    // end = pos;
                    life = pos - start;
                    if(life > max_life) {
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

    void RegAllocator::AssignColors(){
        std::cerr<<"assign colors start"<<std::endl;
        while(!selectStack->GetList().empty()) {
            // 用这两步模拟pop的操作
            auto node = selectStack->GetList().front();
            selectStack->DeleteNode(node);
            auto ok_colors = reg_manager->Registers();
            // auto rsp = reg_manager->GetRegister(frame::X64RegManager::Reg::RSP);
            // ok_colors->Insert(rsp, 7);

            auto adj_nodes = node->Adj();
            auto union_color = coloredNodes->Union(precoloredNodes);
            for(auto adj_node : adj_nodes->GetList()) {
                if(union_color->Contain(GetAlias(adj_node))) {
                    ok_colors->Delete(color[GetAlias(adj_node)]);
                }
            }
            if(ok_colors->GetList().empty()) {
                spilledNodes->Append(node);
            }
            else {
                coloredNodes->Append(node);
                // 任取一个颜色分配给他
                color[node] = ok_colors->GetList().front();
            }
            delete adj_nodes;
            delete union_color;
        }
        std::cerr<<"ready to find alias"<<std::endl;
        for(auto node : coalescedNodes->GetList()) {
            color[node] = color[GetAlias(node)];
        }
        std::cerr<<"assign colors end"<<std::endl;
    }

    void RegAllocator::RewriteProgram(){
        std::cerr<<"rewrite program start"<<std::endl;
        // 为每个溢出的vi分配一个存储单元
        // 为每一个定值和每一个使用创建一个新的临时变量
        // 在每一个vi的def指令后加入一个存储指令，在每一个use指令之前插一个load指令
        // 将所有vi放入newTemps，初始化的时候initial是coloredNodes和要被合并节点和newTemp的并集
        auto spilled_nodes = spilledNodes->GetList();
        for(auto node : spilled_nodes) {
            auto new_temp = temp::TempFactory::NewTemp();// 要替换的新的临时变量
            // auto instr = node->Instr();
            // offset += 8 ; // 每多一个溢出的node，frameszie就要多8个字节，同样那个变量的存储位置也要加8
            // auto access = static_cast<frame::InFrameAccess *>(frame_->AllocLocal(true));

            auto instr_list = assem_instr_->GetInstrList();
            // 这里需要改成迭代器，否则insert函数会出问题
            auto instr_iter = instr_list->GetList().begin();
            auto end = instr_list->GetList().end();
            for(;instr_iter != end; ++instr_iter) {
                auto src = (*instr_iter)->Use();
                auto dst = (*instr_iter)->Def();
                if(src!=nullptr && src->Contain(node->NodeInfo())) {
                    // 如果是使用，首先要将temp进行更换
                    src->Replace(node->NodeInfo(), new_temp);
                    // 然后在使用的指令中也进行替换
                    if(typeid(*(*instr_iter)) == typeid(assem::MoveInstr)) {
                        auto move_instr = static_cast<assem::MoveInstr *>(*instr_iter);
                        move_instr->src_=src;
                    }
                    else if(typeid(*(*instr_iter)) == typeid(assem::OperInstr)) {
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
                    if(frame_info_map[func_name_].first == 0) {
                        assem = "movq `s0, " + std::string(func_name_)+"_framesize_local-"+std::to_string(frame_info_map[func_name_].first) + ")(%rsp)";
                    }
                    assem = "movq (" + std::string(func_name_)+"_framesize_local"+std::to_string(frame_info_map[func_name_].first) + ")(%rsp), `d0";
                    instr_list->Insert(instr_iter, new assem::MoveInstr(assem, new temp::TempList(new_temp), new temp::TempList()));
                }
                if(dst!=nullptr && dst->Contain(node->NodeInfo())) {
                    // 如果是定值，首先要将temp进行更换
                    dst->Replace(node->NodeInfo(), new_temp);
                    // 然后在定值的指令中也进行替换
                    if(typeid(*(*instr_iter)) == typeid(assem::MoveInstr)) {
                        auto move_instr = static_cast<assem::MoveInstr *>(*instr_iter);
                        move_instr->dst_=dst;
                    }
                    else if(typeid(*(*instr_iter)) == typeid(assem::OperInstr)) {
                        auto oper_instr = static_cast<assem::OperInstr *>(*instr_iter);
                        oper_instr->dst_ = dst;
                    }
                    // 然后要在定值后先使用movq将值store到temp中，即插在下一条指令
                    // 此时的地址应该是rsp+framesize+offset

                    // 如果是在dst中，即要被定值，此时要存入，需要加offset

                    std::string assem;
                    frame_info_map[func_name_].first -= 8;
                    frame_info_map[func_name_].second += 8;
                    if(frame_info_map[func_name_].first == 0) {
                        assem = "movq " + std::string(func_name_)+"_framesize_local-"+std::to_string(frame_info_map[func_name_].first) + ")(%rsp), `d0";
                    }
                    assem = "movq `s0, (" + std::string(func_name_)+"_framesize_local"+std::to_string(frame_info_map[func_name_].first) + ")(%rsp)";
                    instr_list->Insert(std::next(instr_iter), new assem::MoveInstr(assem, new temp::TempList(), new temp::TempList(new_temp)));
                }
            }
        }
        // spilledNodes->Clear();
        // coloredNodes->Clear();
        // coalescedNodes->Clear();
        // precoloredNodes->Clear();
        // selectStack->Clear();
        // delete live_graph_factory_; 
        // for(auto iter = move_list_map.begin(); iter != move_list_map.end(); iter++) {
        //     delete iter->second;
        // }
        // 对所有内容都进行释放：
        delete live_graph_factory_;
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
        for(auto iter = move_list_map.begin(); iter != move_list_map.end(); iter++) {
            delete iter->second;
        }
        std::cerr<<"rewrite program end"<<std::endl;

    }

    std::unique_ptr<Result> RegAllocator::TransferResult(){
        std::cerr<<"transfer result"<<std::endl;
        return std::move(result_);
    }

    temp::Map * RegAllocator::AssignRegisters(){ //根据color进行实际的分配
        auto result = temp::Map::Empty();
        auto regs_with_rsp = reg_manager->Registers();
        // auto rsp = reg_manager->GetRegister(frame::X64RegManager::Reg::RSP);
        for(auto node : live_graph_factory_->GetLiveGraph().interf_graph->Nodes()->GetList()) {
            result->Enter(node->NodeInfo(), reg_manager->temp_map_->Look(color[node]));
        }
        // regs_with_rsp->Insert(rsp, 7);
        // for(auto pair:color){
        //     result->Enter(pair.first->NodeInfo(), reg_manager->temp_map_->Look(pair.second));
        // }
        return result;
    }

    void RegAllocator::DeleteRepeatMoves(){
        // 遍历整个指令集，将所有mov的前后相同的指令全部删除
        std::vector<assem::Instr *> instrs_to_delete;
        auto temp_map = live_graph_factory_->GetTempNodeMap();
        auto instr_iter = assem_instr_->GetInstrList()->GetList().begin();
        auto end = assem_instr_->GetInstrList()->GetList().end();
        auto rsp = reg_manager->GetRegister(frame::X64RegManager::Reg::RSP);
        for(;instr_iter != end; ++instr_iter) {
            if(typeid(*(*instr_iter)) == typeid(assem::MoveInstr)) {
                auto move_instr = static_cast<assem::MoveInstr *>(*instr_iter);
                auto src = move_instr->src_;
                
                auto dst = move_instr->dst_;
                if(src->GetList().size() == 1 && dst->GetList().size() == 1) {
                    
                    auto src_temp = src->GetList().front();
                    auto dst_temp = dst->GetList().front();
                    if(src_temp == rsp || dst_temp == rsp) {
                        continue;
                    }
                    auto src_node = temp_map->Look(src_temp);
                    auto dst_node = temp_map->Look(dst_temp);
                    if(color[src_node] == color[dst_node]) {
                        instrs_to_delete.push_back(move_instr);
                    }
                }
            }
        }
        
        for(auto instr_to_delete : instrs_to_delete) {
            assem_instr_->GetInstrList()->Remove(instr_to_delete);
        }
    }



} // namespace ra

                // // 遍历所有指令，找到对这个node的定值和使用
                // temp::TempList *src = nullptr, *dst = nullptr;
                // if(typeid(*instr_iter) == typeid(assem::MoveInstr)) {
                //     auto move_instr = static_cast<assem::MoveInstr *>(instr_iter);
                //     src = move_instr->src_;
                //     dst = move_instr->dst_;
                // }
                // else if(typeid(*instr_iter) == typeid(assem::OperInstr)) {
                //     auto oper_instr = static_cast<assem::OperInstr *>(instr_iter);
                //     src = oper_instr->src_;
                //     dst = oper_instr->dst_;
                // }
                // else { // 此时为label，一定不会是use或者def
                //     continue;
                // }