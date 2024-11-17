#include <iostream>
#include <llvm-14/llvm/IR/Instructions.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <map>

std::map<std::string, llvm::StructType *> struct_types;
std::map<std::string, llvm::GlobalVariable *> global_values;
std::map<std::string, llvm::Function *> functions;

llvm::StructType *addStructType(std::shared_ptr<llvm::Module> ir_module,
                                std::string name,
                                std::vector<llvm::Type *> fields) {
  llvm::StructType *struct_type =
      llvm::StructType::create(ir_module->getContext(), name);
  struct_type->setBody(fields);
  struct_types.insert(std::make_pair(name, struct_type));
  return struct_type;
}

llvm::GlobalVariable *addGlobalValue(std::shared_ptr<llvm::Module> ir_module,
                                     std::string name, llvm::Type *type,
                                     llvm::Constant *initializer, int align) {
  llvm::GlobalVariable *global =
      (llvm::GlobalVariable *)ir_module->getOrInsertGlobal(name, type);
  global->setInitializer(initializer);
  global->setDSOLocal(true);
  global->setAlignment(llvm::MaybeAlign(align));
  global_values.insert(std::make_pair(name, global));
  return global;
}

llvm::GlobalVariable *addGlobalString(std::shared_ptr<llvm::Module> ir_module,
                                      std::string name, std::string value) {
  llvm::GlobalVariable *global =
      (llvm::GlobalVariable *)ir_module->getOrInsertGlobal(
          name,
          llvm::ArrayType::get(llvm::Type::getInt8Ty(ir_module->getContext()),
                               value.size() + 1));
  global->setInitializer(
      llvm::ConstantDataArray::getString(ir_module->getContext(), value, true));
  global->setDSOLocal(true);
  global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
  global->setLinkage(llvm::GlobalValue::LinkageTypes::PrivateLinkage);
  global->setConstant(true);
  global->setAlignment(llvm::MaybeAlign(1));
  global_values.insert(std::make_pair(name, global));
  return global;
}

llvm::Function *addFunction(std::shared_ptr<llvm::Module> ir_module,
                            std::string name, llvm::FunctionType *type) {
  llvm::Function *function = llvm::Function::Create(
      type, llvm::Function::ExternalLinkage, name, ir_module.get());
  for (auto &arg : function->args())
    arg.addAttr(llvm::Attribute::NoUndef);
  functions.insert(std::make_pair(name, function));
  return function;
}

void buildGlobal(std::shared_ptr<llvm::Module> ir_module) {
  llvm::IRBuilder<> ir_builder(ir_module->getContext());

  // add struct type
  llvm::Type *Edge_s_fields[] = {
      llvm::Type::getInt32Ty(ir_module->getContext()),
      llvm::Type::getInt32Ty(ir_module->getContext()),
      llvm::Type::getInt64Ty(ir_module->getContext())};
  llvm::StructType *Edge_s = addStructType(
      ir_module, "struct.Edge_s",
      std::vector<llvm::Type *>(Edge_s_fields, Edge_s_fields + 3));

  // add global value
  llvm::PointerType::get(Edge_s, 0);
  llvm::GlobalVariable *edge1 = addGlobalValue(
      ir_module, "edge1", Edge_s,
      llvm::ConstantStruct::get(
          Edge_s,
          llvm::ConstantInt::get(
              llvm::Type::getInt32Ty(ir_module->getContext()), 0),
          llvm::ConstantInt::get(
              llvm::Type::getInt32Ty(ir_module->getContext()), 0),
          llvm::ConstantInt::get(
              llvm::Type::getInt64Ty(ir_module->getContext()), 5)),
      8);
  llvm::GlobalVariable *edge2 = addGlobalValue(
      ir_module, "edge2", Edge_s,
      llvm::ConstantStruct::get(
          Edge_s,
          llvm::ConstantInt::get(
              llvm::Type::getInt32Ty(ir_module->getContext()), 0),
          llvm::ConstantInt::get(
              llvm::Type::getInt32Ty(ir_module->getContext()), 0),
          llvm::ConstantInt::get(
              llvm::Type::getInt64Ty(ir_module->getContext()), 10)),
      8);
  addGlobalValue(ir_module, "allDist",
                 llvm::ArrayType::get(
                     llvm::ArrayType::get(
                         llvm::Type::getInt32Ty(ir_module->getContext()), 3),
                     3),
                 llvm::ConstantAggregateZero::get(llvm::ArrayType::get(
                     llvm::ArrayType::get(
                         llvm::Type::getInt32Ty(ir_module->getContext()), 3),
                     3)),
                 16);
  addGlobalValue(
      ir_module, "dist",
      llvm::ArrayType::get(llvm::PointerType::get(Edge_s, 0), 3),
      llvm::ConstantArray::get(
          llvm::ArrayType::get(llvm::PointerType::get(Edge_s, 0), 3),
          {llvm::ConstantExpr::getBitCast(edge1,
                                          llvm::PointerType::get(Edge_s, 0)),
           llvm::ConstantExpr::getBitCast(edge2,
                                          llvm::PointerType::get(Edge_s, 0)),
           llvm::ConstantPointerNull::get(llvm::PointerType::get(Edge_s, 0))}),
      16);
  addGlobalValue(ir_module, "minDistance",
                 llvm::Type::getInt64Ty(ir_module->getContext()),
                 llvm::ConstantInt::get(
                     llvm::Type::getInt64Ty(ir_module->getContext()), 5),
                 8);

  // add global string
  llvm::GlobalVariable *str = addGlobalString(ir_module, ".str", "%lld\00");
  llvm::GlobalVariable *str1 =
      addGlobalString(ir_module, ".str1", "%lld %lld %d\n\00");

  // add external function
  addFunction(ir_module, "printf",
              llvm::FunctionType::get(
                  llvm::Type::getInt32Ty(ir_module->getContext()),
                  llvm::PointerType::get(
                      llvm::Type::getInt8Ty(ir_module->getContext()), 0),
                  true));
}

void buildCaculateDistance(std::shared_ptr<llvm::Module> ir_module) {
  llvm::IRBuilder<> builder(ir_module->getContext());

  llvm::Function *caculateDistance =
      addFunction(ir_module, "caculateDistance",
                  llvm::FunctionType::get(
                      llvm::Type::getVoidTy(ir_module->getContext()), false));
  llvm::BasicBlock *entry =
      llvm::BasicBlock::Create(ir_module->getContext(), "", caculateDistance);
  llvm::BasicBlock *entry_3 =
      llvm::BasicBlock::Create(ir_module->getContext(), "3", caculateDistance);
  llvm::BasicBlock *entry_6 =
      llvm::BasicBlock::Create(ir_module->getContext(), "6", caculateDistance);
  llvm::BasicBlock *entry_16 =
      llvm::BasicBlock::Create(ir_module->getContext(), "16", caculateDistance);
  llvm::BasicBlock *entry_18 =
      llvm::BasicBlock::Create(ir_module->getContext(), "18", caculateDistance);
  llvm::BasicBlock *entry_20 =
      llvm::BasicBlock::Create(ir_module->getContext(), "20", caculateDistance);
  llvm::BasicBlock *entry_22 =
      llvm::BasicBlock::Create(ir_module->getContext(), "22", caculateDistance);
  llvm::BasicBlock *entry_25 =
      llvm::BasicBlock::Create(ir_module->getContext(), "25", caculateDistance);

  builder.SetInsertPoint(entry);
  llvm::AllocaInst *alloca_1 = builder.CreateAlloca(
      llvm::Type::getInt32Ty(ir_module->getContext()), nullptr, "1");
  llvm::AllocaInst *alloca_2 = builder.CreateAlloca(
      llvm::Type::getInt64Ty(ir_module->getContext()), nullptr, "2");
  llvm::Value *val_0 = llvm::ConstantInt::get(
      llvm::Type::getInt32Ty(ir_module->getContext()), 0);
  builder.CreateStore(val_0, alloca_1);
  builder.CreateBr(entry_3);

  // entry_3
  builder.SetInsertPoint(entry_3);
  llvm::Value *load_4 = builder.CreateLoad(
      llvm::Type::getInt32Ty(ir_module->getContext()), alloca_1, "4");
  llvm::Value *val_3 = llvm::ConstantInt::get(
      llvm::Type::getInt32Ty(ir_module->getContext()), 3);
  llvm::Value *icmp_5 = builder.CreateICmpSLT(load_4, val_3, "5");
  builder.CreateCondBr(icmp_5, entry_6, entry_25);

  // entry_6
  builder.SetInsertPoint(entry_6);
  llvm::Value *load_7 = builder.CreateLoad(
      llvm::Type::getInt32Ty(ir_module->getContext()), alloca_1, "7");
  llvm::Value *sext_8 = builder.CreateSExt(
      load_7, llvm::Type::getInt64Ty(ir_module->getContext()),"8");
  //%9 = getelementptr inbounds [3 x %struct.Edge_s*], [3 x %struct.Edge_s*]*
  llvm::GlobalVariable *dist = global_values["dist"];
  llvm::Value *zero = llvm::ConstantInt::get(
      llvm::Type::getInt64Ty(ir_module->getContext()), 0); // 索引 0
  llvm::Value *gepIndex = sext_8; // %8 已经通过 SExt 转换为 i64
  llvm::Value *gep_9 =
      builder.CreateInBoundsGEP(dist->getValueType(), // 数组类型：[3 x %struct.Edge_s*]
                        dist,                 // 基地址
                        {zero, gepIndex}, // 索引列表
                        "9"               // 结果变量名
      );
  llvm::StructType *Edge_s = struct_types["struct.Edge_s"];
  llvm::Value *load_10 = builder.CreateLoad(
      llvm::PointerType::get(Edge_s, 0), // 加载的类型：%struct.Edge_s*
      gep_9,                             // 源地址：%9
      "10"                               // 加载结果的名字
  );
  //%11 = getelementptr inbounds %struct.Edge_s, %struct.Edge_s* %10, i32 0, i32
  // 2
  llvm::Value *gep_11 = builder.CreateInBoundsGEP(
      Edge_s,  // Base type: %struct.Edge_s
      load_10, // Pointer to the base: %10
      {llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_module->getContext()),
                              0),
       llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_module->getContext()),
                              2)},
      "11" // Name of the result
  );
  llvm::Value *load_12 = builder.CreateLoad(
      llvm::Type::getInt64Ty(ir_module->getContext()), gep_11, "12");
  builder.CreateStore(load_12, alloca_2);
  llvm::Value *load_13 = builder.CreateLoad(
      llvm::Type::getInt64Ty(ir_module->getContext()), alloca_2, "13");
  llvm::GlobalVariable *minDistance_6 = global_values["minDistance"];
  llvm::Value *load_14 = builder.CreateLoad(
      llvm::Type::getInt64Ty(ir_module->getContext()), minDistance_6, "14");
  llvm::Value *icmp_15 = builder.CreateICmpSLT(load_13, load_14, "15");
  builder.CreateCondBr(icmp_15, entry_16, entry_18);

  // entry_16
  builder.SetInsertPoint(entry_16);
  llvm::Value *load_17 = builder.CreateLoad(
      llvm::Type::getInt64Ty(ir_module->getContext()), alloca_2, "17");
  builder.CreateBr(entry_20);

  // entry_18
  builder.SetInsertPoint(entry_18);
  llvm::GlobalVariable *minDistance_18 = global_values["minDistance"];
  llvm::Value *load_19 = builder.CreateLoad(
      llvm::Type::getInt64Ty(ir_module->getContext()), minDistance_18, "19");
  builder.CreateBr(entry_20);

  // entry_20
  builder.SetInsertPoint(entry_20);
  llvm::PHINode *phi_21 = builder.CreatePHI(
      llvm::Type::getInt64Ty(ir_module->getContext()), 2, "21");
  phi_21->addIncoming(load_17, entry_16);
  phi_21->addIncoming(load_19, entry_18);
  llvm::GlobalVariable *minDistance_20 = global_values["minDistance"];
  builder.CreateStore(phi_21, minDistance_20);
  builder.CreateBr(entry_22);

  // entry_22
  builder.SetInsertPoint(entry_22);
  llvm::Value *load_23 = builder.CreateLoad(
      llvm::Type::getInt32Ty(ir_module->getContext()), alloca_1, "23");
  llvm::Value *val_1 = llvm::ConstantInt::get(
      llvm::Type::getInt32Ty(ir_module->getContext()), 1);
  llvm::Value *add_24 = builder.CreateNSWAdd(load_23, val_1);
  builder.CreateStore(add_24, alloca_1);
  builder.CreateBr(entry_3);

  // entry_25
  builder.SetInsertPoint(entry_25);
  builder.CreateRetVoid();
}

void buildMain(std::shared_ptr<llvm::Module> ir_module) {
  llvm::IRBuilder<> builder(ir_module->getContext());

  llvm::Function *main =
      addFunction(ir_module, "main",
                  llvm::FunctionType::get(
                      llvm::Type::getInt32Ty(ir_module->getContext()), false));
  llvm::BasicBlock *entry =
      llvm::BasicBlock::Create(ir_module->getContext(), "", main);
  builder.SetInsertPoint(entry);
  llvm::AllocaInst *alloca_1 = builder.CreateAlloca(
      llvm::Type::getInt32Ty(ir_module->getContext()), nullptr, "1");
  llvm::StructType *Edge_s = struct_types["struct.Edge_s"];
  llvm::AllocaInst *alloca_2 = builder.CreateAlloca(Edge_s, nullptr, "2");
  llvm::Value *val_0 = llvm::ConstantInt::get(
      llvm::Type::getInt32Ty(ir_module->getContext()), 0);
  builder.CreateStore(val_0, alloca_1);
  llvm::Value *gep_3 = builder.CreateInBoundsGEP(
      Edge_s,   // Base type: %struct.Edge_s
      alloca_2, // Pointer to the base: %10
      {llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_module->getContext()),
                              0),
       llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_module->getContext()),
                              2)},
      "3" // Name of the result
  );
  //  %4 = call i32 (i8*, ...) @__isoc99_scanf(i8* getelementptr inbounds ([5 x
  //  i8], [5 x i8]* @.str, i64 0, i64 0), i64* %3)
  llvm::LLVMContext &context = ir_module->getContext();
  llvm::FunctionType *scanfType = llvm::FunctionType::get(
      llvm::Type::getInt32Ty(context), // 返回值类型：i32
      {llvm::PointerType::get(llvm::Type::getInt8Ty(context),
                              0)}, // 第一个参数类型：i8* (格式字符串)
      true                         // 可变参数
  );
  llvm::Function *scanfFunc =
      llvm::Function::Create(scanfType, llvm::Function::ExternalLinkage,
                             "__isoc99_scanf", ir_module.get());
llvm::GlobalVariable *str = global_values[".str"];
  llvm::Value *gep_str = builder.CreateInBoundsGEP(
      str->getValueType(), str, // 类型和指针
      {llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), 0),
       llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), 0)},
      "gep_str");
  llvm::CallInst *call_4 =
      builder.CreateCall(scanfFunc, {gep_str, gep_3}, "call_4");
  // store %struct.Edge_s* %2, %struct.Edge_s** getelementptr inbounds ([3 x
  //%struct.Edge_s*], [3 x %struct.Edge_s*]* @dist, i64 0, i64 2), align 8

  llvm::GlobalVariable *dist = global_values["dist"];
  llvm::Value *gep_dist = builder.CreateInBoundsGEP(
      dist->getValueType(), // Base type: [3 x %struct.Edge_s*]
      dist,                 // Pointer: @dist
      {llvm::ConstantInt::get(llvm::Type::getInt64Ty(ir_module->getContext()),
                              0),
       llvm::ConstantInt::get(llvm::Type::getInt64Ty(ir_module->getContext()),
                              2)},
      "gep_dist");
  builder.CreateStore(alloca_2, gep_dist);

  llvm::Value *gep_5 = builder.CreateInBoundsGEP(
      Edge_s,   // Base type: %struct.Edge_s
      alloca_2, // Pointer to the base: %10
      {llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_module->getContext()),
                              0),
       llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_module->getContext()),
                              2)},
      "5" // Name of the result
  );
  llvm::Value *load_6 = builder.CreateLoad(
      llvm::Type::getInt64Ty(ir_module->getContext()), gep_5, "6");
  // %7 = trunc i64 %6 to i32
  // %7 = trunc i64 %6 to i32
  llvm::Value *trunc_7 = builder.CreateTrunc(
      load_6,                                          // %6 的值
      llvm::Type::getInt32Ty(ir_module->getContext()), // 目标类型 i32
      "7"                                              // 结果变量名
  );
  // store i32 %7, i32* getelementptr inbounds ([3 x [3 x i32]], [3 x [3 x
  // i32]]* @allDist, i64 0, i64 0, i64 0), align 4
  llvm::GlobalVariable *allDist = global_values["allDist"];
  llvm::Value *index0 = llvm::ConstantInt::get(
      llvm::Type::getInt64Ty(ir_module->getContext()), 0); // i64 0
  llvm::Value *gep_allDist =
      builder.CreateInBoundsGEP(allDist->getValueType(),  // [3 x [3 x i32]]
                                allDist,                  // 基地址
                                {index0, index0, index0}, // 偏移量列表
                                "gep_allDist");
  builder.CreateStore(trunc_7,    // 要存储的值：i32 %7
                      gep_allDist // 存储目标地址
  );
  llvm::Function *caculateDistanceFunc = functions["caculateDistance"];
  builder.CreateCall(caculateDistanceFunc, {}, ""); // 无参数调用
  llvm::GlobalVariable *minDistance = global_values["minDistance"];
  llvm::Value *load_minDistance = builder.CreateLoad(
      llvm::Type::getInt64Ty(ir_module->getContext()), // i64 类型
      minDistance, // 要加载的全局变量
      "8"          // 加载的结果变量名
  );
  Edge_s = struct_types["struct.Edge_s"];
  llvm::Value *gep_9 = builder.CreateInBoundsGEP(
      Edge_s,   // 结构体类型
      alloca_2, // 指向结构体的指针（%2）
      {llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_module->getContext()),
                              0), // i32 0
       llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_module->getContext()),
                              2)}, // i32 2
      "9"                          // 结果变量名
  );
  llvm::Value *load_10 = builder.CreateLoad(
      llvm::Type::getInt64Ty(ir_module->getContext()), // i64 类型
      gep_9,                                           // 指针 %9
      "10"                                             // 加载结果变量名
  );

  llvm::Value *add_11 = builder.CreateNSWAdd(
      load_10, // %10 的值
      llvm::ConstantInt::get(llvm::Type::getInt64Ty(ir_module->getContext()),
                             5), // 常量 5
      "11"                       // 结果变量名
  );

  llvm::Value *add_12 = builder.CreateNSWAdd(
      add_11, // %11 的值
      llvm::ConstantInt::get(llvm::Type::getInt64Ty(ir_module->getContext()),
                             10), // 常量 10
      "12"                        // 结果变量名
  );

  allDist = global_values["allDist"];

  index0 = llvm::ConstantInt::get(
      llvm::Type::getInt64Ty(ir_module->getContext()), 0); // i64 0

  gep_allDist = builder.CreateInBoundsGEP(
      allDist->getValueType(),  // 全局变量类型：[3 x [3 x i32]]
      allDist,                  // 全局变量指针
      {index0, index0, index0}, // 偏移量
      "gep_allDist");

  llvm::Value *load_13 = builder.CreateLoad(
      llvm::Type::getInt32Ty(ir_module->getContext()), // i32 类型
      gep_allDist,                                     // 指针地址
      "13"                                             // 加载结果变量名
  );
  llvm::Function *printfFunc = functions["printf"];
  llvm::GlobalVariable *str1 = global_values[".str1"];
  llvm::Value *gep_str1 = builder.CreateInBoundsGEP(
      str1->getValueType(), // 字符串的类型：[14 x i8]
      str1,                 // 字符串全局变量
      {llvm::ConstantInt::get(llvm::Type::getInt64Ty(ir_module->getContext()),
                              0),
       llvm::ConstantInt::get(llvm::Type::getInt64Ty(ir_module->getContext()),
                              0)},
      "gep_str1");
  llvm::Value *call_14 = builder.CreateCall(
      printfFunc,                                    // @printf 函数
      {gep_str1, load_minDistance, add_12, load_13}, // 参数列表
      "14"                                           // 调用结果变量名
  );

  llvm::Value *returnValue = llvm::ConstantInt::get(
      llvm::Type::getInt32Ty(ir_module->getContext()), 0); // 定义返回值 0
  builder.CreateRet(returnValue);
}

void buildFunction(std::shared_ptr<llvm::Module> ir_module) {
  buildCaculateDistance(ir_module);
  buildMain(ir_module);
}

int main(int, char **) {
  llvm::LLVMContext context;
  std::shared_ptr<llvm::Module> ir_module =
      std::make_shared<llvm::Module>("calculateDistance", context);
  ir_module->setTargetTriple("x86_64-pc-linux-gnu");

  buildGlobal(ir_module);
  buildFunction(ir_module);

  ir_module->print(llvm::outs(), nullptr);

  return 0;
}
