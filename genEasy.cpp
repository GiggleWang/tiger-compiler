#include <iostream>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <map>

std::map<std::string, llvm::Function *> functions;

llvm::Function *addFunction(std::shared_ptr<llvm::Module> ir_module, std::string name, llvm::FunctionType *type)
{
    llvm::Function *function = llvm::Function::Create(type, llvm::Function::ExternalLinkage, name, ir_module.get());
    for (auto &arg : function->args())
        arg.addAttr(llvm::Attribute::NoUndef);
    functions.insert(std::make_pair(name, function));
    return function;
}

void buildGlobal(std::shared_ptr<llvm::Module> ir_module)
{
}

void buildMain(std::shared_ptr<llvm::Module> ir_module)
{
    // llvm::IRBuilder<> builder(ir_module->getContext());

    // llvm::Function *main = addFunction(ir_module, "main", llvm::FunctionType::get(llvm::Type::getInt32Ty(ir_module->getContext()), false));
    // llvm::BasicBlock *entry = llvm::BasicBlock::Create(ir_module->getContext(), "", main);
    // builder.SetInsertPoint(entry);

    // TODO 
    llvm::IRBuilder<> builder(ir_module->getContext());

    // Define the main function: int main()
    llvm::Function *main = addFunction(ir_module, "main", llvm::FunctionType::get(llvm::Type::getInt32Ty(ir_module->getContext()), false));
    llvm::BasicBlock *entry = llvm::BasicBlock::Create(ir_module->getContext(), "entry", main);
    llvm::BasicBlock *ifTrue = llvm::BasicBlock::Create(ir_module->getContext(), "if.true", main);
    llvm::BasicBlock *ifEnd = llvm::BasicBlock::Create(ir_module->getContext(), "if.end", main);
    builder.SetInsertPoint(entry);

    // Define local variables
    llvm::Value *a = builder.CreateAlloca(llvm::Type::getInt32Ty(ir_module->getContext()), nullptr, "a");
    llvm::Value *b = builder.CreateAlloca(llvm::Type::getInt32Ty(ir_module->getContext()), nullptr, "b");

    // a = 1;
    builder.CreateStore(llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_module->getContext()), 1), a);

    // b = 2;
    builder.CreateStore(llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_module->getContext()), 2), b);

    // if (a < b)
    llvm::Value *aValue = builder.CreateLoad(llvm::Type::getInt32Ty(ir_module->getContext()), a, "aValue");
    llvm::Value *bValue = builder.CreateLoad(llvm::Type::getInt32Ty(ir_module->getContext()), b, "bValue");
    llvm::Value *cmp = builder.CreateICmpSLT(aValue, bValue, "cmp");
    builder.CreateCondBr(cmp, ifTrue, ifEnd);

    // if.true block
    builder.SetInsertPoint(ifTrue);
    // b = 3;
    builder.CreateStore(llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_module->getContext()), 3), b);
    builder.CreateBr(ifEnd);

    // if.end block
    builder.SetInsertPoint(ifEnd);
    // return a + b;
    aValue = builder.CreateLoad(llvm::Type::getInt32Ty(ir_module->getContext()), a, "aValue");
    bValue = builder.CreateLoad(llvm::Type::getInt32Ty(ir_module->getContext()), b, "bValue");
    llvm::Value *result = builder.CreateAdd(aValue, bValue, "result");
    builder.CreateRet(result);
}

void buildFunction(std::shared_ptr<llvm::Module> ir_module)
{
    buildMain(ir_module);
}

int main()
{
    llvm::LLVMContext context;
    std::shared_ptr<llvm::Module> ir_module = std::make_shared<llvm::Module>("easy", context);
    ir_module->setTargetTriple("x86_64-pc-linux-gnu");

    buildGlobal(ir_module);
    buildFunction(ir_module);

    ir_module->print(llvm::outs(), nullptr);

    return 0;
}