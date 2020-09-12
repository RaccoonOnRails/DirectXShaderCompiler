//===- Hello.cpp - Example code from "Writing an LLVM Pass" ---------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements two versions of the LLVM "Hello World" pass described
// in docs/WritingAnLLVMPass.html
//
//===----------------------------------------------------------------------===//

#include "dxc/HLSL/DxilGenerationPass.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/IR/CallSite.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/Pass.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

#define DEBUG_TYPE "rps"

namespace {
// Rps - The second implementation with getAnalysisUsage implemented.
struct DxilToRps : public ModulePass {
  static char ID; // Pass identification, replacement for typeid
  DxilToRps() : ModulePass(ID) {}

  Function *m_RpsNodeCallFunc = nullptr;
  Function *m_RpsNodeParamPushFunc = nullptr;
  std::vector<StringRef> m_RpsNodeNames = {};
  StringMap<int32_t> m_RpsNodeNameIndice = {};
  StringMap<Function *> m_RpsLibFuncs = {};
  GlobalVariable *m_ModuleIdGlobal = nullptr;

  enum class NodeParamTypeCategory {
    Resource,
    View,
    RawBytes,
  };

  static StringRef AddModuleNamePostfix(const char *prefix, Module &M) {
    return (prefix + std::string(M.getName().empty() ? "" : "_") + M.getName()).str();
  }

  bool runOnModule(Module &M) override {
    auto voidType = Type::getVoidTy(M.getContext());
    auto intType = Type::getInt32Ty(M.getContext());
    auto nodeCallFunc =
        M.getOrInsertFunction("__rps_node_call", voidType, intType, intType, nullptr); // ModuleId, NodeId
    m_RpsNodeCallFunc = dyn_cast<Function>(nodeCallFunc);
    m_RpsNodeCallFunc->setLinkage(GlobalValue::ExternalLinkage);

    auto int8PtrType = Type::getInt8PtrTy(M.getContext());
    auto paramPushFunc =
        M.getOrInsertFunction("__rps_param_push", voidType, int8PtrType, intType, intType, nullptr);
    m_RpsNodeParamPushFunc = dyn_cast<Function>(paramPushFunc);
    m_RpsNodeParamPushFunc->setLinkage(GlobalValue::ExternalLinkage);

    m_ModuleIdGlobal = dyn_cast<GlobalVariable>(M.getOrInsertGlobal(AddModuleNamePostfix("__rps_module_id", M), intType));

    ConstantInt *constIntZero = ConstantInt::get(M.getContext(), APInt(32, 0));
    m_ModuleIdGlobal->setInitializer(constIntZero);
    m_ModuleIdGlobal->setLinkage(GlobalValue::ExternalLinkage);
    m_ModuleIdGlobal->setConstant(false);

    m_RpsLibFuncs.insert(std::make_pair("describe_resource", nullptr));
    m_RpsLibFuncs.insert(std::make_pair("describe_view", nullptr));
    m_RpsLibFuncs.insert(std::make_pair("create_resource", nullptr));
    m_RpsLibFuncs.insert(std::make_pair("create_view", nullptr));
    m_RpsLibFuncs.insert(std::make_pair("clear_view", nullptr));

    for (auto &F : M) {
      printf("\nFunction %s", F.getName().data());
      runOnFunction(M, F);
    }

    WriteNodeTable(M);

    WriteExportEntries(M);

    return true;
  }

  void ReplaceLibFunction(CallInst *C) {
    auto calleeF = C->getCalledFunction();
    auto demangledName = DemangleNames(calleeF->getName());
    auto libFuncIter = m_RpsLibFuncs.find(demangledName);

    if (libFuncIter != m_RpsLibFuncs.end()) {

      if (libFuncIter->second == nullptr) {
        auto newFuncName = "__rps_" + demangledName;
        calleeF->setName(newFuncName);
        libFuncIter->second = calleeF;
      }

      printf("\n        Replaced: %s => %s", demangledName.str().c_str(),
             C->getCalledFunction()->getName().str().c_str());
    }
  }

  NodeParamTypeCategory GetNodeParamTypeCategory(Module &M, Type *pType) {
    if (pType->isStructTy()) {
      auto name = pType->getStructName();

      if ((name == "struct.srv") || (name == "struct.rtv") ||
          (name == "struct.uav") || (name == "struct.view")) {
        return NodeParamTypeCategory::View;
      } else if (name == "struct.resource") {
        return NodeParamTypeCategory::Resource;
      }
    }
    return NodeParamTypeCategory::RawBytes;
  }

  bool runOnFunction(Module& M, Function &F) {

    llvm::SmallVector<CallInst *, 32> callInsts;

    if (!F.empty() && (F.getLinkage() == GlobalValue::ExternalLinkage)) {
      F.setName(DemangleNames(F.getName()));
    }

    for (auto bbIter = F.begin(); bbIter != F.end(); ++bbIter) {
      for (auto inst = bbIter->begin(); inst != bbIter->end(); ++inst) {

        auto callInst = dyn_cast<CallInst>(inst);
        if (callInst == nullptr) {
          continue;
        }

        auto callSite = CallSite(callInst);
        auto calleeF = callInst->getCalledFunction();
        auto calleeName = calleeF->getName();

        printf("\n    Callee %s", calleeName.data());

        auto nodeDefIdx = getNodeDefIndex(calleeF);
        if (nodeDefIdx >= 0) {
          auto nodeDefIdxVal = ConstantInt::get(
              Type::getInt32Ty(F.getContext()), nodeDefIdx, false);

          auto argIter = callSite.arg_begin();
          argIter++;

          auto dstType = Type::getInt8PtrTy(F.getContext());

          Instruction *firstStoreInst = nullptr;
          Instruction *firstCastInst = nullptr;
          Instruction *firstPushInst = nullptr;

          for (; argIter != callSite.arg_end(); argIter++) {

            auto argValue = argIter->get();

            auto insertBefore = firstStoreInst ? firstStoreInst : callInst;

            auto ptrInst =
                new AllocaInst(argValue->getType(), "", insertBefore);

            insertBefore = firstCastInst ? firstCastInst : callInst;

            auto storeInst =
                new StoreInst(argValue, ptrInst, false, insertBefore);

            firstStoreInst = firstStoreInst ? firstStoreInst : storeInst;

            insertBefore = firstPushInst ? firstPushInst : callInst;

            auto argAsBytePtrVal = CastInst::Create(Instruction::CastOps::BitCast, ptrInst,
                                 dstType, "", insertBefore);

            firstCastInst = firstCastInst ? firstCastInst : argAsBytePtrVal;


            Type *pArgType = argValue->getType();
            if (pArgType->isPointerTy()) {
              pArgType = dyn_cast<PointerType>(pArgType)->getElementType();
            }

            ConstantInt *argSizeInBytesVal = ConstantInt::get(
                M.getContext(), APInt(32, M.getDataLayout().getTypeAllocSize(pArgType)));

            NodeParamTypeCategory paramTypeCategory =
                GetNodeParamTypeCategory(M, pArgType);

            ConstantInt *paramTypeCategoryVal = ConstantInt::get(
                M.getContext(), APInt(32, uint32_t(paramTypeCategory)));

            auto pushInst = CallInst::Create(
                m_RpsNodeParamPushFunc,
                { argAsBytePtrVal, argSizeInBytesVal, paramTypeCategoryVal },
                "", callInst);

            firstPushInst = firstPushInst ? firstPushInst : pushInst;
          }

          auto loadModuleId = new LoadInst(m_ModuleIdGlobal, "", callInst);
          CallInst::Create(m_RpsNodeCallFunc, { loadModuleId, nodeDefIdxVal }, "",
                           callInst);

          callInsts.push_back(callInst);
        }
        else {
          ReplaceLibFunction(callInst);
        }
      }
    }

    for (auto &inst : callInsts) {
      inst->eraseFromParent();
    }

    return true;
  }

  int32_t getNodeDefIndex(Function *F) {
    int32_t funcIndex = -1;
    if (F->getArgumentList().size() > 0) {
      auto &argList = F->getArgumentList();
      auto arg = argList.begin();
      auto argType = arg->getType();

      if (argType->isPointerTy()) {
        auto argPtrType = dyn_cast<PointerType>(argType);
        auto elemType = dyn_cast<StructType>(argPtrType->getElementType());
        if (elemType) {
          auto name = elemType->getName();
          if (name == "struct.nodeidentifier") {
            auto funcName = F->getName();
            auto funcIndexIter = m_RpsNodeNameIndice.find(funcName);
            if (funcIndexIter == m_RpsNodeNameIndice.end()) {
              funcIndex = static_cast<int32_t>(m_RpsNodeNames.size());
              m_RpsNodeNameIndice.insert(
                  std::make_pair(funcName, m_RpsNodeNames.size()));
              m_RpsNodeNames.push_back(funcName);
            } else {
              funcIndex = funcIndexIter->second;
            }
          }
        }
      }
    }
    return funcIndex;
  }

  Constant *CreateGlobalStringPtr(Module &M, StringRef Str) {
    Constant *StrConstant = ConstantDataArray::getString(M.getContext(), Str);
    auto gv = new GlobalVariable(M, StrConstant->getType(), true,
                                 GlobalValue::PrivateLinkage, StrConstant);
    gv->setUnnamedAddr(true);
    Value *zero = ConstantInt::get(Type::getInt32Ty(M.getContext()), 0);
    Value *Args[] = {zero, zero};

    return ConstantExpr::getGetElementPtr(gv->getValueType(), gv, Args, true);
  }

  void WriteNodeTable(Module &M) {
    std::vector<Constant *> stringConstants(m_RpsNodeNames.size() + 1);

    auto stringType = llvm::Type::getInt8PtrTy(M.getContext());

    for (uint32_t i = 0; i < m_RpsNodeNames.size(); i++) {
      auto demangledName = DemangleNames(m_RpsNodeNames[i]);
      auto stringValue = CreateGlobalStringPtr(M, demangledName);
      stringConstants[i] = stringValue;
    }
    stringConstants.back() = ConstantPointerNull::get(stringType);

    auto stringArrayType = ArrayType::get(stringType, m_RpsNodeNames.size() + 1);

    auto arrayValue = ConstantArray::get(stringArrayType, stringConstants);

    auto nodedefNameArrayVar = dyn_cast<GlobalVariable>(M.getOrInsertGlobal(
        AddModuleNamePostfix("__rps_nodedefs", M),
        stringArrayType));
    nodedefNameArrayVar->setLinkage(GlobalVariable::ExternalLinkage);
    nodedefNameArrayVar->setAlignment(4);
    nodedefNameArrayVar->setInitializer(arrayValue);
    nodedefNameArrayVar->setConstant(true);
  }

  StringRef DemangleNames(const StringRef &name) {
    auto start = name.find_first_of('?');
    auto end = name.find_first_of('@');
    return name.substr(start + 1, end - start - 1);
  }

  void WriteExportEntries(Module &M) {

    std::vector<Function *> exportFuncs = {};

    for (auto &F : M) {
      if (!F.empty() && (F.getLinkage() == GlobalValue::ExternalLinkage)) {
        exportFuncs.push_back(&F);
      }
    }

    std::vector<Constant *> funcConstants((exportFuncs.size() << 1) + 1);

    auto funcPtrType = llvm::Type::getInt8PtrTy(M.getContext());
    auto funcPtrArrayType = ArrayType::get(funcPtrType, funcConstants.size());

    for (uint32_t i = 0; i < exportFuncs.size(); i++) {
      uint32_t idx = i << 1;

      funcConstants[idx] = ConstantExpr::getBitCast(exportFuncs[i], funcPtrType);
      funcConstants[idx + 1] =
          CreateGlobalStringPtr(M, DemangleNames(exportFuncs[i]->getName()));
    }
    funcConstants.back() = ConstantPointerNull::get(funcPtrType);

    auto arrayValue = ConstantArray::get(funcPtrArrayType, funcConstants);

    auto exportEntryArrayVar = dyn_cast<GlobalVariable>(M.getOrInsertGlobal(
        AddModuleNamePostfix("__rps_entries", M),
        funcPtrArrayType));

    exportEntryArrayVar->setLinkage(GlobalVariable::ExternalLinkage);
    exportEntryArrayVar->setAlignment(4);
    exportEntryArrayVar->setInitializer(arrayValue);
    exportEntryArrayVar->setConstant(true);
  }
};
} // namespace

char DxilToRps::ID = 0;

ModulePass *llvm::createDxilToRpsPass() { return new DxilToRps(); }

INITIALIZE_PASS(DxilToRps, "dxil-2-rps", "DXIL to RPS convertion", false, false)
