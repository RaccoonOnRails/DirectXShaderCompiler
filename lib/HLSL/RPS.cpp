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

  bool runOnModule(Module &M) override {
    auto voidType = Type::getVoidTy(M.getContext());
    auto nodeCallFunc =
        M.getOrInsertFunction("__rps_node_call", voidType, nullptr);
    m_RpsNodeCallFunc = dyn_cast<Function>(nodeCallFunc);
    m_RpsNodeCallFunc->setLinkage(GlobalValue::ExternalLinkage);

    auto argType = Type::getInt8PtrTy(M.getContext());
    auto paramPushFunc =
        M.getOrInsertFunction("__rps_param_push", voidType, argType, nullptr);
    m_RpsNodeParamPushFunc = dyn_cast<Function>(paramPushFunc);
    m_RpsNodeParamPushFunc->setLinkage(GlobalValue::ExternalLinkage);

    for (auto &F : M) {
      printf("\nFunction %s", F.getName().data());
      runOnFunction(F);
    }

    WriteNodeTable(M);

    WriteExportEntries(M);

    return true;
  }

  bool runOnFunction(Function &F) {

    llvm::SmallVector<CallInst *, 32> callInsts;

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


            auto pushInst = CallInst::Create(m_RpsNodeParamPushFunc, { argAsBytePtrVal }, "", callInst);

            firstPushInst = firstPushInst ? firstPushInst : pushInst;
          }

          auto nodeDefIndxAsPtrVal = ConstantExpr::getIntToPtr(
              nodeDefIdxVal, Type::getInt8PtrTy(F.getContext()));
          auto pushNodeIdInst =
              CallInst::Create(m_RpsNodeParamPushFunc, {nodeDefIndxAsPtrVal});
          pushNodeIdInst->insertBefore(callInst);

          auto replaceCallInst = CallInst::Create(m_RpsNodeCallFunc);
          replaceCallInst->insertBefore(callInst);

          callInsts.push_back(callInst);
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
      auto stringValue = CreateGlobalStringPtr(M, m_RpsNodeNames[i]);
      stringConstants[i] = stringValue;
    }
    stringConstants.back() = ConstantPointerNull::get(stringType);

    auto stringArrayType = ArrayType::get(stringType, m_RpsNodeNames.size() + 1);

    auto arrayValue = ConstantArray::get(stringArrayType, stringConstants);

    auto nodedefNameArrayVar = dyn_cast<GlobalVariable>(
        M.getOrInsertGlobal("__rps_nodedefs", stringArrayType));
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

    auto exportEntryArrayVar = dyn_cast<GlobalVariable>(
        M.getOrInsertGlobal("__rps_entries", funcPtrArrayType));
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