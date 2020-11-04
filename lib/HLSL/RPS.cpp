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
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/Pass.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

#define RPS_DEBUG_VERBOSE 0
#define DEBUG_TYPE "rps"

namespace {
// Rps - The second implementation with getAnalysisUsage implemented.
struct DxilToRps : public ModulePass {
  static char ID; // Pass identification, replacement for typeid
  DxilToRps() : ModulePass(ID) {}

  Function *m_RpsNodeCallFunc = nullptr;
  Function *m_RpsNodeParamPushFunc = nullptr;

  enum RpsCommandTypeFlagBits {
    RPS_COMMAND_TYPE_NO_FLAGS                   = 0,            ///< No flags.
    RPS_COMMAND_TYPE_GRAPHICS_BIT               = (1 << 1),     ///< The command is graphics only.
    RPS_COMMAND_TYPE_COMPUTE_BIT                = (1 << 2),     ///< The command is compute only.
    RPS_COMMAND_TYPE_COPY_BIT                   = (1 << 3),     ///< The command is copy only.
    RPS_COMMAND_TYPE_RESOLVE_BIT                = (1 << 4),     ///< The command is resolve only.
    RPS_COMMAND_TYPE_CALLBACK_BIT               = (1 << 5),     ///< The command is a callback, no setup.
    RPS_COMMAND_TYPE_CMD_BUF_BIT                = (1 << 6),     ///< The command submits prebuilt cmd bufs.
    RPS_COMMAND_TYPE_PREFER_ASYNC_BIT           = (1 << 7),     ///< The command type prefers to run on an async queue.
    RPS_COMMAND_TYPE_CUSTOM_VIEWPORT_BIT        = (1 << 8),     ///< The callback will set a custom viewport & scissor.
    RPS_COMMAND_TYPE_EXTERNAL_WRITE_BIT         = (1 << 9),     ///< Command does some external write, such as updating debug CPU data.
  };

  struct RpsNodeDefInfo {
    StringRef name;
    uint32_t flags;
  };

  std::vector<RpsNodeDefInfo> m_RpsNodeInfos = {};
  std::vector<Function *> m_RpsExportEntries = {};
  StringMap<int32_t> m_RpsNodeNameIndice = {};
  StringMap<Function *> m_RpsLibFuncs = {};
  GlobalVariable *m_ModuleIdGlobal = nullptr;
  llvm::FunctionType *m_RpsExportWrapperFuncType;

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
    m_ModuleIdGlobal->setDLLStorageClass(llvm::GlobalValue::DLLExportStorageClass);

    m_RpsLibFuncs.insert(std::make_pair("describe_resource", nullptr));
    m_RpsLibFuncs.insert(std::make_pair("describe_view", nullptr));
    m_RpsLibFuncs.insert(std::make_pair("create_resource", nullptr));
    m_RpsLibFuncs.insert(std::make_pair("create_view", nullptr));
    m_RpsLibFuncs.insert(std::make_pair("clear_view", nullptr));

    SmallVector<Type *, 1> exportWrapperParamType;
    exportWrapperParamType.push_back(
        Type::getInt8PtrTy(M.getContext())->getPointerTo());
    m_RpsExportWrapperFuncType = FunctionType::get(
        llvm::Type::getVoidTy(M.getContext()), exportWrapperParamType, false);

    for (auto &F : M) {
#if RPS_DEBUG_VERBOSE
      printf("\nFunction %s", F.getName().data());
#endif
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

  Function* EmitRpsExportWrapperFunction(Module &M, Function &F) {
    Function *WrapperFn = Function::Create(
        m_RpsExportWrapperFuncType,
        llvm::GlobalValue::LinkageTypes::ExternalLinkage, "", &M);

    // Already demangled
    WrapperFn->setName(F.getName() + "_wrapper");

    WrapperFn->addFnAttr(Attribute::NoInline);
    WrapperFn->addFnAttr(Attribute::NoUnwind);

    BasicBlock *WrapperFnBody =
        BasicBlock::Create(M.getContext(), "body", WrapperFn);

    SmallVector<llvm::Type *, 16> dstArgTypes;
    SmallVector<llvm::Value *, 16> dstArgValues;

    for (auto &dstArg : F.getArgumentList()) {
      dstArgTypes.push_back(dstArg.getType());
    }

    IRBuilder<> Builder(WrapperFnBody);

    // Input argument (pointer array)
    llvm::Value *srcArg = WrapperFn->getArgumentList().begin();

    dstArgValues.resize(dstArgTypes.size(), nullptr);

#if 0
    // Create temp local for sreturn
    llvm::Type *sretType = cast<PointerType>(dstArgTypes[0])->getElementType();
    llvm::Value *sretValue = Builder.CreateAlloca(sretType);

    dstArgValues[0] = sretValue;

    // Deference args from pointer array
    for (unsigned i = 1; i < dstArgTypes.size(); i++) {
      auto indexConst = ConstantInt::get(M.getContext(), APInt(32, i - 1));
#else
    // Deference args from pointer array
    for (unsigned i = 0; i < dstArgTypes.size(); i++) {
      auto indexConst = ConstantInt::get(M.getContext(), APInt(32, i));
#endif
      auto argPtrValue = Builder.CreateGEP(srcArg, indexConst);
      auto argTypedPtrValue =
          Builder.CreateBitCast(argPtrValue, dstArgTypes[i]->getPointerTo());

      dstArgValues[i] = Builder.CreateLoad(argTypedPtrValue);
    }

    // Call actual function
    Builder.CreateCall(&F, dstArgValues);

    assert(Type::getVoidTy(M.getContext()) == WrapperFn->getReturnType());
    Builder.CreateRetVoid();

    return WrapperFn;
  }

  bool runOnFunction(Module &M, Function &F) {

#if 0
    auto attrs = F.getAttributes();
    printf("Attrs: %s", attrs.getAsString(AttributeSet::FunctionIndex).c_str());
#endif

    bool bIsExportEntry = false;

#if 0 // TODO: dummy return rpsexportidentifier breaks control flow atm.
    // Check for rpsexport entry functions
    if (F.getReturnType()->isVoidTy() && (F.arg_size() > 1) &&
        F.hasStructRetAttr()) {
      auto firstArg = F.arg_begin();
      if (firstArg->getType()->isPointerTy()) {
        auto pPtrType = dyn_cast<PointerType>(firstArg->getType());
        if (pPtrType->getElementType()->isStructTy() &&
            (pPtrType->getElementType()->getStructName() ==
             "struct.rpsexportidentifier")) {
          bIsExportEntry = true;
        }
      }
    }
#else
    if (F.hasFnAttribute(llvm::Attribute::AttrKind::NoInline)) {
      bIsExportEntry = true;
    }
#endif

    // Demangle rpsexport function
    if (bIsExportEntry) {
      F.setName(DemangleNames(F.getName()));
      m_RpsExportEntries.push_back(&F);
    }

    llvm::SmallVector<CallInst *, 32> callInsts;

    // Go through the function,
    // replace raw node function calls with __rps_param_push / __rps_node_call
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

      static const struct {
        const char *name;
        unsigned flags;
      } nodeIdentifiers[] = {
        { "struct.nodeidentifier", RPS_COMMAND_TYPE_NO_FLAGS },
        { "struct.gfxnodeidentifier", RPS_COMMAND_TYPE_GRAPHICS_BIT },
        { "struct.compnodeidentifier", RPS_COMMAND_TYPE_COMPUTE_BIT },
        { "struct.copynodeidentifier", RPS_COMMAND_TYPE_COPY_BIT },
      };

      if (argType->isPointerTy()) {
        auto argPtrType = dyn_cast<PointerType>(argType);
        auto elemType = dyn_cast<StructType>(argPtrType->getElementType());
        if (elemType) {
          auto name = elemType->getName();

          unsigned iNodedef = 0;
          for (; iNodedef < _countof(nodeIdentifiers); iNodedef++) {
            if (name == nodeIdentifiers[iNodedef].name) {
              break;
            }
          }

          if (iNodedef < _countof(nodeIdentifiers)) {
            auto funcName = F->getName();
            auto funcIndexIter = m_RpsNodeNameIndice.find(funcName);
            if (funcIndexIter == m_RpsNodeNameIndice.end()) {
              funcIndex = static_cast<int32_t>(m_RpsNodeInfos.size());
              m_RpsNodeNameIndice.insert(
                  std::make_pair(funcName, m_RpsNodeInfos.size()));
              m_RpsNodeInfos.emplace_back();
              m_RpsNodeInfos.back().name = funcName;
              m_RpsNodeInfos.back().flags = nodeIdentifiers[iNodedef].flags;
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
    std::vector<Constant *> nodeDefConstants((m_RpsNodeInfos.size() << 1) + 1);

    auto bytePtrType = llvm::Type::getInt8PtrTy(M.getContext());
    auto nodeDefArrayType = ArrayType::get(bytePtrType, nodeDefConstants.size());

    auto intType = llvm::Type::getInt32Ty(M.getContext());

    for (uint32_t i = 0; i < m_RpsNodeInfos.size(); i++) {
      uint32_t idx = i << 1;

      auto demangledName = DemangleNames(m_RpsNodeInfos[i].name);
      auto stringValue = CreateGlobalStringPtr(M, demangledName);

      auto nodeFlagConstant =
          llvm::ConstantInt::get(intType, uint64_t(m_RpsNodeInfos[i].flags));

      nodeDefConstants[idx] = stringValue;
      nodeDefConstants[idx + 1] =
          ConstantExpr::getIntToPtr(nodeFlagConstant, bytePtrType);
    }
    nodeDefConstants.back() = ConstantPointerNull::get(bytePtrType);

    auto arrayValue = ConstantArray::get(nodeDefArrayType, nodeDefConstants);

    auto nodedefNameArrayVar = dyn_cast<GlobalVariable>(M.getOrInsertGlobal(
        AddModuleNamePostfix("__rps_nodedefs", M),
        nodeDefArrayType));
    nodedefNameArrayVar->setLinkage(GlobalVariable::ExternalLinkage);
    nodedefNameArrayVar->setAlignment(4);
    nodedefNameArrayVar->setInitializer(arrayValue);
    nodedefNameArrayVar->setConstant(true);
    nodedefNameArrayVar->setDLLStorageClass(llvm::GlobalValue::DLLExportStorageClass);
  }

  StringRef DemangleNames(const StringRef &name) {
    auto start = name.find_first_of('?');
    auto end = name.find_first_of('@');
    return name.substr(start + 1, end - start - 1);
  }

  void WriteExportEntries(Module &M) {
    std::vector<Constant *> funcConstants((m_RpsExportEntries.size() * 3) + 1);

    auto funcPtrType = llvm::Type::getInt8PtrTy(M.getContext());
    auto funcPtrArrayType = ArrayType::get(funcPtrType, funcConstants.size());

    for (uint32_t i = 0; i < m_RpsExportEntries.size(); i++) {

      auto expFn = m_RpsExportEntries[i];
      auto wrapperFn = EmitRpsExportWrapperFunction(M, *expFn);

      uint32_t idx = i * 3;

      funcConstants[idx] = ConstantExpr::getBitCast(expFn, funcPtrType);
      funcConstants[idx + 1] = ConstantExpr::getBitCast(wrapperFn, funcPtrType);
      funcConstants[idx + 2] = CreateGlobalStringPtr(M, expFn->getName());
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
    exportEntryArrayVar->setDLLStorageClass(llvm::GlobalValue::DLLExportStorageClass);
  }
};
} // namespace

char DxilToRps::ID = 0;

ModulePass *llvm::createDxilToRpsPass() { return new DxilToRps(); }

INITIALIZE_PASS(DxilToRps, "dxil-2-rps", "DXIL to RPS convertion", false, false)
