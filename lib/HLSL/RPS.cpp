// Copyright (c) 2020 Advanced Micro Devices, Inc. All rights reserved.

#include "dxc/HLSL/DxilGenerationPass.h"
#include "dxc/DXIL/DxilConstants.h"
#include "dxc/HLSL/HLModule.h"
#include "dxc/DXIL/DxilOperations.h"
#include "dxc/DXIL/DxilFunctionProps.h"
#include "dxc/DXIL/DxilModule.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/IR/CallSite.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/Pass.h"
#include "llvm/Support/Path.h"

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
    std::string name;
    uint32_t flags;
    SmallVector<uint32_t, 16> paramFlags;
  };

  std::vector<RpsNodeDefInfo> m_RpsNodeInfos = {};
  std::vector<Function *> m_RpsExportEntries = {};
  StringMap<int32_t> m_RpsNodeNameIndice = {};
  StringMap<Function *> m_RpsLibFuncs = {};
  GlobalVariable *m_ModuleIdGlobal = nullptr;
  llvm::FunctionType *m_RpsExportWrapperFuncType;
  std::string m_ModuleNameSimplified;

  enum class NodeParamTypeCategory {
    Resource,
    View,
    RawBytes,
  };

  std::string AddModuleNamePostfix(const char *prefix) {
    return (prefix + std::string(m_ModuleNameSimplified.empty() ? "" : "_") + m_ModuleNameSimplified);
  }

  bool runOnModule(Module &M) override {

    hlsl::DxilModule &DM = M.GetDxilModule();

    if (!DM.GetShaderModel()->IsRPS()) {
      return false;
    }

    m_ModuleNameSimplified = M.getName();
    if (!m_ModuleNameSimplified.empty()) {
      m_ModuleNameSimplified = llvm::sys::path::stem(m_ModuleNameSimplified);
    }

    InitializeBuiltinsForModule(M);

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

  void InitializeBuiltinsForModule(Module &M) {
    static const char *StrTableId = "__rps_string_table";
    auto StrTableGV = M.getGlobalVariable(StrTableId);
    if (StrTableGV) {
      std::string StrTableIdPostfixed = AddModuleNamePostfix(StrTableId);
      StrTableGV->setName(StrTableIdPostfixed);
    }

    auto voidType = Type::getVoidTy(M.getContext());
    auto intType = Type::getInt32Ty(M.getContext());
    auto nodeCallFunc =
        M.getOrInsertFunction("__rps_node_call", voidType, intType, intType,
                              intType, nullptr); // ModuleId, NodeId, NodeFlags
    m_RpsNodeCallFunc = dyn_cast<Function>(nodeCallFunc);
    m_RpsNodeCallFunc->setLinkage(GlobalValue::ExternalLinkage);

    auto int8PtrType = Type::getInt8PtrTy(M.getContext());
    auto paramPushFunc =
        M.getOrInsertFunction("__rps_param_push", voidType, int8PtrType,
                              intType, intType, intType, nullptr);
    m_RpsNodeParamPushFunc = dyn_cast<Function>(paramPushFunc);
    m_RpsNodeParamPushFunc->setLinkage(GlobalValue::ExternalLinkage);

    m_ModuleIdGlobal = dyn_cast<GlobalVariable>(
        M.getOrInsertGlobal(AddModuleNamePostfix("__rps_module_id"), intType));

    ConstantInt *constIntZero = ConstantInt::get(M.getContext(), APInt(32, 0));
    m_ModuleIdGlobal->setInitializer(constIntZero);
    m_ModuleIdGlobal->setLinkage(GlobalValue::ExternalLinkage);
    m_ModuleIdGlobal->setConstant(false);
    m_ModuleIdGlobal->setDLLStorageClass(
        llvm::GlobalValue::DLLExportStorageClass);

    m_RpsLibFuncs.insert(std::make_pair("describe_resource", nullptr));
    m_RpsLibFuncs.insert(std::make_pair("describe_view", nullptr));
    m_RpsLibFuncs.insert(std::make_pair("create_resource", nullptr));
    m_RpsLibFuncs.insert(std::make_pair("create_view", nullptr));
    m_RpsLibFuncs.insert(std::make_pair("clear_view", nullptr));
    m_RpsLibFuncs.insert(std::make_pair("__rps_set_resource_name", nullptr));

    SmallVector<Type *, 1> exportWrapperParamType;
    exportWrapperParamType.push_back(
        Type::getInt8PtrTy(M.getContext())->getPointerTo());
    m_RpsExportWrapperFuncType = FunctionType::get(
        llvm::Type::getVoidTy(M.getContext()), exportWrapperParamType, false);
  }

  void DemangleBuiltinFunctions(CallInst *C) {
    auto calleeF = C->getCalledFunction();
    auto demangledName = DemangleNames(calleeF->getName());
    auto libFuncIter = m_RpsLibFuncs.find(demangledName);

    if (libFuncIter != m_RpsLibFuncs.end()) {

      if (libFuncIter->second == nullptr) {
        auto newFuncName =
            ((demangledName.find_first_of("__rps_") == 0) ? "" : "__rps_") +
            demangledName;
        calleeF->setName(newFuncName);
        libFuncIter->second = calleeF;
      }
#if RPS_DEBUG_VERBOSE
      printf("\n        Replaced: %s => %s", demangledName.str().c_str(),
             C->getCalledFunction()->getName().str().c_str());
#endif
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
      auto argElementPtr = Builder.CreateGEP(srcArg, indexConst);

      Value *argTypedPtrValue = nullptr;

      if (dstArgTypes[i]->isPointerTy()) {
        argTypedPtrValue = Builder.CreateBitCast(argElementPtr, dstArgTypes[i]->getPointerTo());
      } else {
        auto argTypedPtrPtrValue = Builder.CreateBitCast(
            argElementPtr, dstArgTypes[i]->getPointerTo()->getPointerTo());
        argTypedPtrValue = Builder.CreateLoad(dstArgTypes[i]->getPointerTo(),
                                              argTypedPtrPtrValue);
      }

      dstArgValues[i] = Builder.CreateLoad(argTypedPtrValue);
    }

    // Call actual function
    Builder.CreateCall(&F, dstArgValues);

    assert(Type::getVoidTy(M.getContext()) == WrapperFn->getReturnType());
    Builder.CreateRetVoid();

    return WrapperFn;
  }

  bool runOnFunction(Module &M, Function &F) {

    hlsl::DxilModule &DM = M.GetDxilModule();

    SmallPtrSet<llvm::User *, 32> toRemove;

    bool bIsExportEntry = false;
    if (DM.HasDxilFunctionProps(&F)) {
      auto &FuncProp = DM.GetDxilFunctionProps(&F);
      if(FuncProp.IsRPS()) {
        bIsExportEntry = (FuncProp.ShaderProps.RPS.entryKind == hlsl::DXIL::RPS::EntryKind::ExportEntry);
      }
    }

    // Demangle rpsexport function
    if (bIsExportEntry) {
      F.setName(DemangleNames(F.getName()));
      m_RpsExportEntries.push_back(&F);
    }

    llvm::SmallVector<CallInst *, 32> callInstsToRemove;

    bool pendingAsyncNodeCall = false;

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
#if RPS_DEBUG_VERBOSE
        printf("\n    Callee %s", calleeName.data());
#endif

        auto demangledCalleeName = DemangleNames(calleeName);
        if (demangledCalleeName == "__rps_asyncmarker") {
          pendingAsyncNodeCall = true;
          toRemove.insert(callInst);
          continue;
        }

        // Check if it's a node call
        auto nodeDefIdx = getNodeDefIndex(DM, calleeF);

        if (nodeDefIdx >= 0) {
          ProcessNodeCall(M, F, nodeDefIdx, callSite, callInst,
                          pendingAsyncNodeCall, callInstsToRemove);
        }
        else {
          DemangleBuiltinFunctions(callInst);
        }
      }
    }

    for (auto &inst : callInstsToRemove) {
      inst->eraseFromParent();
    }

    while (!toRemove.empty()) {
      auto junk = *toRemove.begin();
      RemoveUserRecursive(junk, toRemove);
    }

    return true;
  }

  void RemoveUserRecursive(User *U, SmallPtrSet<llvm::User *, 32>& toRemove) {

    for (auto UU : U->users()) {
      RemoveUserRecursive(UU, toRemove);
    }
    auto pInst = dyn_cast<Instruction>(U);
    if (pInst) {
      pInst->eraseFromParent();
    } else {
      assert(false);
    }
    toRemove.erase(U);
  }

  void ProcessNodeCall(Module& M, Function &F, uint32_t nodeDefIdx, CallSite &callSite,
                       CallInst *callInst, bool &pendingAsyncNodeCall,
                       SmallVector<CallInst *, 32> &callInstsToRemove) {
    auto nodeDefIdxVal =
        ConstantInt::get(Type::getInt32Ty(F.getContext()), nodeDefIdx, false);

    auto argIter = callSite.arg_begin();

    // Check async compute hint
    auto &nodeDefInfo = m_RpsNodeInfos[nodeDefIdx];

    bool isAsyncNode = false;
    // TODO: Handle copy separately
    if (pendingAsyncNodeCall) {
      if ((nodeDefInfo.flags & RPS_COMMAND_TYPE_COMPUTE_BIT) ||
          (nodeDefInfo.flags & RPS_COMMAND_TYPE_COPY_BIT)) {
        isAsyncNode = pendingAsyncNodeCall;
      }
      pendingAsyncNodeCall = false;
    }

    static const unsigned RPS_COMMAND_ASYNC_COMPUTE = 1 << 1;

    auto nodeFlagsVal =
        ConstantInt::get(Type::getInt32Ty(F.getContext()),
                         (isAsyncNode ? RPS_COMMAND_ASYNC_COMPUTE : 0), false);

    argIter++;

    auto dstType = Type::getInt8PtrTy(F.getContext());

    Instruction *firstStoreInst = nullptr;
    Instruction *firstCastInst = nullptr;
    Instruction *firstPushInst = nullptr;

    uint32_t argIndex = 1;
    for (; argIter != callSite.arg_end(); argIter++, argIndex++) {

      auto argValue = argIter->get();

      auto insertBefore = firstStoreInst ? firstStoreInst : callInst;

      auto ptrInst = new AllocaInst(argValue->getType(), "", insertBefore);

      insertBefore = firstCastInst ? firstCastInst : callInst;

      auto storeInst = new StoreInst(argValue, ptrInst, false, insertBefore);

      firstStoreInst = firstStoreInst ? firstStoreInst : storeInst;

      insertBefore = firstPushInst ? firstPushInst : callInst;

      auto argAsBytePtrVal = CastInst::Create(
          Instruction::CastOps::BitCast, ptrInst, dstType, "", insertBefore);

      firstCastInst = firstCastInst ? firstCastInst : argAsBytePtrVal;

      Type *pArgType = argValue->getType();
      if (pArgType->isPointerTy()) {
        pArgType = dyn_cast<PointerType>(pArgType)->getElementType();
      }

      ConstantInt *argSizeInBytesVal = ConstantInt::get(
          M.getContext(),
          APInt(32, M.getDataLayout().getTypeAllocSize(pArgType)));

      NodeParamTypeCategory paramTypeCategory =
          GetNodeParamTypeCategory(M, pArgType);

      ConstantInt *paramTypeCategoryVal = ConstantInt::get(
          M.getContext(), APInt(32, uint32_t(paramTypeCategory)));

      ConstantInt *paramFlagsVal = ConstantInt::get(
          M.getContext(),
          APInt(32, uint32_t(nodeDefInfo.paramFlags[argIndex])));

      auto pushInst = CallInst::Create(m_RpsNodeParamPushFunc,
                                       {argAsBytePtrVal, argSizeInBytesVal,
                                        paramTypeCategoryVal, paramFlagsVal},
                                       "", callInst);

      firstPushInst = firstPushInst ? firstPushInst : pushInst;
    }

    auto loadModuleId = new LoadInst(m_ModuleIdGlobal, "", callInst);
    CallInst::Create(m_RpsNodeCallFunc,
                     {loadModuleId, nodeDefIdxVal, nodeFlagsVal}, "", callInst);

    callInstsToRemove.push_back(callInst);
  }

  int32_t getNodeDefIndex(hlsl::DxilModule &DM, Function *F) {

    auto &DMTypeSystem = DM.GetTypeSystem();

    int32_t funcIndex = -1;
    if (F->getArgumentList().size() > 0) {
      auto &argList = F->getArgumentList();
      auto arg = argList.begin();
      auto argType = arg->getType();

      static const uint32_t entryKindToFlags[] = {
        RPS_COMMAND_TYPE_NO_FLAGS,
        RPS_COMMAND_TYPE_NO_FLAGS,
        RPS_COMMAND_TYPE_GRAPHICS_BIT,
        RPS_COMMAND_TYPE_COMPUTE_BIT,
        RPS_COMMAND_TYPE_COPY_BIT,
      };

      static_assert(sizeof(entryKindToFlags) / sizeof(entryKindToFlags[0]) == uint32_t(hlsl::DXIL::RPS::EntryKind::MaxValue),
          "Mismatching entryKindToFlags and hlsl::DXIL::RPS::EntryKind");

      bool isNodeDef = false;
      uint32_t nodeFlags = RPS_COMMAND_TYPE_NO_FLAGS;
      if (DM.HasDxilFunctionProps(F)) {
        auto &FuncProp = DM.GetDxilFunctionProps(F);
        if (FuncProp.IsRPS()) {
          nodeFlags =
              entryKindToFlags[uint32_t(FuncProp.ShaderProps.RPS.entryKind)];
          isNodeDef = true;
        }
      }

      if (argType->isPointerTy()) {
        auto argPtrType = dyn_cast<PointerType>(argType);
        auto elemType = dyn_cast<StructType>(argPtrType->getElementType());
        if (elemType) {
          auto name = elemType->getName();

          if (isNodeDef) {
            auto funcName = F->getName();

            if (name != "struct.nodeidentifier") {
              // TODO: Report error
              return -1;
            }

            auto funcIndexIter = m_RpsNodeNameIndice.find(funcName);
            if (funcIndexIter == m_RpsNodeNameIndice.end()) {
              funcIndex = static_cast<int32_t>(m_RpsNodeInfos.size());
              m_RpsNodeNameIndice.insert(
                  std::make_pair(funcName, m_RpsNodeInfos.size()));

              m_RpsNodeInfos.emplace_back();
              m_RpsNodeInfos.back().name = funcName;
              m_RpsNodeInfos.back().flags = nodeFlags;

              auto pFuncAnnotation = DMTypeSystem.GetFunctionAnnotation(F);
              if (pFuncAnnotation) {
                m_RpsNodeInfos.back().paramFlags.resize(pFuncAnnotation->GetNumParameters(), 0);

                for (uint32_t iArg = 0; iArg < pFuncAnnotation->GetNumParameters(); iArg++) {
                  auto &argAnnotation = pFuncAnnotation->GetParameterAnnotation(iArg);
                  m_RpsNodeInfos.back().paramFlags[iArg] = argAnnotation.GetRPSAccessFlags();
                }
              }
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
        AddModuleNamePostfix("__rps_nodedefs"),
        nodeDefArrayType));
    nodedefNameArrayVar->setLinkage(GlobalVariable::ExternalLinkage);
    nodedefNameArrayVar->setAlignment(4);
    nodedefNameArrayVar->setInitializer(arrayValue);
    nodedefNameArrayVar->setConstant(true);
    nodedefNameArrayVar->setDLLStorageClass(llvm::GlobalValue::DLLExportStorageClass);
  }

  std::string DemangleNames(const StringRef &name) {
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
        AddModuleNamePostfix("__rps_entries"),
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
