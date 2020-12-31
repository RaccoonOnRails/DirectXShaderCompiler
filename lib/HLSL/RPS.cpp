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
#include "llvm/IR/DiagnosticInfo.h"
#include "llvm/IR/DiagnosticPrinter.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/Pass.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

#define RPS_DEBUG_VERBOSE 0
#define DEBUG_TYPE "rps"

namespace {

class RpsGenDiagnosticInfo : public DiagnosticInfo {
  std::string m_Msg;
public:
  RpsGenDiagnosticInfo(const char *msg, DiagnosticSeverity Severity)
      : DiagnosticInfo(GetDiagnosticKind(), Severity), m_Msg(msg) {}

  virtual void print(DiagnosticPrinter &DP) const override { DP << m_Msg; }
private:
  static int GetDiagnosticKind() {
    static const int s_DiagnosticKind = getNextAvailablePluginDiagnosticKind();
    return s_DiagnosticKind;
  }
};

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

  enum class RpsBasicType : uint8_t
  {
    RPS_TYPE_UNKNOWN,
    RPS_TYPE_BOOL,
    RPS_TYPE_SINT,
    RPS_TYPE_UINT,
    RPS_TYPE_FLOAT,
    RPS_TYPE_STRUCT,
    RPS_TYPE_VIEW,
    RPS_TYPE_RESOURCE,
  };

  struct RpsTypeInfo {
    RpsBasicType elementType;
    uint8_t elementBitWidth;
    uint8_t rows;
    uint8_t columns;
    uint32_t arrayElementsOrPtr;
    uint32_t sizeInBytes;
    uint32_t alignment;

    bool operator==(const RpsTypeInfo& rhs) const {
      return (elementType == rhs.elementType) && 
             (elementBitWidth == rhs.elementBitWidth) &&
             (rows == rhs.rows) &&
             (columns == rhs.columns) &&
             (arrayElementsOrPtr == rhs.arrayElementsOrPtr) &&
             (sizeInBytes == rhs.sizeInBytes) &&
             (alignment == rhs.alignment);
    }
  };

  struct RpsTypeInfoHasher {
    size_t operator()(const RpsTypeInfo &val) const {
      return (uint32_t(val.elementType) | (uint32_t(val.elementBitWidth) << 8) |
              (uint32_t(val.rows) << 16) | (uint32_t(val.columns) << 24)) ^
             val.arrayElementsOrPtr ^ (uint32_t(val.sizeInBytes) << 16) ^
             (val.alignment << 24);
    }
  };

  struct RpsNodeParamInfo {
    std::string name;
    std::string typeName;
    std::string semanticName;
    uint32_t semanticIndex;
    uint32_t typeInfoIndex;
    uint32_t flags;
    uint16_t alignedSizeInBytes;
    uint16_t offsetInBytes;

    RpsNodeParamInfo()
        : semanticIndex(0), typeInfoIndex(UINT32_MAX),
          flags(0), alignedSizeInBytes(0), offsetInBytes(0) {}
  };

  struct RpsNodeDefInfo {
    std::string name;
    uint32_t flags;
    SmallVector<RpsNodeParamInfo, 16> paramInfos;
  };

  std::vector<RpsNodeDefInfo> m_RpsNodeInfos = {};
  std::vector<Function *> m_RpsExportEntries = {};
  StringMap<int32_t> m_RpsNodeNameIndice = {};
  StringMap<Function *> m_RpsLibFuncs = {};
  GlobalVariable *m_ModuleIdGlobal = nullptr;
  llvm::FunctionType *m_RpsExportWrapperFuncType;
  std::string m_ModuleNameSimplified;
  std::vector<char> m_StrTableValueString;
  std::unordered_map<std::string, uint32_t> m_StrTableEntries;
  std::vector<RpsTypeInfo> m_RpsTypes;
  std::unordered_map<RpsTypeInfo, uint32_t, RpsTypeInfoHasher> m_RpsTypesLookup;

  std::string AddModuleNamePostfix(const char *prefix) {
    return (prefix + std::string(m_ModuleNameSimplified.empty() ? "" : "_") + m_ModuleNameSimplified);
  }

  bool runOnModule(Module &M) override {

    hlsl::HLModule &DM = M.GetHLModule();

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

    EmitStringTableGlobal(M);

    return true;
  }

  void EmitStringTableGlobal(Module& M) {
    static const char *StrTableId = "__rps_string_table";

    std::string StrTableIdPostfixed = AddModuleNamePostfix(StrTableId);

    if (!m_StrTableValueString.empty()) {
      Constant *StrConstant = ConstantDataArray::getString(
          M.getContext(),
          StringRef(m_StrTableValueString.data(), m_StrTableValueString.size()),
          false);
      GlobalVariable *RpsResourceNameTableGV = dyn_cast<GlobalVariable>(
          M.getOrInsertGlobal(StrTableIdPostfixed, StrConstant->getType()));

      RpsResourceNameTableGV->setLinkage(GlobalVariable::ExternalLinkage);
      RpsResourceNameTableGV->setAlignment(4);
      RpsResourceNameTableGV->setInitializer(StrConstant);
      RpsResourceNameTableGV->setConstant(true);
      RpsResourceNameTableGV->setDLLStorageClass(
          llvm::GlobalValue::DLLExportStorageClass);
    }
  }

  void InitializeBuiltinsForModule(Module &M) {

    m_StrTableValueString = M.GetHLModule().TakeRPSStringTable();

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
        if (dstArgTypes[i]->getPointerElementType()->getStructName() == "struct.resource") {
          // TODO: Temporary solution to translate ResourceIndex to handle
          // Load resource index value then OR RPS_SHADER_HANDLE_TYPE_RESOURCE_BIT.
          // Shouldn't need this after external resource refactoring & resource/view/null built-in type work.
          static const uint32_t RPS_SHADER_HANDLE_TYPE_RESOURCE_BIT = 0x40000000;
          auto int32Ty = Type::getInt32Ty(M.getContext());
          auto int32PtrTy = int32Ty->getPointerTo();
          auto asInt32Ptr = Builder.CreateBitCast(argElementPtr, int32PtrTy->getPointerTo());
          auto resourceIndexPtrVal = Builder.CreateLoad(asInt32Ptr);
          auto resourceIndexVal = Builder.CreateLoad(resourceIndexPtrVal);
          auto resourceHdlVal = Builder.CreateOr(resourceIndexVal, RPS_SHADER_HANDLE_TYPE_RESOURCE_BIT);
          auto resourceHdlValPtr = Builder.CreateAlloca(resourceHdlVal->getType());
          auto resourceHdlPtr = Builder.CreateBitCast(resourceHdlValPtr, dstArgTypes[i]);
          Builder.CreateStore(resourceHdlVal, resourceHdlValPtr);
          argTypedPtrValue = Builder.CreateAlloca(dstArgTypes[i]);
          Builder.CreateStore(resourceHdlPtr, argTypedPtrValue);
        } else {
          argTypedPtrValue = Builder.CreateBitCast(
              argElementPtr, dstArgTypes[i]->getPointerTo());
        }
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

    hlsl::HLModule &DM = M.GetHLModule();

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
        auto nodeDefIdx = getNodeDefIndex(M, DM, calleeF);

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

    uint32_t argIndex = 0;
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

      const auto &paramInfo = nodeDefInfo.paramInfos[argIndex];
      auto &typeInfo = m_RpsTypes[paramInfo.typeInfoIndex];

      ConstantInt *argSizeInBytesVal = ConstantInt::get(
          M.getContext(), APInt(32, uint64_t(paramInfo.alignedSizeInBytes)));

      static const uint32_t indirectionFlag = 0x80000000;

      // Is array or struct pointer
      bool isIndirection =
          ((typeInfo.arrayElementsOrPtr != 0) &&
           (typeInfo.arrayElementsOrPtr != UINT32_MAX)) ||
          (typeInfo.elementType == RpsBasicType::RPS_TYPE_STRUCT) ||
          (typeInfo.elementType == RpsBasicType::RPS_TYPE_RESOURCE) ||
          (typeInfo.elementType == RpsBasicType::RPS_TYPE_VIEW);

      ConstantInt *paramTypeBaseVal = ConstantInt::get(
          M.getContext(), APInt(32,
              uint32_t(m_RpsTypes[paramInfo.typeInfoIndex].elementType) |
              (isIndirection ? indirectionFlag : 0)));

      ConstantInt *paramFlagsVal = ConstantInt::get(
          M.getContext(), APInt(32, uint32_t(paramInfo.flags)));

      auto pushInst = CallInst::Create(m_RpsNodeParamPushFunc,
          { argAsBytePtrVal, argSizeInBytesVal, paramTypeBaseVal, paramFlagsVal },
          "", callInst);

      firstPushInst = firstPushInst ? firstPushInst : pushInst;
    }

    auto loadModuleId = new LoadInst(m_ModuleIdGlobal, "", callInst);
    CallInst::Create(m_RpsNodeCallFunc,
                     {loadModuleId, nodeDefIdxVal, nodeFlagsVal}, "", callInst);

    callInstsToRemove.push_back(callInst);
  }

  int32_t getNodeDefIndex(Module& M, hlsl::HLModule &DM, Function *F) {

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
              auto& newNode = m_RpsNodeInfos.back();
              
              newNode.name = funcName;
              newNode.flags = nodeFlags;

              auto pFuncAnnotation = DMTypeSystem.GetFunctionAnnotation(F);
              if (pFuncAnnotation) {
                assert(F->arg_size() == pFuncAnnotation->GetNumParameters());

                auto argIter = F->arg_begin();
                assert(argIter->hasStructRetAttr());

                ++argIter;

                uint32_t numParams = pFuncAnnotation->GetNumParameters() - 1;
                newNode.paramInfos.resize(numParams);

                uint32_t paramOffset = 0;

                for (uint32_t iArg = 0; iArg < numParams; iArg++, ++argIter) {
                  auto &argAnnotation = pFuncAnnotation->GetParameterAnnotation(iArg + 1);
                  auto& currParam = newNode.paramInfos[iArg];
                  currParam.flags = argAnnotation.GetRPSAccessFlags();
                  currParam.name = argAnnotation.GetFieldName();
                  currParam.typeName = argAnnotation.GetTypeName();
                  ResolveParamTypeInfo(currParam, M, argIter->getType(), argAnnotation);
                  currParam.semanticName = argAnnotation.GetSemanticString();
                  currParam.semanticIndex =
                      argAnnotation.GetSemanticIndexVec().empty()
                          ? 0
                          : argAnnotation.GetSemanticIndexVec()[0];

                  const uint32_t paramSizeInBytes =
                      m_RpsTypes[currParam.typeInfoIndex].sizeInBytes;
                  assert(paramSizeInBytes < UINT16_MAX);

                  currParam.offsetInBytes = paramOffset;

                  paramOffset += paramSizeInBytes;
                  // TODO: Per-param alignment.
                  paramOffset = uint32_t(
                      alignAddr(reinterpret_cast<const void *>(static_cast<uintptr_t>(paramOffset)), 4));

                  assert(paramOffset <= UINT16_MAX);

                  currParam.alignedSizeInBytes = paramOffset - currParam.offsetInBytes;
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

  RpsBasicType HLSLCompTypeToRPSType(hlsl::DXIL::ComponentType compType,
                                     uint8_t &elementBitWidth) const {
    switch (compType) {
    case hlsl::DXIL::ComponentType::I1:       elementBitWidth = 8; return RpsBasicType::RPS_TYPE_BOOL;
    case hlsl::DXIL::ComponentType::I16:      elementBitWidth = 16; return RpsBasicType::RPS_TYPE_SINT;
    case hlsl::DXIL::ComponentType::I32:      elementBitWidth = 32; return RpsBasicType::RPS_TYPE_SINT;
    case hlsl::DXIL::ComponentType::I64:      elementBitWidth = 64; return RpsBasicType::RPS_TYPE_SINT;
    case hlsl::DXIL::ComponentType::U16:      elementBitWidth = 16; return RpsBasicType::RPS_TYPE_UINT;
    case hlsl::DXIL::ComponentType::U32:      elementBitWidth = 32; return RpsBasicType::RPS_TYPE_UINT;
    case hlsl::DXIL::ComponentType::U64:      elementBitWidth = 64; return RpsBasicType::RPS_TYPE_UINT;
    case hlsl::DXIL::ComponentType::F16:      elementBitWidth = 16; return RpsBasicType::RPS_TYPE_FLOAT;
    case hlsl::DXIL::ComponentType::F32:      elementBitWidth = 32; return RpsBasicType::RPS_TYPE_FLOAT;
    case hlsl::DXIL::ComponentType::F64:      elementBitWidth = 64; return RpsBasicType::RPS_TYPE_FLOAT;
    case hlsl::DXIL::ComponentType::SNormF16:  __fallthrough;
    case hlsl::DXIL::ComponentType::UNormF16:  __fallthrough;
    case hlsl::DXIL::ComponentType::SNormF32:  __fallthrough;
    case hlsl::DXIL::ComponentType::UNormF32:  __fallthrough;
    case hlsl::DXIL::ComponentType::SNormF64:  __fallthrough;
    case hlsl::DXIL::ComponentType::UNormF64:  __fallthrough;
    default:
      assert(false && "invalid type kind");
    }
    return RpsBasicType::RPS_TYPE_UNKNOWN;
  }

  void ResolveParamTypeInfo(RpsNodeParamInfo &paramInfo, Module &M, Type *pType,
                            const hlsl::DxilParameterAnnotation &annotation) {

    RpsTypeInfo typeInfo = {};

    // Pointer
    if (pType->isPointerTy()) {
      assert(typeInfo.arrayElementsOrPtr == 0);
      typeInfo.arrayElementsOrPtr = UINT32_MAX;

      pType = pType->getPointerElementType();
    }

    typeInfo.sizeInBytes = M.getDataLayout().getTypeAllocSize(pType);
    typeInfo.alignment = std::max(M.getDataLayout().getABITypeAlignment(pType), 4u);

    // Array - Only support 1d for now. Nd array are expanded to 1d here.
    uint32_t arrayDim = 0;
    uint32_t numElements = 1;
    while (pType->isArrayTy()) {
      numElements *= pType->getArrayNumElements();
      arrayDim++;

      pType = pType->getArrayElementType();
    }
    typeInfo.arrayElementsOrPtr = (arrayDim > 0) ? numElements : 0;

    // Array of pointers are not supported.
    assert(!pType->isPointerTy());

    // Matrix / Vector
    if (annotation.HasMatrixAnnotation()) {
      typeInfo.rows = annotation.GetMatrixAnnotation().Rows;
      typeInfo.columns = annotation.GetMatrixAnnotation().Cols;
    }
    else if (pType->isVectorTy()) {
      typeInfo.columns = pType->getVectorNumElements();
      typeInfo.rows = 0;

      pType = pType->getScalarType();
    }

    // Struct
    if (pType->isStructTy()) {
      auto name = pType->getStructName();
      if ((name == "struct.srv") || (name == "struct.rtv") ||
          (name == "struct.uav") || (name == "struct.view")) {
        typeInfo.elementType = RpsBasicType::RPS_TYPE_VIEW;
      } else if (name == "struct.resource") {
        typeInfo.elementType = RpsBasicType::RPS_TYPE_RESOURCE;
      } else {
        typeInfo.elementType = RpsBasicType::RPS_TYPE_STRUCT;
      }
    }

    // Component
    if (annotation.HasCompType()) {
      typeInfo.elementType =
          HLSLCompTypeToRPSType(annotation.GetCompType().GetKind(),
                                typeInfo.elementBitWidth);
    }

    auto existing = m_RpsTypesLookup.find(typeInfo);
    if (existing == m_RpsTypesLookup.end()) {
      paramInfo.typeInfoIndex = m_RpsTypes.size();
      m_RpsTypes.emplace_back(typeInfo);
      m_RpsTypesLookup[typeInfo] = paramInfo.typeInfoIndex;
    } else {
      paramInfo.typeInfoIndex = existing->second;
    }
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

  uint32_t AppendToModuleStringTable(StringRef s) {
    auto existing = m_StrTableEntries.find(s);
    if (existing != m_StrTableEntries.end()) {
      return existing->second;
    }
    uint32_t offs = uint32_t(m_StrTableValueString.size());
    m_StrTableValueString.insert(m_StrTableValueString.end(), s.data(),
                                 s.data() + s.size());
    m_StrTableValueString.push_back('\0');

    m_StrTableEntries[s] = offs;
    return offs;
  }

  void WriteNodeTable(Module &M) {
    // Metadata layout:
    //
    // struct RpsTypeInfo {
    //   RpsBasicType elementType;
    //   uint8_t elementBitWidth;
    //   uint8_t rows;
    //   uint8_t columns;
    //   uint32_t arrayElementsOrPtr;
    //   uint32_t sizeInBytes;
    //   uint32_t alignment;
    // };
    // struct RpsNodeParamInfo {
    //   uint32_t nameOffset;
    //   uint32_t semanticName;
    //   uint32_t semanticIndex;
    //   uint32_t typeInfoIndex;
    //   uint16_t alignedSizeInBytes;
    //   uint16_t byteOffset;
    //   uint32_t flags;
    // };
    // struct NodeDef {
    //     uint32_t ordinal;
    //     uint32_t flags;
    //     uint32_t nameOffset;
    //     uint32_t paramsOffsetAndCount;
    // };

    static const uint32_t TYPE_INFO_SIZE_IN_DWORDS = 4;
    static const uint32_t PARAM_METADATA_SIZE_IN_DWORDS = 5;
    static const uint32_t NODE_METADATA_SIZE_IN_DWORDS = 4;

    auto nodeMetadataSizeInDWs =
        (m_RpsNodeInfos.size() + 1) * NODE_METADATA_SIZE_IN_DWORDS;

    SmallVector<uint32_t, 256> nodeDefMetadataValues;
    nodeDefMetadataValues.reserve(nodeMetadataSizeInDWs);

    SmallVector<uint32_t, 256> paramMetadataValues;
    uint32_t totalParamsCount = 0;

    SmallVector<uint32_t, 256> typeInfoValues;
    typeInfoValues.reserve(m_RpsTypes.size() * TYPE_INFO_SIZE_IN_DWORDS);

    for (uint32_t iT = 0; iT < m_RpsTypes.size(); iT++) {
      auto &typeInfo = m_RpsTypes[iT];
      typeInfoValues.append({
        (uint32_t(typeInfo.elementType) |
         (uint32_t(typeInfo.elementBitWidth) << 8) |
         (uint32_t(typeInfo.rows) << 16) |
         (uint32_t(typeInfo.columns) << 24)),
        typeInfo.arrayElementsOrPtr,
        typeInfo.sizeInBytes,
        typeInfo.alignment,
      });
    }

    for (uint32_t iN = 0; iN < m_RpsNodeInfos.size(); iN++) {
      auto &nodeInfo = m_RpsNodeInfos[iN];
      auto demangledName = DemangleNames(m_RpsNodeInfos[iN].name);
      auto strOffs = AppendToModuleStringTable(demangledName);
      auto paramsOffsetAndCount =
          (uint32_t(m_RpsNodeInfos[iN].paramInfos.size()) << 16) |
          totalParamsCount;
      nodeDefMetadataValues.append({
          iN,
          m_RpsNodeInfos[iN].flags,
          strOffs,
          paramsOffsetAndCount,
      });

      for (uint32_t iP =0; iP < nodeInfo.paramInfos.size(); iP++) {
        auto &paramInfo = nodeInfo.paramInfos[iP];
        const uint32_t nameOffset = AppendToModuleStringTable(paramInfo.name);
        const uint32_t semanticNameOffset = AppendToModuleStringTable(paramInfo.semanticName);

        paramMetadataValues.append({
            nameOffset,
            semanticNameOffset,
            paramInfo.semanticIndex,
            paramInfo.typeInfoIndex,
            uint32_t(paramInfo.offsetInBytes) | (uint32_t(paramInfo.alignedSizeInBytes) << 16u),
            paramInfo.flags,
        });
      }

      totalParamsCount += m_RpsNodeInfos[iN].paramInfos.size();
    }
    nodeDefMetadataValues.append({ 0xffffffffU, 0, 0, 0, });
    paramMetadataValues.append({ 0xffffffffU, 0, 0, 0, 0, });
    typeInfoValues.append({ 0xffffffffU, 0, 0, 0, });

    auto nodeDefMetadataCArr = ConstantDataArray::get(M.getContext(), nodeDefMetadataValues);
    auto nodeDefArrayGV = dyn_cast<GlobalVariable>(
        M.getOrInsertGlobal(AddModuleNamePostfix("__rps_nodedefs"),
                            nodeDefMetadataCArr->getType()));
    nodeDefArrayGV->setLinkage(GlobalVariable::ExternalLinkage);
    nodeDefArrayGV->setAlignment(4);
    nodeDefArrayGV->setInitializer(nodeDefMetadataCArr);
    nodeDefArrayGV->setConstant(true);
    nodeDefArrayGV->setDLLStorageClass(llvm::GlobalValue::DLLExportStorageClass);

    auto paramsMetadataCArr = ConstantDataArray::get(M.getContext(), paramMetadataValues);
    auto paramsMetadataGV = dyn_cast<GlobalVariable>(
        M.getOrInsertGlobal(AddModuleNamePostfix("__rps_node_params_metadata"),
                            paramsMetadataCArr->getType()));
    paramsMetadataGV->setLinkage(GlobalVariable::ExternalLinkage);
    paramsMetadataGV->setAlignment(4);
    paramsMetadataGV->setInitializer(paramsMetadataCArr);
    paramsMetadataGV->setConstant(true);
    paramsMetadataGV->setDLLStorageClass(llvm::GlobalValue::DLLExportStorageClass);

    auto typeInfoCArr = ConstantDataArray::get(M.getContext(), typeInfoValues);
    auto typeInfoGV = dyn_cast<GlobalVariable>(
        M.getOrInsertGlobal(AddModuleNamePostfix("__rps_types_metadata"),
                            typeInfoCArr->getType()));
    typeInfoGV->setLinkage(GlobalVariable::ExternalLinkage);
    typeInfoGV->setAlignment(4);
    typeInfoGV->setInitializer(typeInfoCArr);
    typeInfoGV->setConstant(true);
    typeInfoGV->setDLLStorageClass(llvm::GlobalValue::DLLExportStorageClass);
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
