// Copyright (c) 2020 Advanced Micro Devices, Inc. All rights reserved.

#define NOMINMAX
#include <Windows.h>
#include <Shlobj.h>
#include <algorithm>
#include <dxcapi.h>
#include <sstream>
#include <string>
#include <vector>
#include <unordered_map>
#include <functional>
#include <wrl/client.h>
#include "dxc/Support/Global.h"
#include <llvm/Support/Path.h>
#include <llvm/Support/CommandLine.h>
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/MSFileSystem.h"
#include "llvm/Support/raw_ostream.h"

#define RPS_ENABLE_DEBUG_INFO 1
#define RPS_DEBUG_AST_DUMP 0

using namespace llvm;

static cl::opt<std::string> InputFilename(cl::Positional,
                                          cl::desc("<input .rpsl file>"),
                                          cl::init("-"));

static cl::opt<std::string> OutputModuleName("m",
                                             cl::desc("Override output module name"),
                                             cl::init("-"));

static cl::opt<std::string> OutputDirectory("od",
                                            cl::desc("Override output directory"),
                                            cl::init("."));

static cl::opt<std::string> TempDirectory("td",
                                          cl::desc("Override intermediate directory for temporary files"),
                                          cl::init("."));

static cl::opt<bool> OutputCbe("cbe",
                               cl::desc("Output C file using LLVM C backend"),
                               cl::init(true));

static cl::opt<bool> OutputObj("obj",
                               cl::desc("Output obj file compiled with llc"),
                               cl::init(false));

static cl::opt<bool> OutputAsm("asm",
                               cl::desc("Output asm file compiled with llc"),
                               cl::init(false));

static cl::opt<bool> DumpOriginalILBin("dxil_bin",
                                       cl::desc("Dump original DXIL blob (DXC output)"),
                                       cl::init(false));

static cl::opt<bool> DumpRpsILBin("rpsll_bin",
                                  cl::desc("Dump processed LLVM IL blob (RPS transform pass output)"),
                                  cl::init(false));

static cl::opt<bool> DumpOriginalIL("dxil",
                                    cl::desc("Dump original DXIL text (DXC output)"),
                                    cl::init(false));

static cl::opt<bool> DumpRpsIL("rpsll",
                               cl::desc("Dump processed LLVM IL text (RPS transform pass output)"),
                               cl::init(false));

static cl::opt<bool> DumpCFG("dot-cfg", cl::desc("Dump CFG"), cl::init(false));

static std::string OutputFileStem;
static std::string OutputFileDirectoryAndStem;

// Loader
HMODULE g_DxcDll = {};
DxcCreateInstanceProc g_pfnDxcCreateInstance = {};

bool LoadDxc() {
  if (g_DxcDll == nullptr) {
    g_DxcDll = LoadLibrary(TEXT("dxcompiler.dll"));

    if (g_DxcDll) {
      g_pfnDxcCreateInstance =
          (DxcCreateInstanceProc)GetProcAddress(g_DxcDll, "DxcCreateInstance");
    }
  }

  return (g_pfnDxcCreateInstance != nullptr);
}

using Microsoft::WRL::ComPtr;
using namespace std::string_literals;

void ExitIfFailed(HRESULT hr, const char* msg) {
  if (FAILED(hr)) {
    llvm::errs() << msg;
    exit(hr);
  }
}

#define RPS_HEADER_NAME "__rps_builtin_header_.rpsl"

static const char c_RpsHeaderIncludeString[] = "#include \"" RPS_HEADER_NAME "\"\n";

static const char c_RpsHeader[] = R"(
struct nodeidentifier { uint unused; };
#define node nodeidentifier

uint __rps_asyncmarker();
#define async __rps_asyncmarker();

// Syntax sugars
#define rtv         [readwrite(rendertarget)] texture
#define discard_rtv [writeonly(rendertarget)] texture
#define srv         [readonly(ps, cs)] texture
#define srv_buf     [readonly(ps, cs)] buffer
#define ps_srv      [readonly(ps)] texture
#define ps_srv_buf  [readonly(ps)] buffer
#define dsv         [readwrite(depth, stencil)] texture
#define uav         [readwrite(ps, cs)] texture
#define uav_buf     [readwrite(ps, cs)] buffer

#define __RPS_BEGIN_DECL_ENUM(X) namespace rps { enum X {
#define __RPS_ENUM_VALUE(N, V) N = (V),
#define __RPS_END_DECL_ENUM() }; }

uint __rps_set_resource_name(uint h, uint nameOffset, uint nameLength);

ResourceDesc     describe_resource      ( texture t ) { return t.desc(); }
ResourceDesc     describe_resource      ( buffer b  ) { return b.desc(); }
ResourceDesc     describe_texture       ( texture t ) { return t.desc(); }
ResourceDesc     describe_buffer        ( buffer b  ) { return b.desc(); }
TextureViewDesc  describe_view          ( texture t ) { return t.view_desc(); }
BufferViewDesc   describe_view          ( buffer b  ) { return b.view_desc(); }

texture          create_texture         ( ResourceDesc desc );
texture          view_texture           ( texture t, TextureViewDesc desc );
buffer           create_buffer          ( ResourceDesc desc );
buffer           view_buffer            ( buffer b, BufferViewDesc desc );

void             clear_texture          ( texture t, RPS_CLEAR_FLAGS option, uint4 data );
void             clear_buffer           ( buffer b, RPS_CLEAR_FLAGS option, uint4 data );

inline texture create_tex1d(
    RPS_FORMAT format,
    uint width,
    uint numMips = 1,
    uint arraySlices = 1,
    uint numTemporalLayers = 1,
    RPS_RESOURCE_FLAGS flags = RPS_RESOURCE_FLAG_NONE )
{
    ResourceDesc desc;
    desc.Dimension = RPS_RESOURCE_TEX1D;
    desc.Flags = flags;
    desc.Format = format;
    desc.Width = width;
    desc.Height = 1;
    desc.DepthOrArraySize = arraySlices;
    desc.MipLevels = numMips;
    desc.SampleCount = 1;
    desc.SampleQuality = 0;
    desc.TemporalLayers = numTemporalLayers;

    return create_texture(desc);
}

inline texture create_tex2d(
    RPS_FORMAT format,
    uint width,
    uint height,
    uint numMips = 1,
    uint arraySlices = 1,
    uint numTemporalLayers = 1,
    uint sampleCount = 1,
    uint sampleQuality = 0,
    RPS_RESOURCE_FLAGS flags = RPS_RESOURCE_FLAG_NONE )
{
    ResourceDesc desc;
    desc.Dimension = RPS_RESOURCE_TEX2D;
    desc.Flags = flags;
    desc.Format = format;
    desc.Width = width;
    desc.Height = height;
    desc.DepthOrArraySize = arraySlices;
    desc.MipLevels = numMips;
    desc.SampleCount = sampleCount;
    desc.SampleQuality = sampleQuality;
    desc.TemporalLayers = numTemporalLayers;

    return create_texture(desc);
}

inline texture create_tex3d(
    RPS_FORMAT format,
    uint width,
    uint height,
    uint depth,
    uint numMips = 1,
    uint numTemporalLayers = 1,
    RPS_RESOURCE_FLAGS flags = RPS_RESOURCE_FLAG_NONE )
{
    ResourceDesc desc;
    desc.Dimension = RPS_RESOURCE_TEX3D;
    desc.Flags = flags;
    desc.Format = format;
    desc.Width = width;
    desc.Height = height;
    desc.DepthOrArraySize = depth;
    desc.MipLevels = numMips;
    desc.SampleCount = 1;
    desc.SampleQuality = 0;
    desc.TemporalLayers = numTemporalLayers;

    return create_texture(desc);
}

inline buffer create_buffer( uint64_t width, uint numTemporalLayers = 1, RPS_RESOURCE_FLAGS flags = RPS_RESOURCE_FLAG_NONE )
{
    ResourceDesc desc;
    desc.Dimension = RPS_RESOURCE_BUFFER;
    desc.Flags = flags;
    desc.Format = RPS_FORMAT_UNKNOWN;
    desc.Width = width;
    desc.Height = 1;
    desc.DepthOrArraySize = 1;
    desc.MipLevels = 1;
    desc.SampleCount = 1;
    desc.SampleQuality = 0;
    desc.TemporalLayers = numTemporalLayers;

    return create_buffer(desc);
}

inline SubResourceRange make_texture_range( uint baseMip, uint mipLevels = 1, uint baseArraySlice = 0, uint numArraySlices = 1, uint planeMask = 1, uint temporalLayer = 0 )
{
    SubResourceRange range;

    range.BaseMip        = baseMip;
    range.MipLevels      = mipLevels;
    range.BaseArraySlice = baseArraySlice;
    range.ArrayLayers    = numArraySlices;
    range.PlaneMask      = planeMask;
    range.TemporalLayer  = temporalLayer;

    return range;
}

inline texture create_texture_view(
    texture r,
    uint baseMip = 0,
    uint mipLevels = 1,
    uint baseArraySlice = 0,
    uint numArraySlices = 1,
    uint planeMask = 1,
    uint temporalLayer = 0,
    RPS_FORMAT format = RPS_FORMAT_UNKNOWN )
{
    TextureViewDesc desc;
    desc.Range = make_texture_range( baseMip, mipLevels, baseArraySlice, numArraySlices, planeMask, temporalLayer );
    desc.Format = format;

    return view_texture( r, desc );
}

inline buffer create_buffer_view(
    buffer r,
    uint64_t offset = 0,
    uint64_t sizeInBytes = 0,
    uint temporalLayer = 0,
    RPS_FORMAT format = RPS_FORMAT_UNKNOWN,
    uint structureStride = 0 )
{
    BufferViewDesc desc;

    desc.Offset        = offset;
    desc.SizeInBytes   = sizeInBytes;
    desc.Format        = format;
    desc.TemporalLayer = temporalLayer;
    desc.StructureByteStride = structureStride;

    return view_buffer( r, desc );
}

inline void clear( texture d, float4 val )
{
    return clear_texture( d, RPS_CLEAR_COLOR, asuint(val) );
}

inline void clear( texture d, uint4 val )
{
    return clear_texture( d, RPS_CLEAR_COLOR, val );
}

inline void clear( texture d, float depth, uint stencil )
{
    return clear_texture( d, RPS_CLEAR_DEPTHSTENCIL, uint4(asuint(depth), stencil, 0, 0) );
}

inline void clear_depth( texture d, float depth )
{
    return clear_texture( d, RPS_CLEAR_DEPTH, uint4(asuint(depth), 0, 0, 0) );
}

inline void clear_stencil( texture d, uint stencil )
{
    return clear_texture( d, RPS_CLEAR_STENCIL, uint4(0, stencil, 0, 0) );
}

inline void clear( buffer d, float4 val )
{
    return clear_buffer( d, RPS_CLEAR_UAV_FLOAT, asuint(val) );
}

inline void clear( buffer d, uint4 val )
{
    return clear_buffer( d, RPS_CLEAR_UAV_UINT, asuint(val) );
}
)";

class RpsHlslcIncludeHandler : public IDxcIncludeHandler {
private:
  std::atomic<ULONG> m_RefCount;
  IDxcLibrary *m_pLib;
  std::unordered_map<std::wstring, ComPtr<IDxcBlob>> m_Buffers;

public:
  RpsHlslcIncludeHandler(IDxcLibrary *pLib) : m_RefCount(1u), m_pLib(pLib) {}

  virtual HRESULT STDMETHODCALLTYPE
  LoadSource(LPCWSTR pFilename, IDxcBlob **ppIncludeSource) override final {
    HRESULT result = S_OK;

    std::wstring fullFileName = pFilename;

    size_t nConv = 0;
    char fillFileNameA[MAX_PATH + 1];
    wcstombs_s(&nConv, fillFileNameA, pFilename, _countof(fillFileNameA) - 1);

    auto existingEntry = m_Buffers.find(fullFileName);
    if (existingEntry != m_Buffers.end()) {
      existingEntry->second->AddRef();
      *ppIncludeSource = existingEntry->second.Get();
      return result;
    }

    UINT codePage = CP_ACP;
    ComPtr<IDxcBlobEncoding> pBlobEncoding;

    auto fileNameA = llvm::sys::path::filename(fillFileNameA);
    if (fileNameA == RPS_HEADER_NAME) {
      result = m_pLib->CreateBlobWithEncodingFromPinned(
          c_RpsHeader, _countof(c_RpsHeader), CP_ACP, &pBlobEncoding);
    } else {
      result = m_pLib->CreateBlobFromFile(pFilename, &codePage, &pBlobEncoding);
    }

    if (SUCCEEDED(result)) {
      m_Buffers.insert(std::make_pair(fullFileName, pBlobEncoding));
      pBlobEncoding->AddRef();
      *ppIncludeSource = pBlobEncoding.Get();
    }

    return result;
  }

  // We are creating this on stack only, skip proper COM shenanigans for now.
  virtual HRESULT STDMETHODCALLTYPE QueryInterface(
      REFIID riid,
      _COM_Outptr_ void __RPC_FAR *__RPC_FAR *ppvObject) override final {
    if (riid == __uuidof(IDxcIncludeHandler)) {
      *ppvObject = this;
      return S_OK;
    } else if (riid == __uuidof(IUnknown)) {
      *ppvObject = this;
      return S_OK;
    }
    return E_NOINTERFACE;
  }

  virtual ULONG STDMETHODCALLTYPE AddRef(void) override final {
    return (++m_RefCount);
  }

  virtual ULONG STDMETHODCALLTYPE Release(void) override final {
    return (--m_RefCount);
  }
};

ComPtr<IDxcBlob> CompileHlslToDxilContainer(const char *fileName) {
  ComPtr<IDxcLibrary> pLibrary;
  ExitIfFailed(
      g_pfnDxcCreateInstance(CLSID_DxcLibrary, IID_PPV_ARGS(&pLibrary)), "Failed to create IDxcLibrary instance.");

  FILE *fp = {};
  fopen_s(&fp, fileName, "rb");
  if (!fp) {
    llvm::errs() << "\nFailed to open file '" << fileName << "'";
    return nullptr;
  }

  fseek(fp, 0, SEEK_END);
  auto fileLen = ftell(fp);
  fseek(fp, 0, SEEK_SET);

  std::vector<char> text;
  text.resize(_countof(c_RpsHeaderIncludeString) + fileLen);

  memcpy(&text[0], c_RpsHeaderIncludeString, _countof(c_RpsHeaderIncludeString) - 1);
  fread(&text[_countof(c_RpsHeaderIncludeString) - 1], 1, fileLen, fp);
  fclose(fp);

  ComPtr<IDxcBlobEncoding> pSource, pError;
  ExitIfFailed(pLibrary->CreateBlobWithEncodingFromPinned(
      text.data(), static_cast<UINT32>(text.size()), CP_UTF8, &pSource),
      "Failed to create source blob.");

  ComPtr<IDxcCompiler> pCompiler;
  ExitIfFailed(
      g_pfnDxcCreateInstance(CLSID_DxcCompiler, IID_PPV_ARGS(&pCompiler)),
      "Failed to create IDxcCompiler instance.");

  size_t nConv = 0;
  wchar_t fileNameW[MAX_PATH + 1];
  mbstowcs_s(&nConv, fileNameW, fileName, _countof(fileNameW) - 1);

  std::vector<LPCWSTR> arguments;

  arguments.push_back(L"-Vd");

  arguments.push_back(L"-default-linkage");
  arguments.push_back(L"external");

  arguments.push_back(L"-Zi");
  arguments.push_back(L"-res_may_alias");

  arguments.push_back(L"-Qembed_debug");

  arguments.push_back(L"-Od");

#if RPS_DEBUG_AST_DUMP
  arguments.push_back(L"-ast-dump");
#endif //RPS_DEBUG_AST_DUMP

  RpsHlslcIncludeHandler includeHandler(pLibrary.Get());

  ComPtr<IDxcOperationResult> pResult;
  HRESULT hr = pCompiler->Compile(
      pSource.Get(), fileNameW, L"",
      L"rps_6_0",
      arguments.data(),
      static_cast<UINT>(arguments.size()), nullptr, 0,
      &includeHandler, &pResult);

  if (pResult) {
    pResult->GetErrorBuffer(&pError);
  }

  if (pError) {
    ComPtr<IDxcBlobEncoding> pErrorUtf8;
    pLibrary->GetBlobAsUtf8(pError.Get(), &pErrorUtf8);
    std::string errText(
        reinterpret_cast<char *>(pErrorUtf8->GetBufferPointer()),
        pErrorUtf8->GetBufferSize());
    llvm::errs() << "\n" << errText;
  }

  if (FAILED(hr)) {
    llvm::errs() << "\nFailed to compile '" << fileName << "'";
    return nullptr;
  }

  ComPtr<IDxcBlob> pContainer;
  pResult->GetResult(&pContainer);

  if (pContainer) {

#if RPS_DEBUG_AST_DUMP
    fp = {};
    fopen_s(&fp, (std::string(fileName) + ".ast.txt").c_str(), "wb");
    if (fp) {
      fwrite(pContainer->GetBufferPointer(), 1, pContainer->GetBufferSize(), fp);
    }
    fclose(fp);
    return nullptr;
#endif // RPS_DEBUG_AST_DUMP

    if (DumpCFG) {
      ComPtr<IDxcOptimizer> pOptimizer;
      if (SUCCEEDED(g_pfnDxcCreateInstance(CLSID_DxcOptimizer,
                                           IID_PPV_ARGS(&pOptimizer)))) {

        std::vector<LPCWSTR> optArgs;
        optArgs.push_back(L"-dot-cfg");

        ComPtr<IDxcBlob> pOptimizerIn, pOptimizerOut;

        ComPtr<IDxcContainerReflection> pReflection;
        if (SUCCEEDED(g_pfnDxcCreateInstance(CLSID_DxcContainerReflection,
                                             IID_PPV_ARGS(&pReflection)))) {
          if (SUCCEEDED(pReflection->Load(pContainer.Get()))) {
            UINT32 numParts = 0;
            if (SUCCEEDED(pReflection->GetPartCount(&numParts))) {
              for (UINT iPart = 0; iPart < numParts; iPart++) {
                UINT kind;
                if (SUCCEEDED(pReflection->GetPartKind(iPart, &kind)) &&
                    (kind == 'LIXD')) {
                  pReflection->GetPartContent(iPart, &pOptimizerIn);
                  break;
                }
              }
            }
          }
        }

        if (pOptimizerIn) {
          ComPtr<IDxcBlobEncoding> pOptimizerOutText;
          pOptimizer->RunOptimizer(pOptimizerIn.Get(), optArgs.data(),
                                   UINT32(optArgs.size()), &pOptimizerOut,
                                   &pOptimizerOutText);

          if (pOptimizerOutText) {
            ComPtr<IDxcBlobEncoding> pCfgDumpUtf8;
            pLibrary->GetBlobAsUtf8(pOptimizerOutText.Get(), &pCfgDumpUtf8);

            fp = {};
            fopen_s(&fp, (std::string(fileName) + ".cfg.dot").c_str(), "wb");
            if (fp) {
              fwrite(pCfgDumpUtf8->GetBufferPointer(), 1,
                     pCfgDumpUtf8->GetBufferSize(), fp);
            }
            fclose(fp);
          }
        }
      }
    }

    if (DumpOriginalIL || DumpOriginalILBin) {
      ComPtr<IDxcBlobEncoding> pDisasm;
      ExitIfFailed(pCompiler->Disassemble(pContainer.Get(), &pDisasm),
                   "Failed to disassemble container.");

      if (DumpOriginalIL) {
        fp = {};
        fopen_s(&fp, (std::string(fileName) + ".dxil.txt").c_str(), "wb");
        if (fp) {
          fwrite(pDisasm->GetBufferPointer(), 1, pDisasm->GetBufferSize(), fp);
        }
        fclose(fp);
      }

      if (DumpOriginalILBin) {
        fp = {};
        fopen_s(&fp, (std::string(fileName) + ".dxil.blob").c_str(), "wb");
        if (fp) {
          fwrite(pContainer->GetBufferPointer(), 1, pContainer->GetBufferSize(),
                 fp);
        }
        fclose(fp);
      }
    }
  }

  return pContainer;
}

void DisassembleRps(const ComPtr<IDxcBlob> &pRpsBC, const char* tmpFileName) {
  ComPtr<IDxcCompiler> pCompiler;

  ExitIfFailed(
      g_pfnDxcCreateInstance(CLSID_DxcCompiler, IID_PPV_ARGS(&pCompiler)),
      "Failed to create IDxcCompiler instance for disassembling.");

  ComPtr<IDxcBlobEncoding> pDisasm;
  ExitIfFailed(pCompiler->Disassemble(pRpsBC.Get(), &pDisasm),
      "Failed to disassemble container.");

  if (pDisasm) {
    FILE *fp = {};
    fopen_s(&fp, tmpFileName, "wb");
    if (fp) {
      fwrite(pDisasm->GetBufferPointer(), 1, pDisasm->GetBufferSize(), fp);
      fclose(fp);
    }
  }
}

int main(const int argc, const char *argv[]) {
  int result = 0;

  DxcInitThreadMalloc();
  DxcSetThreadMallocToDefault();
  if (::llvm::sys::fs::SetupPerThreadFileSystem()) {
    return -1;
  }

  ::llvm::sys::fs::MSFileSystem *msfPtr;
  IFT(CreateMSFileSystemForDisk(&msfPtr));
  std::unique_ptr<::llvm::sys::fs::MSFileSystem> msf(msfPtr);
  ::llvm::sys::fs::AutoPerThreadSystem pts(msf.get());

  // Parse command line options.
  cl::ParseCommandLineOptions(argc, argv, "dxil assembly\n");

  char currExecPath[MAX_PATH];
  ::GetModuleFileNameA(nullptr, currExecPath, _countof(currExecPath));
  auto currExecDir = llvm::sys::path::parent_path(currExecPath).str();

  SHCreateDirectoryExA(NULL, OutputDirectory.c_str(), NULL);

  llvm::errs() << "\nUsing rps-hlslc.exe in " << currExecDir;

  if (OutputModuleName != "-") {
    OutputFileStem = OutputModuleName;
  } else if (llvm::sys::path::has_stem(InputFilename)) {
    OutputFileStem = llvm::sys::path::stem(InputFilename).str();
  } else {
    OutputFileStem = "rps";
  }

  // Force module name if using cbe for static linking
  if ((OutputModuleName == "-") && OutputCbe) {
    OutputModuleName = OutputFileStem;
  }

  OutputFileDirectoryAndStem = OutputDirectory + "/" + OutputFileStem;

  LoadDxc();

  auto pCode = CompileHlslToDxilContainer(InputFilename.c_str());
  if (!pCode) {
    result = -1;
    return result;
  }

  auto tmpRpsLLFile = OutputDirectory + "/" + OutputFileStem + ".tmp.rps.ll";

  DisassembleRps(pCode, tmpRpsLLFile.c_str());

#if 0 // TODO : non-cbe code generators
  if (OutputObj.getValue()) {
    system((currExecDir + "/llc.exe -filetype=obj -mtriple=x86_64-pc-win32 " + tmpRpsLLFile)
               .c_str());
  }
  if (OutputAsm.getValue()) {
    system((currExecDir + "/llc.exe -filetype=asm -mtriple=x86_64-pc-win32 "
            "--x86-asm-syntax=intel " +
            tmpRpsLLFile)
               .c_str());
  }
#endif

  if (llvm::errs().has_error()) {
    llvm::errs().flush();
    llvm::errs().clear_error();
  }

  if (OutputCbe.getValue()) {
    std::string cbeCmd = "\"\"" + currExecDir + "/llvm-cbe.exe\" \"" +
                         tmpRpsLLFile + "\" -O0 -o \"" +
                         OutputFileDirectoryAndStem + ".rpsl.g.c\"\"";
    result = system(cbeCmd.c_str());
  }

  return result;
}
