#define NOMINMAX
#include <Windows.h>
#include <Shlobj.h>
#include <algorithm>
#include <dxcapi.h>
#include <sstream>
#include <string>
#include <vector>
#include <functional>
#include <wrl/client.h>
#include <llvm/Support/Path.h>
#include <llvm/Support/CommandLine.h>

#define RPS_ENABLE_DEBUG_INFO 1

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

void ThrowIfFailed(HRESULT hr) {
  if (FAILED(hr)) {
    throw;
  }
}

static const char c_RpsHeader[] = R"(
struct nodeidentifier { uint unused; };
struct gfxnodeidentifier : nodeidentifier {};
struct compnodeidentifier : nodeidentifier {};
struct copynodeidentifier : nodeidentifier {};

#define node [noinline] nodeidentifier
#define graphics_node [noinline] gfxnodeidentifier
#define compute_node [noinline] compnodeidentifier
#define copy_node [noinline] copynodeidentifier

// typedef void rpsexportidentifier { uint unused; };
#define rps_export [noinline] // rpsexportidentifier // TODO: dummy return rpsexportidentifier breaks control flow atm.

#define __RPS_DECL_HANDLE(X) struct X { uint _value; };
__RPS_DECL_HANDLE(resource);
__RPS_DECL_HANDLE(view);
__RPS_DECL_HANDLE(rtv);
__RPS_DECL_HANDLE(dsv);
__RPS_DECL_HANDLE(srv);
__RPS_DECL_HANDLE(uav);

#define __RPS_BEGIN_DECL_ENUM(X) struct X { uint _value; static struct X or(struct X a, struct X b) { struct X x; x._value = a._value | b._value; return x; } }; namespace rps { namespace X { typedef struct X __RPS_ENUM_TYPE;
#define __RPS_ENUM_VALUE(N, V) static const __RPS_ENUM_TYPE N = { V };
#define __RPS_END_DECL_ENUM() } }

__RPS_BEGIN_DECL_ENUM(resourcetype)
    __RPS_ENUM_VALUE(buffer, 0)
    __RPS_ENUM_VALUE(tex1d, 1)
    __RPS_ENUM_VALUE(tex2d, 2)
    __RPS_ENUM_VALUE(tex3d, 3)
__RPS_END_DECL_ENUM()

__RPS_BEGIN_DECL_ENUM(format)
    __RPS_ENUM_VALUE(unknown                  , 0)
    __RPS_ENUM_VALUE(r32g32b32a32_typeless    , 1)
    __RPS_ENUM_VALUE(r32g32b32a32_float       , 2)
    __RPS_ENUM_VALUE(r32g32b32a32_uint        , 3)
    __RPS_ENUM_VALUE(r32g32b32a32_sint        , 4)
    __RPS_ENUM_VALUE(r32g32b32_typeless       , 5)
    __RPS_ENUM_VALUE(r32g32b32_float          , 6)
    __RPS_ENUM_VALUE(r32g32b32_uint           , 7)
    __RPS_ENUM_VALUE(r32g32b32_sint           , 8)
    __RPS_ENUM_VALUE(r16g16b16a16_typeless    , 9)
    __RPS_ENUM_VALUE(r16g16b16a16_float       , 10)
    __RPS_ENUM_VALUE(r16g16b16a16_unorm       , 11)
    __RPS_ENUM_VALUE(r16g16b16a16_uint        , 12)
    __RPS_ENUM_VALUE(r16g16b16a16_snorm       , 13)
    __RPS_ENUM_VALUE(r16g16b16a16_sint        , 14)
    __RPS_ENUM_VALUE(r32g32_typeless          , 15)
    __RPS_ENUM_VALUE(r32g32_float             , 16)
    __RPS_ENUM_VALUE(r32g32_uint              , 17)
    __RPS_ENUM_VALUE(r32g32_sint              , 18)
    __RPS_ENUM_VALUE(r32g8x24_typeless        , 19)
    __RPS_ENUM_VALUE(d32_float_s8x24_uint     , 20)
    __RPS_ENUM_VALUE(r32_float_x8x24_typeless , 21)
    __RPS_ENUM_VALUE(x32_typeless_g8x24_uint  , 22)
    __RPS_ENUM_VALUE(r10g10b10a2_typeless     , 23)
    __RPS_ENUM_VALUE(r10g10b10a2_unorm        , 24)
    __RPS_ENUM_VALUE(r10g10b10a2_uint         , 25)
    __RPS_ENUM_VALUE(r11g11b10_float          , 26)
    __RPS_ENUM_VALUE(r8g8b8a8_typeless        , 27)
    __RPS_ENUM_VALUE(r8g8b8a8_unorm           , 28)
    __RPS_ENUM_VALUE(r8g8b8a8_unorm_srgb      , 29)
    __RPS_ENUM_VALUE(r8g8b8a8_uint            , 30)
    __RPS_ENUM_VALUE(r8g8b8a8_snorm           , 31)
    __RPS_ENUM_VALUE(r8g8b8a8_sint            , 32)
    __RPS_ENUM_VALUE(r16g16_typeless          , 33)
    __RPS_ENUM_VALUE(r16g16_float             , 34)
    __RPS_ENUM_VALUE(r16g16_unorm             , 35)
    __RPS_ENUM_VALUE(r16g16_uint              , 36)
    __RPS_ENUM_VALUE(r16g16_snorm             , 37)
    __RPS_ENUM_VALUE(r16g16_sint              , 38)
    __RPS_ENUM_VALUE(r32_typeless             , 39)
    __RPS_ENUM_VALUE(d32_float                , 40)
    __RPS_ENUM_VALUE(r32_float                , 41)
    __RPS_ENUM_VALUE(r32_uint                 , 42)
    __RPS_ENUM_VALUE(r32_sint                 , 43)
    __RPS_ENUM_VALUE(r24g8_typeless           , 44)
    __RPS_ENUM_VALUE(d24_unorm_s8_uint        , 45)
    __RPS_ENUM_VALUE(r24_unorm_x8_typeless    , 46)
    __RPS_ENUM_VALUE(x24_typeless_g8_uint     , 47)
    __RPS_ENUM_VALUE(r8g8_typeless            , 48)
    __RPS_ENUM_VALUE(r8g8_unorm               , 49)
    __RPS_ENUM_VALUE(r8g8_uint                , 50)
    __RPS_ENUM_VALUE(r8g8_snorm               , 51)
    __RPS_ENUM_VALUE(r8g8_sint                , 52)
    __RPS_ENUM_VALUE(r16_typeless             , 53)
    __RPS_ENUM_VALUE(r16_float                , 54)
    __RPS_ENUM_VALUE(d16_unorm                , 55)
    __RPS_ENUM_VALUE(r16_unorm                , 56)
    __RPS_ENUM_VALUE(r16_uint                 , 57)
    __RPS_ENUM_VALUE(r16_snorm                , 58)
    __RPS_ENUM_VALUE(r16_sint                 , 59)
    __RPS_ENUM_VALUE(r8_typeless              , 60)
    __RPS_ENUM_VALUE(r8_unorm                 , 61)
    __RPS_ENUM_VALUE(r8_uint                  , 62)
    __RPS_ENUM_VALUE(r8_snorm                 , 63)
    __RPS_ENUM_VALUE(r8_sint                  , 64)
    __RPS_ENUM_VALUE(a8_unorm                 , 65)
    __RPS_ENUM_VALUE(r1_unorm                 , 66)
    __RPS_ENUM_VALUE(r9g9b9e5_sharedexp       , 67)
    __RPS_ENUM_VALUE(r8g8_b8g8_unorm          , 68)
    __RPS_ENUM_VALUE(g8r8_g8b8_unorm          , 69)
    __RPS_ENUM_VALUE(bc1_typeless             , 70)
    __RPS_ENUM_VALUE(bc1_unorm                , 71)
    __RPS_ENUM_VALUE(bc1_unorm_srgb           , 72)
    __RPS_ENUM_VALUE(bc2_typeless             , 73)
    __RPS_ENUM_VALUE(bc2_unorm                , 74)
    __RPS_ENUM_VALUE(bc2_unorm_srgb           , 75)
    __RPS_ENUM_VALUE(bc3_typeless             , 76)
    __RPS_ENUM_VALUE(bc3_unorm                , 77)
    __RPS_ENUM_VALUE(bc3_unorm_srgb           , 78)
    __RPS_ENUM_VALUE(bc4_typeless             , 79)
    __RPS_ENUM_VALUE(bc4_unorm                , 80)
    __RPS_ENUM_VALUE(bc4_snorm                , 81)
    __RPS_ENUM_VALUE(bc5_typeless             , 82)
    __RPS_ENUM_VALUE(bc5_unorm                , 83)
    __RPS_ENUM_VALUE(bc5_snorm                , 84)
    __RPS_ENUM_VALUE(b5g6r5_unorm             , 85)
    __RPS_ENUM_VALUE(b5g5r5a1_unorm           , 86)
    __RPS_ENUM_VALUE(b8g8r8a8_unorm           , 87)
    __RPS_ENUM_VALUE(b8g8r8x8_unorm           , 88)
    __RPS_ENUM_VALUE(b8g8r8a8_typeless        , 90)
    __RPS_ENUM_VALUE(b8g8r8a8_unorm_srgb      , 91)
    __RPS_ENUM_VALUE(b8g8r8x8_typeless        , 92)
    __RPS_ENUM_VALUE(b8g8r8x8_unorm_srgb      , 93)
__RPS_END_DECL_ENUM()

__RPS_BEGIN_DECL_ENUM(access)
    __RPS_ENUM_VALUE(common          , 1 << 0)
    __RPS_ENUM_VALUE(vertex_and_constant_buffer , 1 << 1)
    __RPS_ENUM_VALUE(index_buffer    , 1 << 2)
    __RPS_ENUM_VALUE(render_target   , 1 << 3)
    __RPS_ENUM_VALUE(unordered_access , 1 << 4)
    __RPS_ENUM_VALUE(depth_write     , 1 << 5)
    __RPS_ENUM_VALUE(depth_read      , 1 << 6)
    __RPS_ENUM_VALUE(non_ps_resource , 1 << 7)
    __RPS_ENUM_VALUE(ps_resource     , 1 << 8)
    __RPS_ENUM_VALUE(stream_out      , 1 << 9)
    __RPS_ENUM_VALUE(indirect_args   , 1 << 10)
    __RPS_ENUM_VALUE(copy_dest       , 1 << 11)
    __RPS_ENUM_VALUE(copy_source     , 1 << 12)
    __RPS_ENUM_VALUE(resolve_dest    , 1 << 13)
    __RPS_ENUM_VALUE(resolve_source  , 1 << 14)
    __RPS_ENUM_VALUE(raytracing_acceleration_structure, 1 << 15)
    __RPS_ENUM_VALUE(shading_rate_source , 1 << 16)
    __RPS_ENUM_VALUE(unknown         , 1 << 31)
__RPS_END_DECL_ENUM()

namespace rps
{
    static const uint uav_buffer = 0;
    static const uint uav_tex1d = 1;
    static const uint uav_tex1darray = 2;
    static const uint uav_tex2d = 3;
    static const uint uav_tex2darray = 4;
    static const uint uav_tex3d = 5;

    static const uint srv_buffer = 0x10;
    static const uint srv_tex1d = 0x11;
    static const uint srv_tex1darray = 0x12;
    static const uint srv_tex2d = 0x13;
    static const uint srv_tex2darray = 0x14;
    static const uint srv_tex2dms = 0x15;
    static const uint srv_tex2dmsarray = 0x16;
    static const uint srv_tex3d = 0x17;
    static const uint srv_texcube = 0x18;
    static const uint srv_texcubearray = 0x19;
    static const uint srv_rtas = 0x1a;

    static const uint rtv_buffer = 0x40;
    static const uint rtv_tex1d = 0x41;
    static const uint rtv_tex1darray = 0x42;
    static const uint rtv_tex2d = 0x43;
    static const uint rtv_tex2darray = 0x44;
    static const uint rtv_tex2dms = 0x45;
    static const uint rtv_tex2dmsarray = 0x46;
    static const uint rtv_tex3d = 0x47;

    static const uint dsv_tex1d = 0x50;
    static const uint dsv_tex1darray = 0x51;
    static const uint dsv_tex2d = 0x52;
    static const uint dsv_tex2darray = 0x53;
    static const uint dsv_tex2dms = 0x54;
    static const uint dsv_tex2dmsarray = 0x55;

    static const uint dsv_readonly_depth = 0x8000;
    static const uint dsv_readonly_stencil = 0x4000;

    static const uint cbv = 0x70;

    namespace clear
    {
        static const uint color = 0;
        static const uint depth = 1;
        static const uint stencil = 2;
        static const uint uavfloat = 4;
        static const uint uavuint = 8;
    }
}

struct SampleDesc
{
  uint Count;
  uint Quality;
};

struct ResourceDesc
{
  resourcetype Dimension;
  format Format;
  uint64_t Width;
  uint Height;
  uint DepthOrArraySize;
  uint MipLevels;
  SampleDesc SampleDesc;
};

struct ResourceViewDesc
{
  resource Parent;
  uint Dimension;
  format Format;
  uint BaseMip;
  uint Mips;
  uint BaseArraySlice;
  uint ArrayLayers;
  uint PlaneMask;
  access AccessFlags;
};

ResourceDesc describe_resource( resource r );
ResourceViewDesc describe_view( view v );
resource create_resource( ResourceDesc desc );
view create_view( ResourceViewDesc desc );
void clear_view( view v, uint option, uint4 data );

inline resource create_tex2d( uint width, uint height, uint arraySlices, uint numMips, format format, uint sampleCount = 1, uint sampleQuality = 0 )
{
    ResourceDesc desc;
    desc.Dimension = rps::resourcetype::tex2d;
    desc.Format = format;
    desc.Width = width;
    desc.Height = height;
    desc.DepthOrArraySize = arraySlices;
    desc.MipLevels = numMips;
    desc.SampleDesc.Count = sampleCount;
    desc.SampleDesc.Quality = sampleQuality;

    return create_resource(desc);
}

inline resource create_buffer( uint64_t width, format format = rps::format::unknown )
{
    ResourceDesc desc;
    desc.Dimension = rps::resourcetype::buffer;
    desc.Format = format;
    desc.Width = width;
    desc.Height = 1;
    desc.DepthOrArraySize = 1;
    desc.MipLevels = 1;
    desc.SampleDesc.Count = 1;
    desc.SampleDesc.Quality = 0;

    return create_resource(desc);
}

inline view create_srv( resource r, uint dimension, uint baseMip, uint mipLevels = 1, uint baseArraySlice = 0, uint numArraySlices = 1, uint planeMask = 1, format format = rps::format::unknown )
{
    ResourceViewDesc desc;
    desc.Parent = r;
    desc.Dimension = dimension;
    desc.Format = format;
    desc.BaseMip = baseMip;
    desc.Mips = mipLevels;
    desc.BaseArraySlice = baseArraySlice;
    desc.ArrayLayers = numArraySlices;
    desc.PlaneMask = planeMask;
    desc.AccessFlags = access::or(rps::access::ps_resource, rps::access::non_ps_resource);

    return create_view( desc );
}

inline view create_uav( resource r, uint dimension, uint baseMip, uint baseArraySlice = 0, uint numArraySlices = 1, format format = rps::format::unknown )
{
    ResourceViewDesc desc;
    desc.Parent = r;
    desc.Dimension = dimension;
    desc.Format = format;
    desc.BaseMip = baseMip;
    desc.Mips = 1;
    desc.BaseArraySlice = baseArraySlice;
    desc.ArrayLayers = numArraySlices;
    desc.PlaneMask = 0x1;
    desc.AccessFlags = rps::access::unordered_access;

    return create_view( desc );
}

inline view create_rtv( resource r, uint dimension, uint baseMip, uint baseArraySlice = 0, uint numArraySlices = 1, format format = rps::format::unknown )
{
    ResourceViewDesc desc;
    desc.Parent = r;
    desc.Dimension = dimension;
    desc.Format = format;
    desc.BaseMip = baseMip;
    desc.Mips = 1;
    desc.BaseArraySlice = baseArraySlice;
    desc.ArrayLayers = numArraySlices;
    desc.PlaneMask = 0x1;
    desc.AccessFlags = rps::access::render_target;

    return create_view( desc );
}

inline view create_dsv( resource r, uint dimension, uint baseMip, uint baseArraySlice = 0, uint numArraySlices = 1, format format = rps::format::unknown, uint planeMask = 0x3, bool bReadonly = false )
{
    ResourceViewDesc desc;
    desc.Parent = r;
    desc.Dimension = dimension;
    desc.Format = format;
    desc.BaseMip = baseMip;
    desc.Mips = 1;
    desc.BaseArraySlice = baseArraySlice;
    desc.ArrayLayers = numArraySlices;
    desc.PlaneMask = planeMask; // TODO: consider format

    if (bReadonly)
        desc.AccessFlags = rps::access::depth_read;
    else
        desc.AccessFlags = rps::access::depth_write;

    return create_view( desc );
}

inline void clear( rtv d, float4 val )
{
    return clear_view( d, rps::clear::color, asuint(val) );
}
)";

using ProcessPartCallback = std::function<ComPtr<IDxcBlob> (UINT32 part, const ComPtr<IDxcBlob>& pPart)>;

ComPtr<IDxcBlob>
ProcessContainerParts(const ComPtr<IDxcBlob> &pOriginalContainer,
                      ProcessPartCallback processPartCb) {
  ComPtr<IDxcContainerReflection> pRefl;
  g_pfnDxcCreateInstance(CLSID_DxcContainerReflection, IID_PPV_ARGS(&pRefl));

// This is for including debug info, but llvm-cbe doesn't work with this
// for now because subprogram changes in the llvm version differences.
#if RPS_ENABLE_DEBUG_INFO
  ComPtr<IDxcContainerBuilder> pRebuilder;
  g_pfnDxcCreateInstance(CLSID_DxcContainerBuilder, IID_PPV_ARGS(&pRebuilder));
#endif

  ThrowIfFailed(pRefl->Load(pOriginalContainer.Get()));

  UINT numParts = 0;

  if (SUCCEEDED(pRefl->GetPartCount(&numParts))) {

    for (UINT iPart = 0; iPart < numParts; iPart++) {
      ComPtr<IDxcBlob> pPartContent;
      if (FAILED(pRefl->GetPartContent(iPart, &pPartContent))) {
        break;
      }

      UINT32 partKind = 0;
      pRefl->GetPartKind(iPart, &partKind);

      printf("\nProcessing part %c%c%c%c", partKind & 0xff,
             (partKind >> 8) & 0xff, (partKind >> 16) & 0xff,
             (partKind >> 24) & 0xff); //'LIXD'

      auto pProcessedPart = processPartCb(partKind, pPartContent);

#if RPS_ENABLE_DEBUG_INFO
      if (FAILED(pRebuilder->AddPart(partKind, pProcessedPart.Get()))) {
        break;
      }
#endif
    }
  }

  ComPtr<IDxcBlob> pNewContainer = pOriginalContainer;
#if RPS_ENABLE_DEBUG_INFO
  ComPtr<IDxcOperationResult> pBuildResult;
  if (SUCCEEDED(pRebuilder->SerializeContainer(&pBuildResult))) {
    pBuildResult->GetResult(&pNewContainer);
  }
#endif
  return pNewContainer;
}

ComPtr<IDxcBlob> CompileHlslToDxilContainer(const char *fileName) {
  ComPtr<IDxcLibrary> pLibrary;
  ThrowIfFailed(
      g_pfnDxcCreateInstance(CLSID_DxcLibrary, IID_PPV_ARGS(&pLibrary)));

  FILE *fp = {};
  fopen_s(&fp, fileName, "rb");
  if (!fp) {
    printf("Failed to open file '%s'", fileName);
    return nullptr;
  }

  fseek(fp, 0, SEEK_END);
  auto fileLen = ftell(fp);
  fseek(fp, 0, SEEK_SET);

  std::vector<char> text;
  text.resize(_countof(c_RpsHeader) + fileLen);

  memcpy(&text[0], c_RpsHeader, _countof(c_RpsHeader) - 1);
  fread(&text[_countof(c_RpsHeader) - 1], 1, fileLen, fp);
  fclose(fp);

  ComPtr<IDxcBlobEncoding> pSource, pError;
  ThrowIfFailed(pLibrary->CreateBlobWithEncodingFromPinned(
      text.data(), static_cast<UINT32>(text.size()), CP_UTF8, &pSource));

  ComPtr<IDxcCompiler> pCompiler;
  ThrowIfFailed(
      g_pfnDxcCreateInstance(CLSID_DxcCompiler, IID_PPV_ARGS(&pCompiler)));

  size_t nConv = 0;
  wchar_t fileNameW[MAX_PATH + 1];
  mbstowcs_s(&nConv, fileNameW, fileName, _countof(fileNameW) - 1);

  std::vector<LPCWSTR> arguments;

  arguments.push_back(L"-Vd");
  arguments.push_back(L"-default-linkage");
  arguments.push_back(L"external");

  arguments.push_back(L"-Zi");
  arguments.push_back(L"-res_may_alias");

  ComPtr<IDxcOperationResult> pResult;
  HRESULT hr = pCompiler->Compile(
      pSource.Get(), fileNameW, L"", L"lib_6_3", arguments.data(),
      static_cast<UINT>(arguments.size()), nullptr, 0, nullptr, &pResult);

  if (pResult) {
    pResult->GetErrorBuffer(&pError);
  }

  if (pError) {
    ComPtr<IDxcBlobEncoding> pErrorUtf16;
    pLibrary->GetBlobAsUtf16(pError.Get(), &pErrorUtf16);
    std::wstring errText(
        reinterpret_cast<WCHAR *>(pErrorUtf16->GetBufferPointer()),
        reinterpret_cast<WCHAR *>(pErrorUtf16->GetBufferPointer()) +
            pErrorUtf16->GetBufferSize() / 2);
    wprintf(L"\n");
    wprintf(errText.c_str());
  }

  if (FAILED(hr)) {
    return nullptr;
  }

  ComPtr<IDxcBlob> pContainer;
  pResult->GetResult(&pContainer);

  if (pContainer) {
    ComPtr<IDxcBlobEncoding> pDisasm;
    ThrowIfFailed(pCompiler->Disassemble(pContainer.Get(), &pDisasm));

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

  return pContainer;
}

ComPtr<IDxcBlob> ConvertDxilToRps(const ComPtr<IDxcBlob> &pContainer) {
  ComPtr<IDxcOptimizer> pOptimizer;
  ThrowIfFailed(
      g_pfnDxcCreateInstance(CLSID_DxcOptimizer, IID_PPV_ARGS(&pOptimizer)));

  UINT32 passCnt = 0;
  ThrowIfFailed(pOptimizer->GetAvailablePassCount(&passCnt));

  bool bFoundRpsPass = false;
  for (UINT32 i = 0; i < passCnt; i++) {
    ComPtr<IDxcOptimizerPass> pPass;
    ThrowIfFailed(pOptimizer->GetAvailablePass(i, &pPass));

    LPWSTR name;
    pPass->GetOptionName(&name);

    printf("\n%S", name);

    if (0 == wcscmp(name, L"dxil-2-rps")) {
      bFoundRpsPass = true;
      break;
    }
  }

  printf("%s dxil-2-rps pass.", bFoundRpsPass ? "Found" : "Didn't find");

  ComPtr<IDxcBlob> pOutContianer;

  if (bFoundRpsPass) {
    std::vector<LPCWSTR> arguments;
    arguments.push_back(L"-dxil-2-rps");

    if (OutputModuleName != "-") {
      arguments.push_back(L"-module-name");

      wchar_t stemW[MAX_PATH];

      size_t nConv = 0;
      mbstowcs_s(&nConv, stemW, OutputModuleName.c_str(), _countof(stemW) - 1);
      arguments.push_back(stemW);
    }

    auto processDxil = ProcessPartCallback(
        [&](UINT32 partKind,
            const ComPtr<IDxcBlob> &pPart) -> ComPtr<IDxcBlob> {
          ComPtr<IDxcBlob> pOutPart = pPart;
          ComPtr<IDxcBlobEncoding> pOutText;
          if ((partKind == 'BDLI')) { // || (partKind == 'LIXD')
            ThrowIfFailed(pOptimizer->RunOptimizer(
                pPart.Get(), arguments.data(), arguments.size(), &pOutPart,
                &pOutText));

            pOutContianer = pOutPart;

            if (pOutText) {
              printf("\n%s", (char *)pOutText->GetBufferPointer());
            }
          }
          return pOutPart;
        });

    auto pProcessedContainer = ProcessContainerParts(pContainer, processDxil);

#if 0 && RPS_ENABLE_DEBUG_INFO
    pOutContianer = pProcessedContainer;
#endif

    if (DumpRpsILBin.getValue()) {
      FILE *fp = {};
      fopen_s(&fp, (OutputFileDirectoryAndStem + ".rps.blob").c_str(), "wb");
      if (fp) {
        fwrite(pOutContianer->GetBufferPointer(), 1,
               pOutContianer->GetBufferSize(), fp);
        fclose(fp);
      }
    }
  }

  return pOutContianer;
}

void DisassembleRps(const ComPtr<IDxcBlob> &pRpsBC, const char* tmpFileName) {
  ComPtr<IDxcCompiler> pCompiler;

  ThrowIfFailed(
      g_pfnDxcCreateInstance(CLSID_DxcCompiler, IID_PPV_ARGS(&pCompiler)));

  ComPtr<IDxcBlobEncoding> pDisasm;
  ThrowIfFailed(pCompiler->Disassemble(pRpsBC.Get(), &pDisasm));

  FILE *fp = {};
  fopen_s(&fp, tmpFileName, "wb");
  if (fp) {
    fwrite(pDisasm->GetBufferPointer(), 1, pDisasm->GetBufferSize(), fp);
    fclose(fp);
  }
}

int main(const int argc, const char *argv[]) {
  // Parse command line options.
  cl::ParseCommandLineOptions(argc, argv, "dxil assembly\n");

  char currExecPath[MAX_PATH];
  ::GetModuleFileNameA(nullptr, currExecPath, _countof(currExecPath));
  auto currExecDir = llvm::sys::path::parent_path(currExecPath).str();

  SHCreateDirectoryExA(NULL, OutputDirectory.c_str(), NULL);

  printf("Using rps-hlslc.exe in %s", currExecDir.c_str());

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

  auto pRpsBC = ConvertDxilToRps(pCode);

  auto tmpRpsLLFile = OutputDirectory + "/" + OutputFileStem + ".tmp.rps.ll";

  DisassembleRps(pRpsBC, tmpRpsLLFile.c_str());

  int result = 0;

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

  if (OutputCbe.getValue()) {
    result = system((currExecDir + "/llvm-cbe.exe " + tmpRpsLLFile +
                     " -o " + OutputFileDirectoryAndStem + ".rpsl.g.c")
                        .c_str());
  }

  return result;
}
