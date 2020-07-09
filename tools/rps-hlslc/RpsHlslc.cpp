#include <Windows.h>
#include <wrl/client.h>
#include <dxcapi.h>
#include <string>
#include <vector>
#include <sstream>
#include <algorithm>

// Loader
HMODULE g_DxcDll = {};
DxcCreateInstanceProc g_pfnDxcCreateInstance = {};

bool LoadDxc()
{
    if (g_DxcDll == nullptr)
    {
        g_DxcDll = LoadLibrary(TEXT("dxcompiler.dll"));

        if (g_DxcDll)
        {
            g_pfnDxcCreateInstance = (DxcCreateInstanceProc)GetProcAddress(g_DxcDll, "DxcCreateInstance");
        }
    }

    return (g_pfnDxcCreateInstance != nullptr);
}


using Microsoft::WRL::ComPtr;
using namespace std::string_literals;

void ThrowIfFailed(HRESULT hr)
{
    if (FAILED(hr)) { throw; }
}

static const char c_RpsHeader[] = R"(

struct nodeidentifier { uint unused; };
#define nodedef [noinline] nodeidentifier

typedef uint resource;
typedef uint view;
typedef uint RpsFormat;
typedef uint RpsResourceDimension;

struct ResourceViewDesc
{
  resource parent;
  uint dimension;
  RpsFormat format;
  uint baseMip;
  uint mips;
  uint baseArraySlice;
  uint arrayLayers;
  uint planeMask;
};

struct SampleDesc
{
  uint Count;
  uint Quality;
};

struct ResourceDesc
{
  RpsResourceDimension Dimension;
  uint2 Width;
  uint Height;
  uint DepthOrArraySize;
  uint MipLevels;
  RpsFormat Format;
  SampleDesc SampleDesc;
};

resource create_resource( ResourceDesc desc );
resource create_buffer( uint2 width, RpsFormat format );
resource create_texture2D( uint2 width, uint height, uint arraySize, uint mipLevels, RpsFormat format, uint sampleCount, uint sampleQuality );
ResourceDesc describe_resource( resource r );
ResourceViewDesc describe_view( view v );
view create_srv( resource r, uint baseMip, uint mipLevels );
view create_uav( resource r, uint baseMip, uint mipLevels );
)";

ComPtr<IDxcBlob> ReflectContainer(const ComPtr<IDxcBlob>& pContainer)
{
    ComPtr<IDxcContainerReflection> pRefl;
    g_pfnDxcCreateInstance(CLSID_DxcContainerReflection, IID_PPV_ARGS(&pRefl));

    ThrowIfFailed(pRefl->Load(pContainer.Get()));

    ComPtr<IDxcBlob> dxilContent;
    UINT32 dxilIdx;
    if (SUCCEEDED(pRefl->FindFirstPartKind('LIXD', &dxilIdx)))
    {
        printf("\nFound DXIL");

        pRefl->GetPartContent(dxilIdx, &dxilContent);

        FILE* fp = {};
        fopen_s(&fp, "tmp.bc", "wb");
        if (fp)
        {
            fwrite(dxilContent->GetBufferPointer(), 1, dxilContent->GetBufferSize(), fp);
        }
        fclose(fp);
    }

    return dxilContent;
}

ComPtr<IDxcBlob> CompileHlslToDxil(const char* fileName)
{
    ComPtr<IDxcLibrary> pLibrary;
    ThrowIfFailed(g_pfnDxcCreateInstance(CLSID_DxcLibrary, IID_PPV_ARGS(&pLibrary)));

    FILE* fp = {};
    fopen_s(&fp, fileName, "rb");
    if (!fp)
    {
        return false;
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
    ThrowIfFailed(pLibrary->CreateBlobWithEncodingFromPinned(text.data(), static_cast<UINT32>(text.size()), CP_UTF8, &pSource));

    ComPtr<IDxcCompiler> pCompiler;
    ThrowIfFailed(g_pfnDxcCreateInstance(CLSID_DxcCompiler, IID_PPV_ARGS(&pCompiler)));

    std::vector<LPCWSTR> arguments;

    arguments.push_back(L"-Vd");
    arguments.push_back(L"-default-linkage");
    arguments.push_back(L"external");

    ComPtr<IDxcOperationResult> pResult;
    HRESULT hr = pCompiler->Compile(pSource.Get(), nullptr, L"", L"lib_6_3", arguments.data(), static_cast<UINT>(arguments.size()), nullptr, 0, nullptr, &pResult);
    pResult->GetErrorBuffer(&pError);
    if (pError)
    {
        ComPtr<IDxcBlobEncoding> pErrorUtf16;
        pLibrary->GetBlobAsUtf16(pError.Get(), &pErrorUtf16);
        std::wstring text(reinterpret_cast<WCHAR*>(pErrorUtf16->GetBufferPointer()), reinterpret_cast<WCHAR*>(pErrorUtf16->GetBufferPointer()) + pErrorUtf16->GetBufferSize() / 2);
        wprintf(L"\n");
        wprintf(text.c_str());
    }

    ComPtr<IDxcBlob> pContainer;
    pResult->GetResult(&pContainer);

    if (pContainer)
    {
        ComPtr<IDxcBlobEncoding> pDisasm;
        ThrowIfFailed(pCompiler->Disassemble(pContainer.Get(), &pDisasm));

        fp = {};
        fopen_s(&fp, "tmp.ll", "wb");
        if (fp)
        {
            fwrite(pDisasm->GetBufferPointer(), 1, pDisasm->GetBufferSize(), fp);
        }
        fclose(fp);
    }

    return ReflectContainer(pContainer);
}

ComPtr<IDxcBlob> CompileDxilToAsm(const ComPtr<IDxcBlob>& pShader)
{
    ComPtr<IDxcOptimizer> pOptimizer;
    ThrowIfFailed(g_pfnDxcCreateInstance(CLSID_DxcOptimizer, IID_PPV_ARGS(&pOptimizer)));

    UINT32 passCnt = 0;
    ThrowIfFailed(pOptimizer->GetAvailablePassCount(&passCnt));

    bool bFoundRpsPass = false;
    for (UINT32 i = 0; i < passCnt; i++)
    {
        ComPtr<IDxcOptimizerPass> pPass;
        ThrowIfFailed(pOptimizer->GetAvailablePass(i, &pPass));

        LPWSTR name;
        pPass->GetOptionName(&name);

        printf("\n%S", name);

        if (0 == wcscmp(name, L"dxil-2-rps"))
        {
            bFoundRpsPass = true;
            break;
        }
    }

    printf("%s dxil-2-rps pass.", bFoundRpsPass ? "Found" : "Didn't find");

    ComPtr<IDxcBlob> pOutModule;

    if (bFoundRpsPass)
    {
        LPCWSTR options[] =
        {
            L"-dxil-2-rps",
        };

        ComPtr<IDxcBlobEncoding> pOutText;

        ThrowIfFailed(pOptimizer->RunOptimizer(pShader.Get(), options, _countof(options), &pOutModule, &pOutText));

        if (pOutText)
        {
            printf("\n%s", (char*)pOutText->GetBufferPointer());
        }

        FILE* fp = {};
        fopen_s(&fp, "tmp.rps.bc", "wb");
        if (fp)
        {
            fwrite(pOutModule->GetBufferPointer(), 1, pOutModule->GetBufferSize(), fp);
            fclose(fp);
        }
    }

    return pOutModule;
}

void DisassembleRps(const ComPtr<IDxcBlob>& pRpsBC)
{
    ComPtr<IDxcCompiler> pCompiler;

    ThrowIfFailed(g_pfnDxcCreateInstance(CLSID_DxcCompiler, IID_PPV_ARGS(&pCompiler)));

    ComPtr<IDxcBlobEncoding> pDisasm;
    ThrowIfFailed(pCompiler->Disassemble(pRpsBC.Get(), &pDisasm));

    FILE* fp = {};
    fopen_s(&fp, "tmp.rps.ll", "wb");
    if (fp)
    {
        fwrite(pDisasm->GetBufferPointer(), 1, pDisasm->GetBufferSize(), fp);
        fclose(fp);
    }
}

int main(const int argc, const char* argv[])
{
    LoadDxc();

    auto pCode = CompileHlslToDxil(argv[1]);

    auto pRpsBC = CompileDxilToAsm(pCode);

    DisassembleRps(pRpsBC);

    system("llc.exe -filetype=obj -mtriple=x86_64-pc-win32 tmp.rps.ll");
    system("llc.exe -filetype=asm -mtriple=x86_64-pc-win32 --x86-asm-syntax=intel tmp.rps.ll");

    return 0;
}
