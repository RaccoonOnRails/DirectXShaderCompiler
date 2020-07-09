#include <stdint.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdlib.h>

#define RPS_MAX_ARGS 1024

extern const char* __rps_nodedefs[];
extern void* __rps_entries[];

uint32_t g_paramCount = 0;
void* g_params[RPS_MAX_ARGS];

typedef int (*PFN_RpsNodeCallback)(uint32_t nodeId, int argc, void** args);

PFN_RpsNodeCallback __rps_nodecallback = NULL;

__declspec(dllexport) void __rps_set_nodecallback(PFN_RpsNodeCallback funcPtr)
{
    __rps_nodecallback = funcPtr;
}

__declspec(dllexport) const char* __rps_enum_nodedefs(uint32_t entryId)
{
    return __rps_nodedefs[entryId];
}

__declspec(dllexport) void* __rps_enum_entries(uint32_t entryId, const char** name)
{
    void* func = __rps_entries[(entryId << 1)];
    if (func)
    {
        *name = __rps_entries[(entryId << 1) + 1];
    }
    else
    {
        *name = NULL;
    }
    return func;
}

void __rps_node_call()
{
    if (__rps_nodecallback)
    {
        __rps_nodecallback((uint32_t)(uintptr_t)(g_params[g_paramCount - 1]), g_paramCount - 1, g_params);
    }
    g_paramCount = 0;
}

void __rps_param_push(void* ptr)
{
    if (g_paramCount < RPS_MAX_ARGS)
    {
        g_params[g_paramCount] = ptr;
        g_paramCount++;
    }
    else
    {
        __debugbreak();
    }
}

#ifndef _DLL

int RpsNodeCallback(uint32_t nodeId, int argc, void** args)
{
    printf("\n[RPS] Calling node %d, with %d params at %p", nodeId, argc, args);
    return 0;
}


int main()
{
    system("pause");

    uint32_t i = 0;
    const char* funcName = NULL;

    typedef void (*PFN_SimpleTriangle)(bool b, int n);

    PFN_SimpleTriangle entryPoint = NULL;

    __rps_set_nodecallback(&RpsNodeCallback);

    printf("\nRps entries:");
    for (;;)
    {
        void* func = __rps_enum_entries(i, &funcName);
        if (func)
        {
            printf("\n    %s", funcName);
            i++;

            if (0 == strcmp(funcName, "SimpleTriangle"))
            {
                entryPoint = (PFN_SimpleTriangle)func;
            }
        }
        else
        {
            break;
        }
    }

    if (entryPoint)
    {
        entryPoint(true, 3);

        entryPoint(false, 10);
    }
}

#endif
