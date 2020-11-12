# Modifications Copyright (C) 2020 Advanced Micro Devices, Inc.

if(WIN32 AND HLSL_BUILD_DXILCONV)
  add_subdirectory(dxilconv)
endif (WIN32 AND HLSL_BUILD_DXILCONV)
add_subdirectory(llvm-cbe) # RPS Change