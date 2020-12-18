# Modifications Copyright (C) 2020 Advanced Micro Devices, Inc.

set( LLVM_LINK_COMPONENTS
  ${LLVM_TARGETS_TO_BUILD}
   dxcsupport
#  DXIL
#  DxilContainer
#  HLSL
#  Option     # option library
   Support    # just for assert and raw streams
   MSSupport
  )

set(HLSL_IGNORE_SOURCES RpsHlslcDummyDXIL.cpp)

set(HLSL_VERSION_LOCATION ${CMAKE_CURRENT_SOURCE_DIR})

add_clang_executable(rps-hlslc
  RpsHlslc.cpp
  )

#target_link_libraries(dxc
#  dxclib
#  dxcompiler
#  dxclib
#  )

#if(ENABLE_SPIRV_CODEGEN)
#  target_link_libraries(dxc SPIRV-Tools)
#endif()
#
#set_target_properties(dxc PROPERTIES VERSION ${CLANG_EXECUTABLE_VERSION})
#
#if (WIN32)
#  include_directories(AFTER ${DIASDK_INCLUDE_DIRS})
#endif (WIN32)
#
#include_directories(${LLVM_SOURCE_DIR}/tools/clang/tools)

add_dependencies(rps-hlslc dxcompiler)

#if(UNIX)
#  set(CLANGXX_LINK_OR_COPY create_symlink)
## Create a relative symlink
#  set(dxc_binary "dxc${CMAKE_EXECUTABLE_SUFFIX}")
#else()
#  set(CLANGXX_LINK_OR_COPY copy)
#  set(dxc_binary "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${CMAKE_CFG_INTDIR}/dxc${CMAKE_EXECUTABLE_SUFFIX}")
#endif()

install(TARGETS rps-hlslc
  RUNTIME DESTINATION bin)

# Hack - Dummy dxil.dll to prevent loading any real ones.
set(HLSL_IGNORE_SOURCES RpsHlslc.cpp)
add_clang_library(dummy-dxil SHARED
  RpsHlslcDummyDXIL.cpp
)

set_target_properties(dummy-dxil PROPERTIES
    OUTPUT_NAME dxil)

add_dependencies(rps-hlslc dummy-dxil)

install(TARGETS dummy-dxil
  RUNTIME DESTINATION bin)
