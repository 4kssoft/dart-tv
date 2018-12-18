# Specify the minimum version for CMake

cmake_minimum_required(VERSION 3.7)

# Project's name

project(Dart_LLVM VERSION 1.0 LANGUAGES CXX)

# YCM
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# flags

if (NOT CMAKE_BUILD_TYPE)
  # Default to Debug mode.
  set(CMAKE_BUILD_TYPE "Debug")
endif()

add_definitions(-std=c++14 -fno-rtti)
# Only Debug mode.
if (NOT CMAKE_BUILD_TYPE MATCHES "^Debug$")
  message(FATAL_ERROR "Invalid value for CMAKE_BUILD_TYPE: ${CMAKE_BUILD_TYPE}")
else()
  add_definitions(-g -Wall -fexceptions -Wno-switch -fsanitize=address)
endif()

if (NOT TARGET_ARCH)
  message(FATAL_ERROR "TARGET_ARCH must be specified.")
elseif (NOT TARGET_ARCH MATCHES "^(X64|WASM)$")
  message(FATAL_ERROR "Invalid TARGET_ARCH.")
elseif (TARGET_ARCH STREQUAL "X64")
  add_definitions(-DTARGET_X64)
elseif (TARGET_ARCH STREQUAL "WASM")
  add_definitions(-DTARGET_WASM)
endif()

# Set the output folder where your program will be created
set(EXECUTABLE_OUTPUT_PATH ${PROJECT_SOURCE_DIR}/bin/${CMAKE_BUILD_TYPE}${TARGET_ARCH})
set(LIBRARY_OUTPUT_PATH ${PROJECT_SOURCE_DIR}/lib/${CMAKE_BUILD_TYPE}${TARGET_ARCH})

find_package(LLVM REQUIRED CONFIG)
include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

llvm_map_components_to_libnames(llvm_libs core support ipo scalaropts transformutils)

# The following folder will be included
include_directories(src)
include_directories(third_party)

add_subdirectory(src)
