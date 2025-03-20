cmake_minimum_required(VERSION 3.21)
project(signal)

set(CMAKE_CXX_STANDARD 20)

find_package(GTest REQUIRED)

file(GLOB SOLUTION_SRC src/*.cpp src/*.h)
file(GLOB TEST_SRC test/*.cpp test/*.h)

add_executable(tests ${TEST_SRC} ${SOLUTION_SRC})

target_include_directories(tests PRIVATE src test)

if(CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
  target_compile_options(tests PRIVATE /W4 /permissive-)
  if(TREAT_WARNINGS_AS_ERRORS)
    target_compile_options(tests PRIVATE /WX)
  endif()
  target_compile_definitions(tests PRIVATE -D_CRT_SECURE_NO_WARNINGS)
else()
  target_compile_options(tests PRIVATE -Wall -pedantic -Wextra)
  target_compile_options(tests PRIVATE -Wno-sign-compare -Wno-self-move)
  target_compile_options(tests PRIVATE -Wold-style-cast)
  target_compile_options(tests PRIVATE -Wextra-semi)
  target_compile_options(tests PRIVATE -Woverloaded-virtual)
  target_compile_options(tests PRIVATE -Wzero-as-null-pointer-constant)
  if(TREAT_WARNINGS_AS_ERRORS)
    target_compile_options(tests PRIVATE -Werror -pedantic-errors)
  endif()
endif()

# Compiler specific warnings
if(CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
  target_compile_options(tests PRIVATE -Wshadow=compatible-local)
  target_compile_options(tests PRIVATE -Wduplicated-branches)
  target_compile_options(tests PRIVATE -Wduplicated-cond)
  # Disabled due to GCC bug
  # target_compile_options(tests PRIVATE -Wnull-dereference)
  target_compile_options(tests PRIVATE -Wno-dangling-pointer)
elseif(CMAKE_CXX_COMPILER_ID MATCHES "Clang")
  target_compile_options(tests PRIVATE -Wshadow-uncaptured-local)
  target_compile_options(tests PRIVATE -Wloop-analysis)
  target_compile_options(tests PRIVATE -Wno-self-assign-overloaded)
endif()

option(USE_SANITIZERS "Enable to build with undefined and address sanitizers" OFF)
if(USE_SANITIZERS)
  if(CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
    message(STATUS "Enabling ASAN")
    target_compile_options(tests PUBLIC /fsanitize=address)
    target_compile_definitions(tests PUBLIC _DISABLE_STRING_ANNOTATION=1 _DISABLE_VECTOR_ANNOTATION=1)
  else()
    message(STATUS "Enabling USAN and ASAN")
    target_compile_options(tests PUBLIC -fsanitize=undefined,address)
    target_link_options(tests PUBLIC -fsanitize=undefined,address)

    target_compile_options(tests PUBLIC -fno-sanitize-recover=all -fno-optimize-sibling-calls -fno-omit-frame-pointer)
  endif()
endif()

option(USE_THREAD_SANITIZER "Enable to build with thread sanitizer" OFF)
if(USE_THREAD_SANITIZER)
  message(STATUS "Enabling TSAN")
  target_compile_options(tests PUBLIC -fsanitize=thread -fno-sanitize-recover=all)
  target_link_options(tests PUBLIC -fsanitize=thread)
endif()

target_link_libraries(tests GTest::gtest GTest::gtest_main)
