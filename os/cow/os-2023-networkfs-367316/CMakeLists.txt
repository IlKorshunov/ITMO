cmake_minimum_required(VERSION 3.14 FATAL_ERROR)

# Ref:
# https://gitlab.com/christophacham/cmake-kernel-module
# https://gitioc.upc.edu/open-source/xenomai-cmake/blob/master/cmake-modules/FindKernelHeaders.cmake
# https://stackoverflow.com/questions/50877135/cmake-specify-linux-kernel-module-output-build-directory

project(networkfs LANGUAGES C CXX)

# List driver sources
set(SOURCES entrypoint.c http.c)

# We use gnu++17
set(CMAKE_C_STANDARD 17)
set(CMAKE_C_EXTENSIONS ON)
set(CMAKE_C_STANDARD_REQUIRED ON)

# Use -DARCH=.. to build against something else than x86
# Check /usr/src/linux-headers-$(uname -r)/arch for available options
if(NOT DEFINED ARCH)
    set(ARCH x86)
endif()

# Use -DKERNELHEADERS_DIR=.. to specify headers path
# By default, we assume you want to build against running kernel
if(NOT DEFINED KERNELHEADERS_DIR)
    # Find the kernel release
    execute_process(
        COMMAND uname -r
        OUTPUT_VARIABLE KERNEL_RELEASE
        OUTPUT_STRIP_TRAILING_WHITESPACE
    )

    # Find the headers
    find_path(
        KERNELHEADERS_DIR
        include/linux/user.h
        PATHS /usr/src/linux-headers-${KERNEL_RELEASE}
    )
    
    message(STATUS "Kernel release: ${KERNEL_RELEASE}")
endif()

message(STATUS "Kernel headers: ${KERNELHEADERS_DIR}")

# Generate Kbuild file for kernel Makefile to build
string(REPLACE ";" " " SOURCES_LINE "${SOURCES}")

file(WRITE ${CMAKE_CURRENT_SOURCE_DIR}/Kbuild
    "obj-m := ${CMAKE_PROJECT_NAME}.o
${CMAKE_PROJECT_NAME}-srcs := ${SOURCES_LINE}
${CMAKE_PROJECT_NAME}-y := $(${CMAKE_PROJECT_NAME}-srcs:.c=.o)
ccflags-y := -std=gnu17 -Wall -Werror -Wno-declaration-after-statement"
)

# The actual kernel build will be made through make call
set(DRIVER_FILE "${CMAKE_PROJECT_NAME}.ko")
add_custom_command(
    OUTPUT ${DRIVER_FILE}
    COMMAND $(MAKE) -C ${KERNELHEADERS_DIR} modules
        M=${CMAKE_CURRENT_BINARY_DIR} src=${CMAKE_CURRENT_SOURCE_DIR}
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    DEPENDS ${SOURCES} VERBATIM)

# Add it as a target
add_custom_target(networkfs ALL DEPENDS ${DRIVER_FILE})

# Make a fake library from the same sources to enable LSP
add_library(dummy ${SOURCES})

target_compile_definitions(dummy PRIVATE -D__KERNEL__ -DMODULE)

# All header files
# Ref: https://github.com/microsoft/vscode-cpptools/issues/5588#issuecomment-662116156
target_include_directories(
    dummy
    PRIVATE
    ${KERNELHEADERS_DIR}/include
    ${KERNELHEADERS_DIR}/include/uapi
    ${KERNELHEADERS_DIR}/include/generated/uapi
    ${KERNELHEADERS_DIR}/arch/${ARCH}/include
    ${KERNELHEADERS_DIR}/arch/${ARCH}/include/generated
    ${KERNELHEADERS_DIR}/arch/${ARCH}/include/uapi
    ${KERNELHEADERS_DIR}/arch/${ARCH}/include/generated/uapi
)

include(CTest)

# Tests use gnu++20
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_EXTENSIONS ON)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Fixup for out-of-tree builds
file(CREATE_LINK ${PROJECT_SOURCE_DIR}/CMakePresets.json ${CMAKE_BINARY_DIR}/CMakePresets.json SYMBOLIC)

include(FetchContent)

FetchContent_Declare(
    googletest
    URL https://github.com/google/googletest/archive/03597a01ee50ed33e9dfd640b249b4be3799d395.zip
)
# For Windows: Prevent overriding the parent project's compiler/linker settings
set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(googletest)

FetchContent_Declare(
    httplib
    GIT_REPOSITORY "https://github.com/yhirose/cpp-httplib.git"
    GIT_TAG "v0.14.1"
) 
FetchContent_MakeAvailable(httplib)

add_executable(networkfs_test
    tests/base.cpp tests/encoding.cpp tests/file.cpp tests/link.cpp
    tests/lib/nfs.hpp tests/lib/nfs.cpp
    tests/lib/test.hpp
    tests/lib/util.hpp tests/lib/util.cpp
    tests/lib/main.cpp
)
target_link_libraries(networkfs_test PRIVATE GTest::gtest httplib::httplib)

# We add build procedure as fixtures to all others
# Ref: https://crascit.com/2016/10/18/test-fixtures-with-cmake-ctest/
add_test(
    NAME build_module
    COMMAND "${CMAKE_COMMAND}" --build "${CMAKE_BINARY_DIR}"
        --config "$<CONFIG>" --target networkfs
)

# Unfortunately, test discovery takes place before fixtures are executed, so any
# attempt to build tests here as well will make things more complicated.
set_tests_properties(build_module PROPERTIES FIXTURES_SETUP prepare_tests)

include(GoogleTest)
gtest_discover_tests(networkfs_test PROPERTIES RUN_SERIAL TRUE FIXTURES_REQUIRED prepare_tests DISCOVERY_MODE PRE_TEST)

# We exclude our fake and test targets from `make all`
set_target_properties(
    dummy networkfs_test gtest gmock gtest_main gmock_main
    PROPERTIES
    EXCLUDE_FROM_ALL 1
    EXCLUDE_FROM_DEFAULT_BUILD 1
)
