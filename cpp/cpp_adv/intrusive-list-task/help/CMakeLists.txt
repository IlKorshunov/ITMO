cmake_minimum_required(VERSION 3.21)

project(intrusive_list)
set(CMAKE_CXX_STANDARD 20)

set(SOURCE_FILES
    src/intrusive_list.cpp
    main.cpp
)

add_executable(intrusive_list ${SOURCE_FILES})
target_include_directories(intrusive_list PRIVATE src)
