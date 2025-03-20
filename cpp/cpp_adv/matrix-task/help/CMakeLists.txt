cmake_minimum_required(VERSION 3.21)

project(matrix)
set(CMAKE_CXX_STANDARD 20)

add_executable(matrix_app main.cpp )
target_include_directories(matrix_app PRIVATE ../src)
target_compile_options(matrix_app PRIVATE -Wall -Wextra -Wpedantic)