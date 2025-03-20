cmake_minimum_required(VERSION 3.16)
project(Lab_2 C ASM)

set(CMAKE_C_STANDARD 11)

aux_source_directory(./isa-l/igzip SRC)
aux_source_directory(./isa-l/include SRC)
aux_source_directory(./isa-l/crc SRC)
add_library(igzip ${SRC})
target_include_directories(igzip PUBLIC ./isa-l ./isa-l/igzip ./isa-l/include)
add_subdirectory(./libdeflate)

add_executable(Lab_2_fin main.c return_codes.h)
target_link_libraries(Lab_2_fin igzip)
target_link_libraries(Lab_2_fin libdeflate_static z)
target_compile_definitions(Lab_2_fin PRIVATE ZLIB)