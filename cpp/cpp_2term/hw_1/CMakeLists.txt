project(labfir C)

set(CMAKE_C_STANDARD 99)

add_executable(labfir main.c return_codes.h)
target_link_libraries(labfir PRIVATE m)
