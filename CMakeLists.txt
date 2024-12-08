cmake_minimum_required(VERSION 3.10)
project(aoc)

add_compile_options(-O3 -flto -g)

add_library(libOc
    libOc/grid.cpp
)

include_directories(libOc libOc/)

add_executable(day_02 solutions/cpp/day_02.cpp libOc/grid.h)
add_executable(day_04 solutions/cpp/day_08.cpp libOc/grid.h)
add_executable(day_06 solutions/cpp/day_08.cpp libOc/grid.h)
add_executable(day_08 solutions/cpp/day_08.cpp libOc/grid.h)

target_link_libraries(day_02 libOc)
target_link_libraries(day_04 libOc)
target_link_libraries(day_06 libOc)
target_link_libraries(day_08 libOc)
