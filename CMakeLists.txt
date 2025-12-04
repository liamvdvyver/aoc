cmake_minimum_required(VERSION 3.10)
project(aoc)

add_compile_options(-O3 -flto -std=c++23)
add_link_options(-fuse-ld=lld)

file(GLOB LIBOC_SRC libOc/libOc/*.cpp)
add_library(libOc
    ${LIBOC_SRC}
)
include_directories(libOc libOc/)

# Build solutions
# It's ugly but it works

enable_testing()

set(SOLNS "solutions-25")

file(GLOB SOLN_SRCS ${SOLNS}/*)
foreach(src_dir ${SOLN_SRCS})
    get_filename_component(day ${src_dir} NAME_WE)
    set(src ${src_dir}/${day}.cpp)
    add_executable(${day} ${src})
    target_link_libraries(${name} libOc)
    file(GLOB day_tests ${src_dir}/tests/*.in)
    foreach(infile ${day_tests})
        get_filename_component(test ${infile} NAME_WE)
        add_test(
            NAME ${test}
            COMMAND bash -c "./${day} < ${infile} | diff -u ${src_dir}/tests/${test}.out -"
        )
    endforeach()
endforeach()
