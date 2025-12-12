cmake_minimum_required(VERSION 3.10)
project(aoc)

add_compile_options(-O3 -flto -std=c++23 -DNDEBUG)
add_link_options(-fuse-ld=lld)

file(GLOB LIBOC_SRC libOc/libOc/*.cpp)
add_library(libOc
    ${LIBOC_SRC}
)
include_directories(libOc libOc/)

# Z3
find_package(Z3
  REQUIRED
  CONFIG
  # `NO_DEFAULT_PATH` is set so that -DZ3_DIR has to be passed to find Z3.
  # This should prevent us from accidentally picking up an installed
  # copy of Z3. This is here to benefit Z3's build system when building
  # this project. When making your own project you probably shouldn't
  # use this option.
  # NO_DEFAULT_PATH
)
# add_executable(cpp_example example.cpp)

# target_include_directories(cpp_example PRIVATE ${Z3_CXX_INCLUDE_DIRS})
# target_link_libraries(cpp_example PRIVATE ${Z3_LIBRARIES})

add_compile_options(${Z3_COMPONENT_CXX_FLAGS})
# Build solutions
# It's ugly but it works

enable_testing()

set(SOLNS "solutions-25")

file(GLOB SOLN_SRCS ${SOLNS}/*)
foreach(src_dir ${SOLN_SRCS})
    get_filename_component(day ${src_dir} NAME_WE)
    set(src ${src_dir}/${day}.cpp)
    add_executable(${day} ${src})
    # target_include_directories(${name} PRIVATE ${Z3_CXX_INCLUDE_DIRS})
    target_link_libraries(${day} libOc ${Z3_LIBRARIES})
    file(GLOB day_tests ${src_dir}/tests/*.in)
    foreach(infile ${day_tests})
        get_filename_component(test ${infile} NAME_WE)
        add_test(
            NAME ${test}
            COMMAND bash -c "./${day} < ${infile} | diff -u ${src_dir}/tests/${test}.out -"
        )
    endforeach()
endforeach()
