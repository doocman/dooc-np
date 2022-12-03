#          Copyright Robin S�derholm 2021 - 2022.
# Distributed under the Boost Software License, Version 1.0.
#    (See accompanying file LICENSE_1_0.txt or copy at
#          https://www.boost.org/LICENSE_1_0.txt)

cmake_minimum_required(VERSION 3.4...3.22)

project(DoocNP VERSION 0.1.0)

set(DOOC_NP_TESTS OFF CACHE BOOL "If ON, includes tests for DoocNP. Needs gtest somehow.")
set(DOOC_NP_PRECOMPILED_HEADER OFF CACHE BOOL "If ON, add an extra 'dooc::named_parameters_precomiled' and dooc_named_parameters_precompiled target that have precompiled versions of the header files")
set(DOOC_NP_TESTS_GTEST download CACHE STRING "If DOOC_NP_TESTS: specifies how google test should be included in the project")

add_library(dooc_named_parameters INTERFACE)
add_library(dooc::named_parameters ALIAS dooc_named_parameters)

target_include_directories(dooc_named_parameters
        INTERFACE
        include
        )

if(DOOC_NP_PRECOMPILED_HEADER)
    add_library(dooc_named_parameters_precompiled INTERFACE)
    add_library(dooc::named_parameters_precompiled ALIAS dooc_named_parameters_precompiled)

    target_link_libraries(dooc_named_parameters_precompiled
            INTERFACE
            dooc::named_parameters
            )
    target_precompile_headers(dooc_named_parameters_precompiled
            INTERFACE
            include/dooc/named_args_tuple.hpp
            )
endif()

if(DOOC_NP_TESTS)
    set(CMAKE_CXX_STANDARD 20)
    enable_testing()
    if("${DOOC_NP_TESTS_GTEST}" STREQUAL "download")
        # This code is copied directly from google.github (url as of writing:
        #  https://google.github.io/googletest/quickstart-cmake.html
        # Only commit-SHA has been changed.
        include(FetchContent)
        FetchContent_Declare(
                googletest
                URL https://github.com/google/googletest/archive/c9461a9b55ba954df0489bab6420eb297bed846b.zip
        )
        # For Windows: Prevent overriding the parent project's compiler/linker settings
        set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
        FetchContent_MakeAvailable(googletest)
    endif()
    add_subdirectory(tests)
endif()
