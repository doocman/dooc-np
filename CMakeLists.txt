
cmake_minimum_required(VERSION 3.4)

project(DoocNP VERSION 0.1.0 LANGUAGES CXX)

option(DOOC_NP_TESTS "If ON, includes tests for DoocNP. Needs gtest somehow." OFF)
set(DOOC_NP_TESTS_GTEST download CACHE STRING "If DOOC_NP_TESTS: specifies how google test should be included in the project")

add_library(dooc_named_parameters INTERFACE)
add_library(dooc::named_parameters ALIAS dooc_named_parameters)

target_include_directories(dooc_named_parameters
        INTERFACE
        include
        )

if(DOOC_NP_TESTS)
    set(CMAKE_CXX_STANDARD 20)
    if("${DOOC_NP_TESTS_GTEST}" STREQUAL "download")
        # This code is copied directly from google.github (url as of writing:
        #  https://google.github.io/googletest/quickstart-cmake.html
        # Only commit-SHA has been changed.
        include(FetchContent)
        FetchContent_Declare(
                googletest
                URL https://github.com/google/googletest/archive/e2239ee6043f73722e7aa812a459f54a28552929.zip
        )
        # For Windows: Prevent overriding the parent project's compiler/linker settings
        set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
        FetchContent_MakeAvailable(googletest)
    endif()
    add_subdirectory(tests)
endif()
