#          Copyright Robin Söderholm 2021 - 2022.
# Distributed under the Boost Software License, Version 1.0.
#    (See accompanying file LICENSE_1_0.txt or copy at
#          https://www.boost.org/LICENSE_1_0.txt)

cmake_minimum_required(VERSION 3.4...3.22)

set(CURRENT_TARGET_NAME dooc_named_parameters.tests)

enable_testing()

include(GoogleTest)

if(${CMAKE_CXX_COMPILER_ID} MATCHES ".*Clang" AND NOT WIN32)
        add_compile_options(-ftemplate-backtrace-limit=0)
endif ()

add_executable(${CURRENT_TARGET_NAME}
        named_args_tuple.ut.cpp
        )

target_link_libraries(${CURRENT_TARGET_NAME}
        gmock_main
        dooc::named_parameters
        )
set_property(TARGET ${CURRENT_TARGET_NAME} PROPERTY CXX_STANDARD 20)

gtest_discover_tests(${CURRENT_TARGET_NAME})
