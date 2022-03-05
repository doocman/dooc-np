

cmake_minimum_required(VERSION 3.15)

set(CURRENT_TARGET_NAME dooc_named_parameters.tests)

enable_testing()

include(GoogleTest)

add_executable(${CURRENT_TARGET_NAME}
        named_args_tuple.ut.cpp
        )

target_link_libraries(${CURRENT_TARGET_NAME}
        gmock_main
        dooc::named_parameters
        )
set_property(TARGET ${CURRENT_TARGET_NAME} PROPERTY CXX_STANDARD 20)


gtest_discover_tests(${CURRENT_TARGET_NAME})
