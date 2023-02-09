cmake_minimum_required(VERSION 3.10)
project(IteratorTools)

option(BUILD_TESTS "Build unit tests" OFF)

add_library(${PROJECT_NAME} INTERFACE)
target_sources(${PROJECT_NAME} INTERFACE ${CMAKE_CURRENT_SOURCE_DIR}/include/Iterators.hpp)
target_include_directories(${PROJECT_NAME} INTERFACE ${CMAKE_CURRENT_SOURCE_DIR}/include)

if (${BUILD_TESTS})
    set(CMAKE_CXX_STANDARD 17)
    set(CMAKE_CXX_FLAGS "-Wall -Wpedantic -Wextra")
    if (CMAKE_BUILD_TYPE STREQUAL "Debug")
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fsanitize=address -fno-omit-frame-pointer")
    else ()
        message("Disabling address sanitizer when building for release")
    endif()
    add_subdirectory(Test)
endif ()
