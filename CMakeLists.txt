cmake_minimum_required(VERSION 3.20)
project(IteratorTools)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_FLAGS "-Wall -Wextra -Wpedantic -fsanitize=address")

add_executable(IteratorTools main.cpp)
