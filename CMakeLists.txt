cmake_minimum_required(VERSION 3.16)
project(lisp C)

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_C_EXTENSIONS OFF)

add_executable(lisp src/lisp.c)

target_compile_options(lisp PRIVATE
    -Wall
    -Wextra
    -Wpedantic
    -Wno-unused-parameter
)
