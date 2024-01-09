# Add .cpp files in the project root directory as source files
set(SOURCE_FILES src/lisp.c)

# Add project name
set(PROJECT_NAME lisp)
# add_definitions(-w)

###########################################
# Do not change anything below this line
###########################################

cmake_minimum_required(VERSION 3.16)  # CMake version check
set(C_STANDARD 90)            # Enable 90 standard
project(${PROJECT_NAME} C) 

# Add executable target with source files listed in SOURCE_FILES variable
add_executable(${PROJECT_NAME} ${SOURCE_FILES})