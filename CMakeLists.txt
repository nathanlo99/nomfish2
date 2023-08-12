
cmake_minimum_required(VERSION 3.16)
project(nomfish VERSION 0.1.0)

# Enable exporting commands for use with vscode
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_EXPORT_COMPILE_COMMANDS true)
set(CMAKE_CXX_FLAGS "-pthread -Ofast -flto -ffast-math -Wall -Wextra -pedantic -Werror")
set(CMAKE_EXE_LINKER_FLAGS "-Ofast -flto -ffast-math")

file(GLOB_RECURSE SOURCES ${PROJECT_SOURCE_DIR}/chess/*.cpp)

include_directories(nomfish PUBLIC . tests)

add_executable(nomfish main.cpp ${SOURCES})
# set_target_properties(nomfish PROPERTIES COMPILE_FLAGS "-DLOG")
set_target_properties(nomfish PROPERTIES COMPILE_FLAGS "-DNDEBUG -DLOG_DIR=\\\"${CMAKE_SOURCE_DIR}/debug_logs/\\\" -DTABLEBASE_DIR=\\\"${CMAKE_SOURCE_DIR}/tablebases/\\\"")
