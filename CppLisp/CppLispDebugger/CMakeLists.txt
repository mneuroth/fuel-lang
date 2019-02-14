cmake_minimum_required (VERSION 3.1)

project (FuelDebugger)

set (CMAKE_CXX_STANDARD 11)

file(GLOB fuel_debugger_src
Debugger.h

Debugger.cpp
)

include_directories(
    ../CppLispInterpreter
)

add_library(FuelDebugger SHARED ${fuel_debugger_src})

target_link_libraries(FuelDebugger FuelInterpreter)

install(TARGETS FuelDebugger DESTINATION lib)