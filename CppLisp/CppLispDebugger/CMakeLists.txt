cmake_minimum_required (VERSION 2.8)

project (FuelDebugger)

file(GLOB fuel_debugger_src
Debugger.h

Debugger.cpp
)

target_include_directories(
    FuelDebugger PUBLIC
    ../CppLispInterpreter
)

add_library(FuelDebugger SHARED ${fuel_debugger_src})

target_link_libraries(FuelDebugger FuelInterpreter)

install(TARGETS FuelDebugger DESTINATION lib)