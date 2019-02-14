cmake_minimum_required (VERSION 2.6)

project (FuelDebugger)

file(GLOB fuel_debugger_src
Debugger.h

Debugger.cpp
)

add_library(FuelDebugger SHARED ${fuel_debugger_src})

target_link_libraries(FuelDebugger FuelInterpreter)

install(TARGETS FuelDebugger DESTINATION lib)