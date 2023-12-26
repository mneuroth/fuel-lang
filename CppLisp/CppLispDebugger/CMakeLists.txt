cmake_minimum_required (VERSION 3.1)

project (FuelDebugger)

set (CMAKE_CXX_STANDARD 11)
set (CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS ON)

add_compile_definitions(_DBGEXPORTING=1)

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

if (CMAKE_SYSTEM_NAME MATCHES "Android")
    add_custom_command(TARGET FuelDebugger POST_BUILD
            COMMAND "${ANDROID_TOOLCHAIN_PREFIX}strip" -g -S -d --strip-debug --verbose
            libFuelDebugger.so
            COMMENT "Strip debug symbols done on final FuelDebugger binary.")
endif()
