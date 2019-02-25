cmake_minimum_required (VERSION 3.1)

project (FuelInterpreter)

set (CMAKE_CXX_STANDARD 11)

file(GLOB fuel_interpreter_src
csobject.h
csstring.h
cstypes.h
Token.h
Tokenizer.h
Parser.h
Utils.h
Exception.h
Variant.h
Scope.h
Environment.h
Interpreter.h
DebuggerInterface.h
Lisp.h
fuel.h

csobject.cpp
csstring.cpp
cstypes.cpp
Token.cpp
Tokenizer.cpp
Parser.cpp
Utils.cpp
Exception.cpp
Variant.cpp
Scope.cpp
Environment.cpp
Interpreter.cpp
Lisp.cpp
fuel.cpp
)

add_library(FuelInterpreter SHARED ${fuel_interpreter_src})

install(TARGETS FuelInterpreter DESTINATION lib)

if (CMAKE_SYSTEM_NAME MATCHES "Android")
    add_custom_command(TARGET FuelInterpreter POST_BUILD
            COMMAND "${ANDROID_TOOLCHAIN_PREFIX}strip" -g -S -d --strip-debug --verbose
            libFuelInterpreter.so
            COMMENT "Strip debug symbols done on final FuelInterpreter binary.")
endif()
