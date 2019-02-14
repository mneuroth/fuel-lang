cmake_minimum_required (VERSION 2.6)

project (FuelInterpreter)

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