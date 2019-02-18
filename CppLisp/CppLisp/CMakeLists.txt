cmake_minimum_required (VERSION 2.6)

project (fuel)

add_executable(fuel FuelMain.cpp
../CppLispInterpreter/csobject.h
../CppLispInterpreter/csstring.h
../CppLispInterpreter/cstypes.h
../CppLispInterpreter/Token.h
../CppLispInterpreter/Tokenizer.h
../CppLispInterpreter/Parser.h
../CppLispInterpreter/Utils.h
../CppLispInterpreter/Exception.h
../CppLispInterpreter/Variant.h
../CppLispInterpreter/Scope.h
../CppLispInterpreter/Environment.h
../CppLispInterpreter/Interpreter.h
../CppLispInterpreter/DebuggerInterface.h
../CppLispInterpreter/Lisp.h
../CppLispInterpreter/fuel.h
../CppLispDebugger/Debugger.h

../CppLispInterpreter/csobject.cpp
../CppLispInterpreter/csstring.cpp
../CppLispInterpreter/cstypes.cpp
../CppLispInterpreter/Token.cpp
../CppLispInterpreter/Tokenizer.cpp
../CppLispInterpreter/Parser.cpp
../CppLispInterpreter/Utils.cpp
../CppLispInterpreter/Exception.cpp
../CppLispInterpreter/Variant.cpp
../CppLispInterpreter/Scope.cpp
../CppLispInterpreter/Environment.cpp
../CppLispInterpreter/Interpreter.cpp
../CppLispInterpreter/Lisp.cpp
../CppLispInterpreter/fuel.cpp
../CppLispDebugger/Debugger.cpp
)

install(TARGETS fuel DESTINATION bin)