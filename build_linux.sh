cd CppLisp
cd CppLispInterpreter
rm *.o
g++ -c -O2 -std=gnu++11 -fexceptions    csobject.cpp
g++ -c -O2 -std=gnu++11 -fexceptions    csstring.cpp
g++ -c -O2 -std=gnu++11 -fexceptions    cstypes.cpp
g++ -c -O2 -std=gnu++11 -fexceptions    Debugger.cpp
g++ -c -O2 -std=gnu++11 -fexceptions    Environment.cpp
g++ -c -O2 -std=gnu++11 -fexceptions    Exception.cpp
g++ -c -O2 -std=gnu++11 -fexceptions    Interpreter.cpp
g++ -c -O2 -std=gnu++11 -fexceptions    Lisp.cpp
g++ -c -O2 -std=gnu++11 -fexceptions    Parser.cpp
g++ -c -O2 -std=gnu++11 -fexceptions    Scope.cpp
g++ -c -O2 -std=gnu++11 -fexceptions    Token.cpp
g++ -c -O2 -std=gnu++11 -fexceptions    Tokenizer.cpp
g++ -c -O2 -std=gnu++11 -fexceptions    Utils.cpp
g++ -c -O2 -std=gnu++11 -fexceptions    Variant.cpp
g++ -c -O2 -std=gnu++11 -fexceptions    fuel.cpp
g++ fuel.o Variant.o Utils.o Tokenizer.o Token.o Scope.o Parser.o Lisp.o Interpreter.o Exception.o Environment.o Debugger.o cstypes.o csstring.o csobject.o -o fuel -lstdc++ -lm -lgcc

