cd CppLisp
cd CppLispInterpreter
qmake CppLispInterpreter.pro
make 
#rm *.o
#$CXX -c -O2 -std=gnu++11 -fexceptions    csobject.cpp
#$CXX -c -O2 -std=gnu++11 -fexceptions    csstring.cpp
#$CXX -c -O2 -std=gnu++11 -fexceptions    cstypes.cpp
#$CXX -c -O2 -std=gnu++11 -fexceptions    Debugger.cpp
#$CXX -c -O2 -std=gnu++11 -fexceptions    Environment.cpp
#$CXX -c -O2 -std=gnu++11 -fexceptions    Exception.cpp
#$CXX -c -O2 -std=gnu++11 -fexceptions    Interpreter.cpp
#$CXX -c -O2 -std=gnu++11 -fexceptions    Lisp.cpp
#$CXX -c -O2 -std=gnu++11 -fexceptions    Parser.cpp
#$CXX -c -O2 -std=gnu++11 -fexceptions    Scope.cpp
#$CXX -c -O2 -std=gnu++11 -fexceptions    Token.cpp
#$CXX -c -O2 -std=gnu++11 -fexceptions    Tokenizer.cpp
#$CXX -c -O2 -std=gnu++11 -fexceptions    Utils.cpp
#$CXX -c -O2 -std=gnu++11 -fexceptions    Variant.cpp
#$CXX -c -O2 -std=gnu++11 -fexceptions    fuel.cpp
#$CXX fuel.o Variant.o Utils.o Tokenizer.o Token.o Scope.o Parser.o Lisp.o Interpreter.o Exception.o Environment.o Debugger.o cstypes.o csstring.o csobject.o -o fuel -lstdc++ -lm -lgcc
./fuel -v
cd ..
cd QtLispInterpreterUnitTests
qmake QtLispInterpreterUnitTests.pro
make 
./tst_qtlispinterpreterunitteststest
