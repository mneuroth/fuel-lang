cd CppLisp
cd CppLispInterpreter
qmake CppLispInterpreter.pro
make 
./fuel -v
ls -l
cd ..
cd QtLispInterpreterUnitTests
qmake QtLispInterpreterUnitTests.pro
make 
./tst_qtlispinterpreterunitteststest
ls -l
