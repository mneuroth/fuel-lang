cd CppLisp
cd CppLispInterpreter
qmake "QMAKE_CXX = clang++" CppLispInterpreter.pro
make 
./fuel -v
./fuel  -e "(println (platform))"
#ls -l
cd ..
cd QtLispInterpreterUnitTests
qmake QtLispInterpreterUnitTests.pro
make 
./tst_qtlispinterpreterunitteststest
#ls -l
