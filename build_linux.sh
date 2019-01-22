cd CppLisp
cd CppLispInterpreter
qmake CppLispInterpreter.pro
make 
g++ -v
gcc -v
./fuel -v
./fuel  -e "(println (platform))"
#ls -l
cd ..
cd QtLispInterpreterUnitTests
qmake QtLispInterpreterUnitTests.pro
make 
./tst_qtlispinterpreterunitteststest
#ls -l
