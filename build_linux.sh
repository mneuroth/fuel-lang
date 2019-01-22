cd CppLisp
cd CppLispInterpreter
if [ "$CXX" == "clang++" ]; then
    qmake "QMAKE_CXX = clang++" CppLispInterpreter.pro
else
    qmake CppLispInterpreter.pro
fi
make 
./fuel -v
./fuel  -e "(println (platform))"
#ls -l
cd ..
cd QtLispInterpreterUnitTests
if [ "$CXX" == "clang++" ]; then
    qmake "QMAKE_CXX = clang++" QtLispInterpreterUnitTests.pro
else
    qmake QtLispInterpreterUnitTests.pro
fi
make 
./tst_qtlispinterpreterunitteststest
#ls -l
