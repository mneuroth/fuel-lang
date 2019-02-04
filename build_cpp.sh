cd CppLisp
cd CppLispInterpreter
if [[ $CXX == clang* ]]; then
    qmake "QMAKE_CXX = clang++" "QMAKE_LINK=clang++" CppLispInterpreter.pro
else
    qmake CppLispInterpreter.pro
fi
make 
./fuel -v
./fuel  -e "(println (platform))"
cd ..
cd QtLispInterpreterUnitTests
if [[ $CXX == clang* ]]; then
    qmake "QMAKE_CXX = clang++" "QMAKE_LINK=clang++" QtLispInterpreterUnitTests.pro
else
    qmake QtLispInterpreterUnitTests.pro
fi
make 
./tst_qtlispinterpreterunitteststest
ls -lrt
ls .. -lrt
