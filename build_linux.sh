cd CppLisp
cd CppLispInterpreter
echo $CXX
if [[ $CXX == clang* ]]; then
    echo "use clang++"
    qmake "QMAKE_CXX = clang++" CppLispInterpreter.pro
else
    echo "use g++"
    qmake CppLispInterpreter.pro
fi
make 
./fuel -v
./fuel  -e "(println (platform))"
#ls -l
cd ..
cd QtLispInterpreterUnitTests
if [[ $CXX == clang* ]]; then
    qmake "QMAKE_CXX = clang++" QtLispInterpreterUnitTests.pro
else
    qmake QtLispInterpreterUnitTests.pro
fi
make 
./tst_qtlispinterpreterunitteststest
#ls -l
