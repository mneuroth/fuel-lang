cd CppLisp
cd CppLispInterpreter
echo $CXX
if [[ $CXX == clang* ]]; then
    qmake "QMAKE_CXX = clang++" "QMAKE_LINK=clang++" CppLispInterpreter.pro
else
    qmake CppLispInterpreter.pro
fi
$CXX -v
make 
./fuel -v
./fuel  -e "(println (platform))"
#ls -l
cd ..
cd QtLispInterpreterUnitTests
if [[ $CXX == clang* ]]; then
    qmake "QMAKE_CXX = clang++" "QMAKE_LINK=clang++" QtLispInterpreterUnitTests.pro
else
    qmake QtLispInterpreterUnitTests.pro
fi
make 
./tst_qtlispinterpreterunitteststest
#ls -l
