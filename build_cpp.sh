cd CppLisp
if [[ $CXX == clang* ]]; then
    qmake -r "QMAKE_CXX = clang++" "QMAKE_LINK=clang++" CppLisp.pro
else
    qmake -r CppLisp.pro
fi
make -j 4
ls -lrt
cd CppLispInterpreter
ls -lrt
./fuel -v
./fuel  -e "(println (platform))"
cd ..
cd QtLispInterpreterUnitTests
if [[ $CXX == clang* ]]; then
    qmake "QMAKE_CXX = clang++" "QMAKE_LINK=clang++" QtLispInterpreterUnitTests.pro
else
    qmake QtLispInterpreterUnitTests.pro
fi
make -j 4 
#ls -lrt
#ls -lrt .. 
./tst_qtlispinterpreterunitteststest
#ls -lrt
#ls -lrt Library
#qmake -v
