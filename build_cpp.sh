cd CppLisp
if [[ $CXX == clang* ]]; then
    qmake -r "QMAKE_CXX = clang++" "QMAKE_LINK=clang++" CppLisp.pro
else
    qmake -r CppLisp.pro
fi
make -j 4
ls -lrt
cd CppLisp
ls -lrt
cp ../CppLispInterpreter/libFuelInterpreter.*
cp ../CppLispDebugger/libFuelDebugger.*
mkdir Library
cp ../../Library/fuellib.fuel Library
zip -u fuel-lang-unix-bin.zip fuel libFuelInterpreter.* libFuelDebugger.* Library/*.fuel
unzip -v fuel-lang-unix-bin.zip
#cp fuel ../CppLispInterpreter
#cd ..
#cd CppLispInterpreter
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:.
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
