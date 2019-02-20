#!/bin/bash
# *** simple bash script to build the fuel application with qmake ***
if [ -z "$1" ]; then
    PLATFORM="unix"
else
    PLATFORM=$1
fi

# *** build the fuel application ***
#===================================
cd CppLisp
if [[ $CXX == clang* ]]; then
    qmake -r "QMAKE_CXX = clang++" "QMAKE_LINK=clang++" CppLisp.pro
else
    qmake -r CppLisp.pro
fi
make -j 4

# *** create a binary distribution for the fuel application ***
#==============================================================
cd CppLisp
cp ../CppLispInterpreter/libFuelInterpreter.* .
cp ../CppLispDebugger/libFuelDebugger.* .
mkdir Library
cp ../../Library/fuellib.fuel Library
zip -u fuel-lang-$PLATFORM-bin.zip fuel libFuelInterpreter.* libFuelDebugger.* Library/*.fuel
#unzip -v fuel-lang-$PLATFORM-bin.zip

# *** very simple start and exit test for the fuel application ***
#=================================================================
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:.
./fuel -v
./fuel -e "(println (platform))"
cd ..

# *** run the unit tests for the fuel application ***
#====================================================
cd QtLispInterpreterUnitTests
if [[ $CXX == clang* ]]; then
    qmake "QMAKE_CXX = clang++" "QMAKE_LINK=clang++" QtLispInterpreterUnitTests.pro
else
    qmake QtLispInterpreterUnitTests.pro
fi
make -j 4 
./tst_qtlispinterpreterunitteststest
