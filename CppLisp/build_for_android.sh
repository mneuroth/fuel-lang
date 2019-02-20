#!/bin/bash
# exampe of usage: > ./build_for_android.sh /home/min65/Dokumente/android-ndk-r19b/ armeabi-v7a
# ABI are: armeabi, armeabi-v7a, arm64-v8a, x86, x86_64
if [ $# -ne 2 ]
  then
    echo "this script needs two arguments: <path_to_android_ndk> <abi>"
    exit -1
fi
ANDROID_NDK_HOME=$1
ANDROID_ABI=$2
echo *** Build fuel with shared libraries ***
cmake -DCMAKE_TOOLCHAIN_FILE=$ANDROID_NDK_HOME/build/cmake/android.toolchain.cmake -DANDROID_ABI=$ANDROID_ABI CMakeLists.txt
make clean
make -j 4
cp CppLispInterpreter/libFuelInterpreter.* .
cp CppLispDebugger/libFuelDebugger.* .
mkdir Library
cp ../Library/fuellib.fuel Library
strip fuel
zip -u fuel-lang-android-$ANDROID_ABI-bin.zip fuel libFuelInterpreter.* libFuelDebugger.* Library/fuellib.fuel
echo *** Build fuel with shared libraries ***
cd CppLisp
cmake -DCMAKE_TOOLCHAIN_FILE=$ANDROID_NDK_HOME/build/cmake/android.toolchain.cmake -DANDROID_ABI=$ANDROID_ABI CMakeLists.txt
make clean
make -j 4
mkdir Library
cp ../../Library/fuellib.fuel Library
strip fuel
zip -u fuel-lang-single-android-$ANDROID_ABI-bin.zip fuel Library/fuellib.fuel
cd ..
