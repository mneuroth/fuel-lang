set NDK_ROOT=C:\usr\android-ndk-r15c
set PATH=%NDK_ROOT%\toolchains\arm-linux-androideabi-4.9\prebuilt\windows-x86_64\bin;%PATH%
set SYS_ROOT=%NDK_ROOT%\platforms\android-16\arch-arm
set CC=arm-linux-androideabi-g++ --sysroot=%NDK_ROOT%\platforms\android-16\arch-arm  
rem -isystem C:\usr\android-ndk-r15c\sysroot\usr\include\arm-linux-androideabi -isystem C:\usr\android-ndk-r15c\sources\cxx-stl\gnu-libstdc++\4.9\include -isystem C:\usr\android-ndk-r15c\sources\cxx-stl\gnu-libstdc++\4.9\libs\armeabi-v7a\include
set LD=arm-linux-androideabi-ld
set AR=arm-linux-androideabi-ar
set RANLIB=arm-linux-androideabi-ranlib
set STRIP=arm-linux-androideabi-strip
set MY_INCLUDES=%NDK_ROOT%\sources\cxx-stl\gnu-libstdc++\4.9\include
set MY_INCLUDES2=%NDK_ROOT%\sources\cxx-stl\gnu-libstdc++\4.9\libs\armeabi\include
set MY_LIBS=%NDK_ROOT%\sources\cxx-stl\gnu-libstdc++\4.9\libs\armeabi
set CFLAGS=-O2 -std=gnu++11 -fPIC -fexceptions -D__ANDROID_API__=16 -DANDROID -march=armv7-a -mfloat-abi=softfp -DANDROID_STL=c++_static
rem --sysroot=%SYS_ROOT%
set LDFLAGS=-pie -L %NDK_ROOT%\sources\cxx-stl\gnu-libstdc++\4.9\libs\armeabi-v7a -L %NDK_ROOT%\toolchains\arm-linux-androideabi-4.9\prebuilt\windows-x86_64\lib\gcc\arm-linux-androideabi\4.9.x -lstdc++ -lgnustl_static -lgcc -llog -lz -lm -ldl -lc 

del *.o
%CC% -c %CFLAGS% -I %MY_INCLUDES% -I %MY_INCLUDES2% csobject.cpp
%CC% -c %CFLAGS% -I %MY_INCLUDES% -I %MY_INCLUDES2% csstring.cpp
%CC% -c %CFLAGS% -I %MY_INCLUDES% -I %MY_INCLUDES2% cstypes.cpp
%CC% -c %CFLAGS% -I %MY_INCLUDES% -I %MY_INCLUDES2% Debugger.cpp
%CC% -c %CFLAGS% -I %MY_INCLUDES% -I %MY_INCLUDES2% Environment.cpp
%CC% -c %CFLAGS% -I %MY_INCLUDES% -I %MY_INCLUDES2% Exception.cpp
%CC% -c %CFLAGS% -I %MY_INCLUDES% -I %MY_INCLUDES2% Interpreter.cpp
%CC% -c %CFLAGS% -I %MY_INCLUDES% -I %MY_INCLUDES2% Lisp.cpp
%CC% -c %CFLAGS% -I %MY_INCLUDES% -I %MY_INCLUDES2% Parser.cpp
%CC% -c %CFLAGS% -I %MY_INCLUDES% -I %MY_INCLUDES2% Scope.cpp
%CC% -c %CFLAGS% -I %MY_INCLUDES% -I %MY_INCLUDES2% Token.cpp
%CC% -c %CFLAGS% -I %MY_INCLUDES% -I %MY_INCLUDES2% Tokenizer.cpp
%CC% -c %CFLAGS% -I %MY_INCLUDES% -I %MY_INCLUDES2% Utils.cpp
%CC% -c %CFLAGS% -I %MY_INCLUDES% -I %MY_INCLUDES2% Variant.cpp
%CC% -c %CFLAGS% -I %MY_INCLUDES% -I %MY_INCLUDES2% fuel.cpp
%CC% fuel.o Variant.o Utils.o Tokenizer.o Token.o Scope.o Parser.o Lisp.o Interpreter.o Exception.o Environment.o Debugger.o cstypes.o csstring.o csobject.o -o fuel %LDFLAGS%
%STRIP% fuel

rem exit 0