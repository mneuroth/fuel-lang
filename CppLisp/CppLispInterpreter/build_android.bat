set NDK_ROOT=C:\usr\android-ndk-r18b
set PATH=%NDK_ROOT%\toolchains\arm-linux-androideabi-4.9\prebuilt\windows-x86_64\bin;%PATH%
set SYS_ROOT=%NDK_ROOT%\platforms\android16\arch-arm
set CC=arm-linux-androideabi-g++ --sysroot=C:\usr\android-ndk-r18b\platforms\android16\arch-arm 
rem -isystem C:\usr\android-ndk-r18b\sysroot\usr\include\arm-linux-androideabi -isystem C:\usr\android-ndk-r18b\sources\cxx-stl\gnu-libstdc++\4.9\include -isystem C:\usr\android-ndk-r18b\sources\cxx-stl\gnu-libstdc++\4.9\libs\armeabi-v7a\include
set LD=arm-linux-androideabi-ld
set AR=arm-linux-androideabi-ar
set RANLIB=arm-linux-androideabi-ranlib
set STRIP=arm-linux-androideabi-strip
set MY_INCLUDES=%NDK_ROOT%\sources\cxx-stl\llvm-libc++\include
set MY_INCLUDES2=%NDK_ROOT%\sources\cxx-stl\llvm-libc++abi\libs\armeabi\include
set MY_LIBS=%NDK_ROOT%\sources\cxx-stl\gnu-libstdc++\4.9\libs\armeabi
set CFLAGS=-std=gnu++11 -fPIC --sysroot=%SYS_ROOT% -I C:\usr\android-ndk-r18b\sysroot\usr\include
set LDFLAGS=-pie

del *.o
rem %CC% --help
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
%CC% -c %CFLAGS% -I %MY_INCLUDES% -I %MY_INCLUDES2% fuel.o Variant.o Utils.o Tokenizer.o Token.o Scope.o Parser.o Lisp.o Interpreter.o Exception.o Environment.o Debug.o cstypes.o csstring.o csobject.o -o fuel
rem mingw32-make -j 4 -f makefile.unx -e android_win=1 release=1
rem copy minscript minscript_O2_android_arm
rem C:\usr\android-ndk-r15c\toolchains\arm-linux-androideabi-4.9\prebuilt\windows-x86_64\bin\arm-linux-androideabi-strip minscript_O2_android_arm
rem rem del *.o
rem rem mingw32-make -f makefile.unx -e android=1 debug=1
rem rem copy minscript minscript_d_android

rem exit 0