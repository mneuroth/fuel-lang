set PACK_EXE="C:\Program Files\7-Zip\7z.exe"
rem call setvs.bat
cd CsLisp
MsBuild.exe /m /property:Configuration="Release" /t:Clean;Build CsLisp.sln   
cd bin\release
fuel -v
%PACK_EXE% a fuel-lang-csharp-bin.zip fuel.exe fuel.exe.config FuelInterpreter.dll FuelDebugger.dll FuelCompiler.dll Library\fuellib.fuel
%PACK_EXE% l fuel-lang-csharp-bin.zip
dir 
cd ..\..
cd CsLispUnitTests
cd bin\release
vstest.console FuelUnitTests.dll 
cd ..\..\..\..
cd CppLisp
MsBuild.exe /m /property:Configuration="Release" /t:Clean;Build CppLisp.sln
cd x64\release
fuel -v
%PACK_EXE% a fuel-lang-msc64-bin.zip fuel.exe FuelInterpreter.dll FuelDebugger.dll Library\fuellib.fuel
%PACK_EXE% l fuel-lang-msc64-bin.zip
vstest.console CppLispInterpreterTests.dll 
cd ..\..\..
