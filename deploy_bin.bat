ren call with two parameters: OS APPLICATION_VERSION BUILD_NO PROCESSOR_ARCHITECTURE
rem see: https://stackoverflow.com/questions/20353729/how-can-i-build-a-git-tag-in-teamcity
mkdir deploy
cd deploy
mkdir %1
cd %1
mkdir %2
cd %2
mkdir %3
cd %3
copy ..\..\..\..\CsLisp\bin\release\fuel.exe
copy ..\..\..\..\CsLisp\bin\release\fuel.pdb
copy ..\..\..\..\CsLisp\bin\release\fuel.exe.config
copy ..\..\..\..\CsLisp\bin\release\FuelInterpreter.dll
copy ..\..\..\..\CsLisp\bin\release\FuelInterpreter.pdb
copy ..\..\..\..\CsLisp\bin\release\FuelCompiler.dll
copy ..\..\..\..\CsLisp\bin\release\FuelCompiler.pdb
copy ..\..\..\..\CsLisp\bin\release\FuelDebugger.dll
copy ..\..\..\..\CsLisp\bin\release\FuelDebugger.pdb
mkdir Library
cd Library
copy ..\..\..\..\..\CsLisp\bin\release\Library\fuellib.fuel
