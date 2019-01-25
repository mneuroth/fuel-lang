pwd
ls -lrt
cd CsLisp
# msbuild
MsBuild.exe /p:Configuration=Release CsLisp.sln
ls -lrt
cd bin
Release
# chmod +x fuel.exe
ls -lrt
cd bin
cd Release
ls -lrt
mono fuel.exe -v
mono fuel.exe -e "(println (platform))"
