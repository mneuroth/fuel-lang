pwd
msbuild
which msbuild
which mono
ls -lrt
cd CsLisp
# msbuild
#MsBuild.exe /p:Configuration=Release CsLisp.sln
msbuild /p:Configuration=Release CsLisp.sln
ls -lrt
cd bin
cd Release
# chmod +x fuel.exe
ls -lrt
mono fuel.exe -v
mono fuel.exe -e "(println (platform))"
