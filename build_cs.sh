cd CsLisp
msbuild /p:Configuration=ContinousIntgration CsLisp.sln
cd bin
cd ContinousIntgration
mono fuel.exe -v
mono fuel.exe -e "(println (platform))"
