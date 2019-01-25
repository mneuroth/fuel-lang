cd CsLisp
msbuild /p:Configuration=Release /p:ExcludeFromBuild=Test CsLisp.sln
cd bin
cd Release
mono fuel.exe -v
mono fuel.exe -e "(println (platform))"
