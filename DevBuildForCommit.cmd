set MSBuild="%SystemRoot%\Microsoft.NET\Framework\v3.5\msbuild.exe"
%MSBuild% NemerleAll.nproj /target:DevBuildFull /p:Configuration=Debug /verbosity:n
pause