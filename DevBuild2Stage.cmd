set MSBuild="%SystemRoot%\Microsoft.NET\Framework\v3.5\msbuild.exe"
%MSBuild% NemerleAll.nproj /target:DevBuild2Stage /p:Configuration=Debug /verbosity:n 
rem /p:NTargetName=Build
pause