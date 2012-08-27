set MSBuild="%SystemRoot%\Microsoft.NET\Framework\v3.5\msbuild.exe"

%MSBuild% NemerleAll.nproj /tv:3.5 /p:TargetFrameworkVersion=v3.5 /t:%*
