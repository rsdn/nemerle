set MSBuild="%SystemRoot%\Microsoft.NET\Framework\v4.0.30319\msbuild.exe"

%MSBuild% NemerleAll.nproj /t:InstallerFast /tv:4.0 /p:TargetFrameworkVersion=v4.0;Configuration=Release