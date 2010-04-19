set MSBuild="%SystemRoot%\Microsoft.NET\Framework\v4.0.30319\msbuild.exe"

%MSBuild% NemerleAll-4.0.nproj /t:InstallerFull /p:Configuration=Release
