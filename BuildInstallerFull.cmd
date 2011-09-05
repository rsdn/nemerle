set MSBuild="%SystemRoot%\Microsoft.NET\Framework\v3.5\msbuild.exe"

%MSBuild% NemerleAll.nproj /t:InstallerFull /p:Configuration=Release

pause
