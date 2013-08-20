set MSBuild="%ProgramFiles(x86)%\MSBuild\12.0\Bin\MSBuild.exe"

%MSBuild% NemerleAll.nproj /t:InstallerFast /tv:4.0 /p:TargetFrameworkVersion=v4.5.1;Configuration=Release