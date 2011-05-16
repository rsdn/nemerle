set MSBuild="%SystemRoot%\Microsoft.NET\Framework\v3.5\msbuild.exe"
set NoPause=true
%MSBuild% NemerleAll.nproj /target:DevBuildQuickNccOnly /p:Configuration=Debug /verbosity:n /p:NTargetName=Build
