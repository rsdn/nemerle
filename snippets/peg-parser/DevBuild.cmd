set MSBuild="%SystemRoot%\Microsoft.NET\Framework\v3.5\msbuild.exe"
set NoPause=true
%MSBuild% Nemerle.Peg.sln /target:Build /p:Configuration=Debug /verbosity:n
rem /p:NTargetName=Build
rem IF %errorlevel% == 0 xcopy Calculator\bin\Debug\Nemerle.Peg.* "%ProgramFiles%\Nemerle" /Y
pause