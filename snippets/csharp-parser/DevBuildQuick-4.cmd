set MSBuild="%SystemRoot%\Microsoft.NET\Framework\v4.0.30319\msbuild.exe"
set NoPause=true
%MSBuild% CSharpParser-VS-2010.sln /p:Configuration=Debug /verbosity:n /t:ReBuild  /tv:4.0 /p:TargetFrameworkVersion=v4.0
pause