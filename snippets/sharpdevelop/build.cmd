@echo off
set MSBuild="%SystemRoot%\Microsoft.NET\Framework\v3.5\msbuild.exe"
%MSBuild% "%~dp0\Nemerle.SharpDevelop\Nemerle.SharpDevelop.nproj" /p:Configuration=Release /t:Rebuild
pause