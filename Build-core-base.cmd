@echo off
rem Phase 5.2: thin wrapper around build.ps1 (the canonical build entry).
rem Contract kept for docs/AGENTS.md and the Build-core*.cmd wrappers:
rem     Build-core-base.cmd <Config> <Target> "<Targets>"
rem build.ps1 locates the MSBuild host (vswhere; no VsDevCmd, no VS registry)
rem and passes comma-joined target lists natively (no cmd.exe comma-splitting).

SET Name=Nemerle
SET Config=%~1
SET Target=%~2
SET Features=%~3

IF "%~1"=="" (
@echo The parameter 1 [Config] is empty. Use Debug or Release.
EXIT /B -1
)

IF "%~2"=="" (
@echo The parameter 2 [Target] is empty. Use Build or Rebuild.
EXIT /B -1
)

IF "%~3"=="" (
@echo The parameter 3 [Features] is empty.
EXIT /B -1
)

@echo Starting build %Name% Target=%Target% Config=%Config% Features=%Features%

powershell -NoProfile -ExecutionPolicy Bypass -File "%~dp0build.ps1" -Configuration "%Config%" -NTargetName "%Target%" -Targets "%Features%"
SET NemerleBuildExitCode=%ERRORLEVEL%
@echo Nemerle installed to "%~dp0bin\%Config%\net-4.0"
EXIT /B %NemerleBuildExitCode%
rem pause
