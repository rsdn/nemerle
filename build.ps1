<#
.SYNOPSIS
    Canonical build entry for the Nemerle net10 retarget branch (Phase 5.2).

.DESCRIPTION
    Drives the classic-MSBuild orchestrator NemerleAll.nproj with a modern
    toolchain. No VsDevCmd, no Visual Studio registry probing: the build host
    is located via `-MsBuildExe`, the NEMERLE_MSBUILD environment variable, or
    vswhere (any recent Visual Studio with the MSBuild component).

    Host situation (probed 2026-10-31, see doc/NET10-BUILD-BASELINE.md):
      * `dotnet msbuild` (SDK 10) EVALUATES NemerleAll.nproj correctly
        (UseMSBuild=1), but running any stage fails with

          NemerleAll.nproj(157,5): error MSB4803: The task
          "GetFrameworkSdkPath" is not supported on the .NET Core version of
          MSBuild.

        GetFrameworkSdkPath lives in InitTools, which every stage depends on
        (Stage1 -> NTasks -> InitTools; Validate -> InitTools). Until Phase
        6.3 retires that ildasm discovery, the default host is the
        vswhere-discovered .NET Framework MSBuild.exe (>= 18 required by
        global.json). `-UseDotNetMsBuild` switches to `dotnet msbuild` for the
        day that blocker is gone.

    Package restore needs no separate step: NemerleAll.nproj invokes the
    `Restore` target on its project lists itself (Stage1, _Integration).

    Comma target lists work natively here: PowerShell passes "Stage3,Validate"
    to MSBuild as ONE argument. (cmd.exe used to split unquoted commas -- the
    quoting trap that made `Build-core-base.cmd` so awkward to call.)

.PARAMETER Configuration
    Debug or Release. Default: Debug.

.PARAMETER Targets
    MSBuild target list: an array (`-Targets Stage1,CompilerTests`) or a
    string with ',' or ';' separators. Default mirrors Build-core.cmd:
    Stage1;_PegAndCSharp;_ComputationExpressions;_Async;Install.

.PARAMETER NTargetName
    Target handed to the sub-projects: Build or Rebuild. Default: Build.

.PARAMETER Rebuild
    Shortcut for -NTargetName Rebuild (the standard green gate uses it:
    Stage3,Validate must be a forced Rebuild to prove the IL fixed point).

.PARAMETER NInstall
    Install directory for the `Install` target.
    Default: <repo>\bin\<Configuration>\net-4.0.

.PARAMETER Verbosity
    MSBuild verbosity (q|q|minimal|n|normal|d|detailed|diag). Default: m.

.PARAMETER MsBuildExe
    Explicit path to MSBuild.exe. Overrides NEMERLE_MSBUILD and vswhere.

.PARAMETER UseDotNetMsBuild
    Build via `dotnet msbuild` instead of MSBuild.exe. Currently fails at
    InitTools (MSB4803, see Remarks); provided for forward compatibility.

.PARAMETER DryRun
    Print the resolved command line and exit 0 without building.

.EXAMPLE
    ./build.ps1 -Targets Stage3,Validate -Rebuild   # standard green gate
.EXAMPLE
    ./build.ps1 -Targets Stage1,CompilerTests       # compiler regression suite
.EXAMPLE
    ./build.ps1 -DryRun                             # show the command only
#>
[CmdletBinding()]
param(
    [string]$Configuration = 'Debug',
    [object]$Targets = @('Stage1', '_PegAndCSharp', '_ComputationExpressions', '_Async', 'Install'),
    [string]$NTargetName = 'Build',
    [switch]$Rebuild,
    [string]$NInstall,
    [string]$Verbosity = 'm',
    [string]$MsBuildExe,
    [switch]$UseDotNetMsBuild,
    [switch]$DryRun
)
Set-StrictMode -Version 2.0
$ErrorActionPreference = 'Stop'

function Resolve-MsBuildExe {
    param([string]$Explicit)
    $candidates = @()
    if ($Explicit) { $candidates += , @($Explicit, '-MsBuildExe parameter') }
    if ($env:NEMERLE_MSBUILD) { $candidates += , @($env:NEMERLE_MSBUILD, 'NEMERLE_MSBUILD') }
    foreach ($c in $candidates) {
        if (Test-Path -LiteralPath $c[0] -PathType Leaf) { return $c[0] }
        Write-Warning "MSBuild '$($c[0])' (from $($c[1])) not found; continuing discovery."
    }
    $vswhere = Join-Path ${env:ProgramFiles(x86)} 'Microsoft Visual Studio\Installer\vswhere.exe'
    if (-not (Test-Path -LiteralPath $vswhere -PathType Leaf)) {
        throw "No MSBuild found: pass -MsBuildExe (or set NEMERLE_MSBUILD), or install Visual Studio with the MSBuild component so that vswhere.exe exists at '$vswhere'."
    }
    $vsPath = (& $vswhere -latest -products * -requires Microsoft.Component.MSBuild -property installationPath | Select-Object -First 1)
    if (-not $vsPath) {
        throw "vswhere.exe found no Visual Studio installation with the Microsoft.Component.MSBuild component. Pass -MsBuildExe explicitly."
    }
    foreach ($rel in @('MSBuild\Current\Bin\MSBuild.exe', 'MSBuild\15.0\Bin\MSBuild.exe')) {
        $candidate = Join-Path $vsPath $rel
        if (Test-Path -LiteralPath $candidate -PathType Leaf) { return $candidate }
    }
    throw "MSBuild.exe not found under Visual Studio installation '$vsPath'. Pass -MsBuildExe explicitly."
}

# --- Normalize parameters -------------------------------------------------------------------------

$repoRoot = $PSScriptRoot
$project  = Join-Path $repoRoot 'NemerleAll.nproj'
if (-not (Test-Path -LiteralPath $project -PathType Leaf)) {
    throw "NemerleAll.nproj not found next to build.ps1 ('$project')."
}

if ($Rebuild) { $NTargetName = 'Rebuild' }
if ([string]::IsNullOrWhiteSpace($NTargetName)) { $NTargetName = 'Build' }

# Targets: accept array (-Targets A,B) or string with ','/';' separators; MSBuild wants commas.
$targetList = @($Targets | ForEach-Object { [string]$_ } | ForEach-Object { $_ -split '[,;]' } | ForEach-Object { $_.Trim() } | Where-Object { $_ })
if ($targetList.Count -eq 0) { throw 'Target list is empty.' }
$targetsArg = $targetList -join ','

if ([string]::IsNullOrWhiteSpace($NInstall)) {
    $NInstall = Join-Path $repoRoot "bin\$Configuration\net-4.0"
}

# Required by NemerleAll.nproj property functions (same as the old .cmd entry).
$env:MSBUILDENABLEALLPROPERTYFUNCTIONS = '1'

# --- Compose and run ------------------------------------------------------------------------------

$msbuildArgs = @(
    $project
    "-t:$targetsArg"
    "-p:NTargetName=$NTargetName"
    "-p:Configuration=$Configuration"
    "-p:NInstall=$NInstall"
    "-v:$Verbosity"
    '-nodereuse:false'   # do not leave MSBuild nodes holding file locks between builds
)

if ($UseDotNetMsBuild) {
    $exe = 'dotnet'
    $exeArgs = @('msbuild') + $msbuildArgs
} else {
    $exe = Resolve-MsBuildExe -Explicit $MsBuildExe
    $exeArgs = $msbuildArgs
}

$display = (($(@($exe) + $exeArgs) | ForEach-Object { if ($_ -match '[ "]') { "`"$_`"" } else { $_ } }) -join ' ')
Write-Host "Nemerle build: Configuration=$Configuration NTargetName=$NTargetName"
Write-Host "  targets:  $targetsArg"
Write-Host "  install:  $NInstall"
Write-Host "  command:  $display"

if ($DryRun) {
    Write-Host 'DryRun: not executing.'
    exit 0
}

& $exe @exeArgs
$code = $LASTEXITCODE
if ($null -eq $code) { $code = 1 }

if ($code -eq 0) {
    Write-Host "Build succeeded. Nemerle installed to '$NInstall' (when the Install target ran)."
} else {
    Write-Host "Build FAILED with exit code $code."
}
exit $code
