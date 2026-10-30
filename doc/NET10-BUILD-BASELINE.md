# Known-good build baseline on this machine (pre-net10)

Verified 2026-10-30 at tag `pre-net10-baseline` (commit d9b50f721), branch `retarget-compiler`,
checkout `C:\RSDN\nemerle`. The legacy `Build-core*.cmd` scripts originally required VS2017 in the
registry; since Phase 1 they locate MSBuild via vswhere. With `global.json` (sdk 10.0.402)
**MSBuild >= 18 is required** — VS2022's msbuild 17.14 can no longer build the repo (it fails with
"requires at least version 18.0.0 of MSBuild"); use `Build-core-base.cmd` or VS2026 msbuild.

## Commands (verified green, Phase 1)

```powershell
# From the repo root. NOTE: cmd.exe splits unquoted commas — always pass target lists quoted.
cmd /c "`"C:\RSDN\nemerle\Build-core-base.cmd`" Debug Build `"Stage3,Validate`""
cmd /c "`"C:\RSDN\nemerle\Build-core-base.cmd`" Debug Build `"Stage1,CompilerTests`""
```

Equivalent direct invocation (bypasses the wrapper): resolve MSBuild via
`vswhere -latest -products * -requires Microsoft.Component.MSBuild -property installationPath`
then `<path>\MSBuild\Current\Bin\MSBuild.exe`, and call it with
`NemerleAll.nproj /t:<Targets> /p:NTargetName=Build /p:Configuration=Debug /p:NInstall=bin\Debug\net-4.0`.
CompilerTests must run in the same msbuild session as Stage1 (combined target `Stage1,CompilerTests`),
otherwise `$(Nemerle)` is empty and project imports fail.

### Historical Phase-0 baseline commands (obsolete since global.json; kept for the record)

```powershell
$msb  = 'C:\Program Files\Microsoft Visual Studio\2022\Professional\MSBuild\Current\Bin\MSBuild.exe'
$proj = 'C:\RSDN\nemerle\NemerleAll.nproj'
$ip   = 'C:\RSDN\nemerle\bin\Debug\net-4.0'
& $msb $proj /t:Stage3,Validate /p:NTargetName=Build /p:Configuration=Debug "/p:NInstall=$ip" /v:m
& $msb $proj /t:Stage1,CompilerTests /p:NTargetName=Build /p:Configuration=Debug "/p:NInstall=$ip" /v:m
```

## Baseline results ("before" state)

* Stage1/Stage2/Stage3: **green** (EXITCODE 0).
* Validate IL fixed-point: **5/5 assemblies "FC: no differences encountered"** (Stage2 IL == Stage3 IL).
* Validate pefverify step: **red at baseline** — old `peverify.exe` (NETFX 4.6.1 Tools) reports
  1125 `Internal error : 0x80131252` on `Stage3\Nemerle.Compiler.dll`; it cannot verify
  `netstandard2.0`-referencing assemblies. Environmental/tooling limitation, NOT a regression.
  Fix is expected via Phase 5.1 (drop InitTools/peverify).
* CompilerTests: **green** — positive **468** test files passed, negative **167** test files passed,
  0 failed ("All tests passed." x2; additionally "All 15 tests passed." from a build-time suite).
* Core host `ncc-core.dll` only runs via `dotnet --roll-forward LatestMajor` (netcoreapp2.1 pin) —
  this is the problem the plan fixes, recorded as-is.

Logs: `C:\RSDN\baseline-logs\` (`stage3-validate.log`, `compilertests.log`, `exitcodes.txt`).

## Parity criteria for later phases

1. Stage1/Stage2 green on the same toolchain.
2. Validate: all `FC:` comparisons report no differences (peverify excluded until Phase 5.1).
3. CompilerTests: >= 468 positive / >= 167 negative, 0 failed.

Re-verified 2026-10-30 after Phase 1 (vswhere entry, global.json 10.0.402, MSBuild 18.11):
FC 5/5 identical, CompilerTests 468+167 / 0 failed, NETSDK1138 absent, Validate's only error is
the known-red pefverify step (identical to Phase-0 baseline: 1125 errors on Nemerle.Compiler.dll).
