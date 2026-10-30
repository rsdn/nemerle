# Known-good build baseline on this machine (pre-net10)

Verified 2026-10-30 at tag `pre-net10-baseline` (commit d9b50f721), branch `retarget-compiler`,
checkout `C:\RSDN\nemerle`. The legacy `Build-core*.cmd` scripts require VS2017 in the registry
and do NOT work on this machine; use VS2022 MSBuild directly (or `dotnet` after Phase 2).

## Commands (verified green)

```powershell
$msb  = 'C:\Program Files\Microsoft Visual Studio\2022\Professional\MSBuild\Current\Bin\MSBuild.exe'
$proj = 'C:\RSDN\nemerle\NemerleAll.nproj'
$ip   = 'C:\RSDN\nemerle\bin\Debug\net-4.0'

# Stage1 (+Stage2+Stage3) and IL fixed-point comparison:
& $msb $proj /t:Stage3,Validate /p:NTargetName=Build /p:Configuration=Debug "/p:NInstall=$ip" /v:m

# Compiler regression tests (must run in the SAME msbuild session after Stage1,
# otherwise $(Nemerle) is empty and project imports fail):
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
