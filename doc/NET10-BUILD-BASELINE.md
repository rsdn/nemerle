# Known-good build baseline on this machine (pre-net10)

Verified 2026-10-30 at tag `pre-net10-baseline` (commit d9b50f721), branch `retarget-compiler`,
checkout `C:\RSDN\nemerle`. The legacy `Build-core*.cmd` scripts originally required VS2017 in the
registry; since Phase 1 they locate MSBuild via vswhere. With `global.json` (sdk 10.0.402)
**MSBuild >= 18 is required** — VS2022's msbuild 17.14 can no longer build the repo (it fails with
"requires at least version 18.0.0 of MSBuild"); use `Build-core-base.cmd` or VS2026 msbuild.

## Commands (canonical since Phase 5.2: `build.ps1` / `build.sh`)

```powershell
# From the repo root. PowerShell passes "Stage3,Validate" to MSBuild as ONE
# argument — the cmd.exe comma-splitting trap is gone on this entry.
./build.ps1 -Targets Stage3,Validate -Rebuild
./build.ps1 -Targets Stage1,CompilerTests
./build.ps1 -Targets _NugetPack   # NuGet packages (revived in 6.5, dotnet-pack based) -> bin\<Config>\net-4.0\Nuget
```

`build.ps1` (and its bash twin `build.sh`) never touch the registry or
VsDevCmd; they locate the host via `-MsBuildExe`/`NEMERLE_MSBUILD`/vswhere.
`dotnet msbuild` (SDK 10) **evaluates** `NemerleAll.nproj` correctly but fails
to run any stage: `NemerleAll.nproj(157,5): error MSB4803: The task
"GetFrameworkSdkPath" is not supported on the .NET Core version of MSBuild`
(InitTools is a dependency of every stage; its remaining ildasm discovery
still backs Validate's `fc` IL-diff and retires only together with a Validate
replacement). Until then the default host is the vswhere-discovered .NET
Framework `MSBuild.exe` (>= 18 per global.json); `build.ps1
-UseDotNetMsBuild` is the forward-compatible switch. A separate `dotnet
restore` is not needed: `NemerleAll.nproj` runs the `Restore` target on its
project lists itself.

Legacy .cmd entry still works — `Build-core-base.cmd` is now a thin wrapper
delegating to `build.ps1` with its old contract
`Build-core-base.cmd <Config> <Target> "<Targets>"` (still quote target lists
there, cmd.exe splits unquoted commas):

```powershell
cmd /c "`"C:\RSDN\nemerle\Build-core-base.cmd`" Debug Rebuild `"Stage3,Validate`""
cmd /c "`"C:\RSDN\nemerle\Build-core-base.cmd`" Debug Build `"Stage1,CompilerTests`""
```

Equivalent direct invocation (bypasses the wrappers): resolve MSBuild via
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

## Windows-only surfaces and the Core graph (Phase 5.5)

The VS IDE layer under `snippets/VS2010` (WpfHint WPF popup, VSSDK-era
`Nemerle.VsExtension`, `Nemerle.Compiler.Utils`) and the WiX MSI packaging
(`misc/packages/wix`) are Windows-only by construction (WPF / VSSDK / WiX) and
unmigrated to the net10 toolchain, and the Core build+test graph (`Stage1..4`,
`Validate`, `CompilerTests`, `Install`) has zero dependency edges into any of
them. (`_NugetPack` belonged to this set until Phase 6.5 revived it on modern
tooling — a generated SDK-style shim driven through `dotnet pack`; it no
longer needs the removed `ExternalDependences\NuGet.exe`.) The IDE-reachable
entry points (`Integration*`/`Installer*`/`DevBuild*`) stop with explicit
`<Error>` guards rather than cryptic mid-target failures: `_Integration`/
`_Installer` are opt-in via `-p:BuildingVsIntegration=true` (Windows hosts
only), `DevBuild*` composites stay behind that same 5.5 guard (their
`_NugetPack` step itself works since 6.5), and the removed
`IdeEngineTests` is a documented error stub. The single deliberate Core-graph
edge into WPF is `CompilerTests -> _Wpf`: `Nemerle.WPF.dll` is a test-suite
fixture for `testsuite/{positive,negative}/notifypropertychanged.n` (`MACRO:`
comment), so the target and its test-dir copies skip (not fail) off Windows.
Shipping IDE integration lives on `master`. Audit table:
`doc/plans/retarget-compiler-to-net10-plan/retarget-compiler-to-net10-plan-progress5.5.md`.

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

Re-verified 2026-10-31 after Phase 5.2 (`build.ps1`/`build.sh` entry, vswhere-discovered
MSBuild 18.11; pefverify retired in 5.1): `build.ps1 -Targets Stage3,Validate -Rebuild` →
FC 5/5 identical, zero `: error`, exit 0; `build.ps1 -Targets Stage1,CompilerTests` →
468+167 / 0 failed, exit 0. Logs: `C:\RSDN\nemerle\.tmp\phase52\`.

Re-verified 2026-10-31 after Phases 6.2–6.5 / 7.1 (seed = `boot-net10`, toolchain net10;
progress docs `.../retarget-compiler-to-net10-plan-progress{6.4,6.5,7.1}.md`):
`build.ps1 -Targets Stage3,Validate -Rebuild` → FC 5/5, 0 errors, exit 0;
`build.ps1 -Targets Stage1,CompilerTests` → positives **461 passed + 7 not-run**
(the 468 files, 7 skipped via `// NO-CORE`), negatives 167, 0 failed, exit 0 —
the current Core norm; `build.ps1 -Targets _NugetPack` → `Nemerle` +
`Nemerle.Compiler` .nupkg in `bin\Debug\net-4.0\Nuget` (ncc-core tool closure
under `tools\net10.0`, netstandard2.0 libs under `lib\netstandard2.0`).
After the 7.1 cleanup the VS-IDE-plugin .cmd build entry and the FX
gacutil/ngen/registry-registration scripts are gone; `Build-core*.cmd` remain
thin shims over `build.ps1`. Logs: `.tmp\phase65\`, `.tmp\phase71\`.
