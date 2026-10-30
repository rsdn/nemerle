# Progress — Tasks 1.1, 1.2, 1.4 (build toolchain migration)

Branch: `retarget-compiler` (nothing committed, per instructions).
Date: 2026-10-30.

## Initial intentions

I will make three changes:

1. **Task 1.1 — `Build-core-base.cmd`**: rewrite the script so it no longer
   depends on VS 2017 registry keys / `VsDevCmd.bat`. Instead it will locate
   MSBuild.exe via `vswhere.exe` (`-latest -products *
   -requires Microsoft.Component.MSBuild -property installationPath`),
   preferring `<installationPath>\MSBuild\Current\Bin\MSBuild.exe` with a
   fallback to `<installationPath>\MSBuild\15.0\Bin\MSBuild.exe`. Keep the
   three parameters (Config, Target, Features), their validation,
   `MSBUILDENABLEALLPROPERTYFUNCTIONS=1`, and the final echo of the install
   dir. If vswhere is missing or returns nothing — print error, `EXIT /B -1`.
   Verify: `cmd /c "C:\RSDN\nemerle\Build-core-base.cmd Debug Build Stage1"`
   exits 0.

2. **Task 1.2 — `global.json`** at repo root with sdk version `10.0.402` and
   `rollForward: latestFeature`. Verify `dotnet --version` prints 10.0.402 in
   the repo root, and the known-good VS2022 MSBuild Stage1 command still
   exits 0.

3. **Task 1.4 — suppress NETSDK1138 EOL warnings** without touching any
   TargetFramework. First verify that `.nproj` files import
   `Nemerle.Sdk.props` (import lines of `Nemerle.nproj`), then add
   `<CheckEolTargetFramework>false</CheckEolTargetFramework>` there and to
   the top PropertyGroup of `NemerleAll.nproj`. Verify: incremental Stage1
   build log has zero `NETSDK1138` occurrences and exit 0; save log to
   `C:\RSDN\phase1-verify.log`.

Files touched: `Build-core-base.cmd`, `global.json` (new),
`tools/msbuild-task/Nemerle.Sdk.props`, `NemerleAll.nproj`, this progress doc.

---

## Task 1.1 — de-VS2017 Build-core-base.cmd — DONE

`Build-core-base.cmd` rewritten:

- Same three parameters (Config, Target, Features) with identical validation
  blocks; `MSBUILDENABLEALLPROPERTYFUNCTIONS=1` kept; final
  `Nemerle installed to ...` echo kept.
- No registry read, no `VsDevCmd.bat`. MSBuild is located via
  `"%ProgramFiles(x86)%\Microsoft Visual Studio\Installer\vswhere.exe"
  -latest -products * -requires Microsoft.Component.MSBuild -property
  installationPath`; uses `<installationPath>\MSBuild\Current\Bin\MSBuild.exe`
  and falls back to `<installationPath>\MSBuild\15.0\Bin\MSBuild.exe` only if
  the former does not exist. That full path is invoked instead of bare
  `msbuild`.
- If vswhere.exe is missing → error message + `EXIT /B -1`; if vswhere
  returns no installation path → error message + `EXIT /B -1`.
- The script now also propagates MSBuild's exit code (`EXIT /B %NemerleBuildExitCode%`).

**Verification** (run BEFORE any other change, i.e. old-state build tree):

```
cmd /c "C:\RSDN\nemerle\Build-core-base.cmd Debug Build Stage1"
→ ... "20 Warning(s)  0 Error(s)  Time Elapsed 00:01:03.83"
→ Nemerle installed to "C:\RSDN\nemerle\bin\Debug\net-4.0"
→ EXITCODE = 0
```

vswhere resolves to `C:\Program Files\Microsoft Visual Studio\18\Professional`
(VS 2026, MSBuild 18.9.15), used via `MSBuild\Current\Bin\MSBuild.exe`.

## Task 1.2 — global.json — DONE, but acceptance (b) fails as pre-flagged

Created `C:\RSDN\nemerle\global.json`:

```json
{
  "sdk": {
    "version": "10.0.402",
    "rollForward": "latestFeature"
  }
}
```

**(a) PASS** — `dotnet --version` in `C:\RSDN\nemerle`: prints `10.0.402`.

**(b) FAIL** — the known-good VS2022 MSBuild 17.14 command
(`C:\Program Files\Microsoft Visual Studio\2022\Professional\MSBuild\Current\Bin\MSBuild.exe NemerleAll.nproj /t:Stage1 ...`)
now exits **1** *specifically because of global.json SDK resolution under
classic MSBuild*, exactly the failure mode the plan told me to stop on.
Exact errors (repeated for each of the 5 compiler .nproj projects):

```
C:\RSDN\nemerle\bin\Debug\net-4.0\boot\Nemerle.Sdk.props(11,3): error :   Version 10.0.402 of the .NET SDK requires at least version 18.0.0 of MSBuild. The current available version of MSBuild is 17.14.60.43110. Change the .NET SDK specified in global.json to an older version that requires the MSBuild version currently available.
C:\RSDN\nemerle\bin\Debug\net-4.0\boot\Nemerle.Sdk.props(11,3): error :   The NuGetSdkResolver did not resolve this SDK because there was no version specified in the project or global.json.
C:\RSDN\nemerle\bin\Debug\net-4.0\boot\Nemerle.Sdk.props(11,11): error MSB4236: The SDK 'Microsoft.NET.Sdk' specified could not be found. [C:\RSDN\nemerle\Nemerle.nproj]
```

Per the task instruction I did **not** change the global.json version to work
around it. Consequence: with `global.json` present, Stage1 must be built by
MSBuild ≥ 18 (VS 2026), which is exactly what the new
`Build-core-base.cmd` selects via vswhere `-latest` and what all subsequent
verifications use. (Note: the VS2022 command *was* green with global.json
absent — the failure is introduced solely by 17.14 refusing SDK 10.)

## Task 1.4 — suppress NETSDK1138 — DONE

Verification of the plan's premise: `Nemerle.nproj` line 20 imports
`$(Nemerle)\Nemerle.Sdk.props`; same import exists in
`Nemerle.Compiler.nproj`, `ncc-core.nproj`, `ncc.nproj`, `Nemerle.Macros.nproj`
and all snippets `.nproj`. The source of that file is
`tools/msbuild-task/Nemerle.Sdk.props`; it is redeployed to the boot folder
(`bin\Debug\net-4.0\boot\`) on every outer build by the `AfterBuild` copy
target of `Nemerle.MSBuild.Tasks.csproj`, so the change flows into Stage1
automatically.

Changes (no TargetFramework touched):

- `tools/msbuild-task/Nemerle.Sdk.props` — added
  `<CheckEolTargetFramework>false</CheckEolTargetFramework>` to the top
  PropertyGroup (before the `Sdk.props` import).
- `NemerleAll.nproj` — added the same property to the top-level
  unconditional PropertyGroup (next to `Configuration`).

Confirmation the deployed copy updated:
`bin\Debug\net-4.0\boot\Nemerle.Sdk.props` line 7 now contains
`<CheckEolTargetFramework>false</CheckEolTargetFramework>`.

**Verification build** (MSBuild 18.9.15 / VS 2026, same command line as the
known-good one, plus file logger):

```
& 'C:\Program Files\Microsoft Visual Studio\18\Professional\MSBuild\Current\Bin\MSBuild.exe' `
  'C:\RSDN\nemerle\NemerleAll.nproj' /t:Stage1 /p:NTargetName=Build /p:Configuration=Debug `
  '/p:NInstall=C:\RSDN\nemerle\bin\Debug\net-4.0' /v:n /fl '/flp:logfile=C:\RSDN\phase1-verify.log;verbosity=normal'
→ Build succeeded. 0 Warning(s) 0 Error(s)   EXITCODE = 0
```

Log saved to `C:\RSDN\phase1-verify.log`.
`NETSDK1138` occurrences in the log: **0**. The log does show
`ncc-core.nproj` (Restore + Build) and the other compiler projects being
built, so the check target was evaluated.

Suppression proven (not merely a no-op artifact) by a controlled experiment on
`ncc-core.nproj` (the project that actually warns: Exe + netcoreapp2.1;
`Nemerle.Compiler.nproj` is Library/netstandard2.0):

```
dotnet msbuild ncc-core.nproj -t:_CheckForEolTargetFrameworks ... -p:CheckEolTargetFramework=true
  → warning NETSDK1138: The target framework 'netcoreapp2.1' is out of support ...
dotnet msbuild ncc-core.nproj -t:_CheckForEolTargetFrameworks ...   (default)
  → no warning (CheckEolTargetFramework=false from Nemerle.Sdk.props)
```

SDK 10 source confirms the mechanism: `Microsoft.NET.EolTargetFrameworks.targets`
emits the warning from target `_CheckForEolTargetFrameworks` with condition
`'$(CheckEolTargetFramework)' == 'true'`.

---

## FINAL CHECKLIST

| Item | Result |
|---|---|
| `Build-core-base.cmd Debug Build Stage1` exit code | **0** (1:03, Stage1 built, "Nemerle installed to C:\RSDN\nemerle\bin\Debug\net-4.0") |
| `dotnet --version` in repo root with global.json | **10.0.402** |
| VS2022 MSBuild 17.14 Stage1 with global.json | **exit 1** — global.json SDK-resolution failure under classic MSBuild (exact errors above); per plan, stopped and recorded, version not forced |
| Stage1 (MSBuild 18.9.15, the toolchain vswhere selects) exit code | **0** |
| `NETSDK1138` count in `C:\RSDN\phase1-verify.log` | **0** |
| `git status --short` | see below |

```
$ git status --short
 M Build-core-base.cmd
 M NemerleAll.nproj
 M doc/plans/retarget-compiler-to-net10-checklist.md
 M tools/msbuild-task/Nemerle.Sdk.props
?? AGENTS.md
?? doc/plans/retarget-compiler-to-net10-plan/
?? global.json
```

All entries are intended except two that were **not touched by this session**
and are pre-existing working-tree state: `doc/plans/retarget-compiler-to-net10-checklist.md`
(modified before this session) and `AGENTS.md` (untracked before this session).
My changes: `Build-core-base.cmd`, `NemerleAll.nproj`,
`tools/msbuild-task/Nemerle.Sdk.props`, `global.json` (new), and this progress
doc under `doc/plans/retarget-compiler-to-net10-plan/`. Nothing committed.

## Follow-up note for the plan owner

Because `global.json` pins SDK 10.0.402 (needs MSBuild ≥ 18), the VS2022
MSBuild 17.14 command from `doc/NET10-BUILD-BASELINE.md` is no longer usable
while `global.json` is present; `Build-core-base.cmd` (vswhere `-latest` →
VS 2026 / MSBuild 18) is the working entry point and could replace the
17.14 command as the documented known-good baseline.
