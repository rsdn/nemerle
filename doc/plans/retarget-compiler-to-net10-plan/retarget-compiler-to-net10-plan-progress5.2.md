# Progress 5.2 — Modernize build entry scripts for the .NET 10 toolchain

Status: DONE (gates A–F PASS)
Branch: retarget-compiler
Date: 2026-10-31

## Intent

Sub-atom 5.2 (plan Phase 5.2): "Переписать `Build-core*.cmd` (и добавить
`build.ps1`/`build.sh`): `dotnet restore/build`, без `VsDevCmd`/реестра VS;
кросс-платформенно (исторически был Linux/`xbuild`)."

Orchestrator decision (binding): the classic-MSBuild `NemerleAll.nproj` stays
the build orchestrator (a `dotnet build`-native re-orchestration is explicitly
deferred to plan 5.3). The toolchain, however, is SDK-10-based, so the entry
scripts switch from `vswhere`-discovered `MSBuild.exe` to
`dotnet msbuild NemerleAll.nproj` — verified empirically below. No VsDevCmd,
no VS registry lookups anywhere in the new entry path.

## Pre-edit audit

* Current entry: `Build-core-base.cmd` locates `MSBuild.exe` via vswhere
  (`ProgramFiles(x86)\...\Installer\vswhere.exe`) and runs
  `MSBuild.exe NemerleAll.nproj /t:<Targets> /p:NTargetName=<Build|Rebuild>
  /p:Configuration=<Config> /p:NInstall=bin\<Config>\net-4.0`, with
  `MSBUILDENABLEALLPROPERTYFUNCTIONS=1`.
* Wrappers `Build-core.cmd` / `-2-stage` / `-with-tests` /
  `-with-ide-plug-in` just call the base with fixed args; they keep working
  unchanged as long as the base's CLI contract
  `Build-core-base.cmd <Config> <NTargetName> "<Targets>"` holds (docs +
  AGENTS.md reference it).
* `NemerleAll.nproj` host detection: `UseMSBuild` is set when
  `$(MSBuildBinPath)\msbuild.exe` **or** `msbuild.dll` exists — under
  `dotnet msbuild` the SDK folder carries `MSBuild.dll`, so the MSBuild
  (non-mono) branch is taken on Windows (probe below confirms).
* `Restore` of sub-projects is driven by `NemerleAll.nproj` itself
  (`Stage1`/`_Integration` invoke `Targets="Restore"` on the project lists),
  so the scripts do not need a separate `dotnet restore` step.
* Known Windows-only machinery inside the build (documented in the new
  scripts, unchanged here): Validate uses `ildasm.exe` + `fc` on Windows
  (`$(OS) == 'Windows_NT'` branches; the Unix branches expect mono `monodis`
  / XBuild-era tooling), CompilerTests runs the .NET Framework
  `Nemerle.Compiler.Test.exe` (mono `ExeRunner` fallback is legacy),
  `Junction.exe` / `NuGet.exe` under ExternalDependences are Windows tools.

## Steps

1. This progress file (first).
2. Probe: `dotnet msbuild NemerleAll.nproj` evaluation + cheap
   `/t:InitTools` (tool discovery only, no compilation). Log
   `.tmp\phase52\probe-inittools.log`.
3. New `build.ps1` (repo root, LF): `-Configuration Debug|Release`
   (default Debug), `-Targets` (array or `;`/`,`-joined string, default
   mirrors `Build-core.cmd`: Stage1;_PegAndCSharp;_ComputationExpressions;
   _Async;Install), `-NTargetName Build|Rebuild` / `-Rebuild` switch
   (default Build), `-NInstall` (default `<root>\bin\<Config>\net-4.0`),
   `-Verbosity`, `-DryRun`. Invokes `dotnet msbuild` with the comma-joined
   target list (pwsh passes it as one argument — no cmd quoting trap), sets
   `MSBUILDENABLEALLPROPERTYFUNCTIONS=1`, never touches the registry.
4. New `build.sh` (repo root, LF): same contract (`--configuration`,
   `--targets`, `--rebuild`, `--dry-run`); honest header comment listing what
   does NOT work cross-platform today; refuses to half-run a real build on
   non-Windows unless `NEMERLE_BUILD_ALLOW_UNSUPPORTED_OS=1`.
5. Rewrite `Build-core-base.cmd` minimally: keep the CLI contract + parameter
   validation, delegate to `build.ps1` via PowerShell (`-ExecutionPolicy
   Bypass -File`), propagate exit code. `Build-core*.cmd` wrappers untouched.
6. Update `doc/NET10-BUILD-BASELINE.md` (targeted edit: new canonical
   commands).

## Gates (orchestrator acceptance)

* A: probe — `dotnet msbuild` evaluates `NemerleAll.nproj` (InitTools green)
  or documented fallback with the exact error.
* B: `./build.ps1 -Targets Stage3,Validate -Rebuild` → FC ×5, zero
  `: error`, exit 0 (log `.tmp\phase52\ps1-validate.log`).
* C: `./build.ps1 -Targets Stage1,CompilerTests` → 468+167 / 0 failed,
  exit 0 (log `.tmp\phase52\ps1-tests.log`).
* D: `Build-core-base.cmd Debug Build Stage1` → exit 0 (log
  `.tmp\phase52\cmd-stage1.log`).
* E: `bash build.sh --dry-run` → prints resolved command, exit 0.
* F: `git status` = new `build.ps1`/`build.sh` + modified
  `Build-core-base.cmd` + baseline doc + this progress file. NO commit.

## Probe results

* `dotnet msbuild NemerleAll.nproj -getProperty:UseMSBuild,NTargetName,NVer,NRootBoot`
  (log `.tmp\phase52\probe-getproperty.log`, exit 0) — **evaluation works**
  under SDK-10 `dotnet msbuild` and takes the MSBuild (non-mono) branch:

  ```json
  { "Properties": { "UseMSBuild": "1", "NTargetName": "Rebuild", "NVer": "net-4.0",
                    "NRootBoot": "C:\\RSDN\\nemerle\\boot-net10" } }
  ```

* Cheap target `dotnet msbuild NemerleAll.nproj -t:InitTools`
  (log `.tmp\phase52\probe-inittools.log`) — **runtime is blocked**:

  ```
  MSBuild version 18.9.15 for .NET
  NemerleAll.nproj(157,5): error MSB4803: The task "GetFrameworkSdkPath"
  is not supported on the .NET Core version of MSBuild.
  EXIT=1
  ```

  `GetFrameworkSdkPath` is called unconditionally in `InitTools`, which every
  stage depends on (`Stage1 -> NTasks -> InitTools`; `Validate -> InitTools`),
  so no stage can run under Core MSBuild today. **Fallback per orchestrator
  decision: the scripts locate `MSBuild.exe` via vswhere (no VS2017 registry
  hack, no VsDevCmd).** On this machine vswhere resolves
  `C:\Program Files\Microsoft Visual Studio\18\Professional\MSBuild\Current\Bin\MSBuild.exe`
  (MSBuild 18.11, .NET Framework host — satisfies the global.json >= 18
  requirement). `build.ps1 -UseDotNetMsBuild` / `build.sh --use-dotnet-msbuild`
  are provided so the entry switches over cleanly when Phase 6.3 retires the
  InitTools ildasm discovery.

## Results

### New/changed files

* `build.ps1` (NEW, LF, ASCII): `-Configuration Debug|Release` (default
  Debug), `-Targets` (array or `;`/`,`-joined string; default mirrors
  `Build-core.cmd`: `Stage1;_PegAndCSharp;_ComputationExpressions;_Async;Install`),
  `-NTargetName Build|Rebuild` / `-Rebuild` switch, `-NInstall`
  (default `<root>\bin\<Config>\net-4.0`), `-Verbosity`, `-MsBuildExe`,
  `-UseDotNetMsBuild`, `-DryRun`. Host resolution order: `-MsBuildExe` >
  `NEMERLE_MSBUILD` > vswhere. Sets `MSBUILDENABLEALLPROPERTYFUNCTIONS=1`,
  passes `-nodereuse:false`, propagates the exit code. Registry untouched.
* `build.sh` (NEW, LF): same contract (`--configuration/--targets/
  --target-name/--rebuild/--ninstall/--verbosity/--msbuild/
  --use-dotnet-msbuild/--dry-run`). Header documents honestly what is NOT
  cross-platform today (Validate ildasm+fc; .NET Framework
  Nemerle.Compiler.Test.exe harness; mono/XBuild legacy path; junction.exe /
  NuGet.exe; MSBuild.exe host itself). On a non-Windows kernel a real build
  refuses with exit 2 and an explanatory message instead of half-running
  (override: `NEMERLE_BUILD_ALLOW_UNSUPPORTED_OS=1`); `--dry-run` works
  everywhere.
* `Build-core-base.cmd` (REWRITTEN, CRLF): same CLI contract
  `<Config> <Target> "<Targets>"`, same validation/banner/footer, now
  delegates to `build.ps1` via `powershell -NoProfile -ExecutionPolicy Bypass
  -File` and propagates the exit code. The four `Build-core*.cmd` wrappers are
  untouched and keep working.
* `doc/NET10-BUILD-BASELINE.md`: canonical commands section updated
  (targeted edit).

### Acceptance evidence

* **A (probe)**: evaluation green, runtime blocked by MSB4803
  (`GetFrameworkSdkPath`, NemerleAll.nproj:157) — quotes above; fallback
  (vswhere, no registry) implemented as directed.
* **B**: `./build.ps1 -Targets Stage3,Validate -Rebuild` →
  `PS1-VALIDATE EXIT=0`; log `.tmp\phase52\ps1-validate.log` records the full
  forced-Rebuild chain (boot → Stage1 → Stage2 → Stage3 compiles) and
  `FC: no differences encountered` ×5 (ncc-core.dll, Nemerle.Compiler,
  Nemerle, Nemerle.Macros, Nemerle.MSBuild.Tasks; Stage2 vs Stage3); zero
  `: error` lines; `Build succeeded.` (log lines 96–111). Note: with stdout
  redirected, MSBuild 18.11's console logger omits its summary block; the
  gate relies on the propagated exit code + the checks above.
* **C**: `./build.ps1 -Targets Stage1,CompilerTests` → `PS1-TESTS EXIT=0`;
  log `.tmp\phase52\ps1-tests.log`: positive **468** / negative **167**
  `passed` lines, `All tests passed.` ×2 (also `All 15 tests passed.` from
  the build-time suite), 0 failed / 0 `: error` lines.
* **D**: `cmd /c "Build-core-base.cmd Debug Build Stage1"` → `CMD-STAGE1
  EXIT=0`; log `.tmp\phase52\cmd-stage1.log`: line 1 old banner
  `Starting build Nemerle Target=Build Config=Debug Features=Stage1`, line 5
  the delegated MSBuild command line from build.ps1, `Build succeeded.` +
  old footer; contract intact.
* **E**: `bash build.sh --dry-run` (Git bash; PATH `bash` is the WSL stub on
  this box) prints the resolved command (vswhere host discovery works from
  MINGW via cygpath) and exits 0; `--targets "Stage1;CompilerTests"`
  normalization verified too.
* **F**: `git status --porcelain` → `M Build-core-base.cmd`, `?? build.ps1`,
  `?? build.sh`, `?? doc/.../…-progress5.2.md` (+ the pre-existing untracked
  `AGENTS.md`, untouched) + baseline doc modified. NO commit.

## Status: DONE (gates A–F PASS)
