# Progress 5.5 — `_Wpf`/WPF and `snippets/VS2010/*` (`_Integration`): gated by Windows / opt-in, Core graph provably IDE-free

Status: DONE (gates A–F PASS)
Branch: retarget-compiler
Date: 2026-10-31

## Intent

Sub-atom 5.5 (plan Phase 5.5): "`_Wpf`/WPF и `snippets/VS2010/*` (`_Integration`)
— гейтить по Windows/TFM или исключить из Core-сборки компилятора."

Core graph = `Stage1..4`, `Validate`, `CompilerTests`, `Install` in
`NemerleAll.nproj` as driven by `build.ps1` (canonical entry since 5.2; reseed
cycle = `Stage3,Validate -Rebuild` + `Stage1,CompilerTests`). IDE code is NOT
deleted on this branch (`master` keeps the shipping VS2017 integration) — the
deliverable is that the Core build graph never reaches IDE/WPF bits on any OS,
and every IDE-reachable target invocable via `build.ps1` stops with an explicit
`<Error>` instead of a mid-target cryptic failure.

## Audit method

* `git grep -n "_Wpf" -- "*.nproj" "*.csproj" "*.sln" NemerleAll.nproj` →
  hits only in `NemerleAll.nproj` (359, 532, 537).
* `git grep -n -i "snippets" -- NemerleAll.nproj` → all `snippets/*` item lists
  (lines 70–133, 150–152) + `NSTestSuiteDir` (501); of these only `NWpf` (133)
  and `NIntegrationProject` (150–152) are WPF/VS2010 entities.
* `git ls-files snippets/VS2010` + WPF greps in the three integration csprojs
  (evidence per row below).
* `git grep -n "\.sln" -- NemerleAll.nproj` → the ONLY solution the orchestrator
  ever builds is `misc/packages/wix/nemerle.sln` (line 563, `_Installer`); no
  target imports anything under `snippets/VS2010/*.sln` (e.g.
  `snippets/VS2010/Nemerle.VsExtension/WpfHint.sln` is IDE-side junk).
* `git grep -rn "MACRO" -- snippets/Nemerle.Test` → the CompilerTests harness
  parses `// MACRO: <dll>` (`NccTestDescription.n:61`) and passes each hit as
  `-m:<dll>` (`NccTest.n:184`); `testsuite/positive/notifypropertychanged.n:1`
  and `testsuite/negative/notifypropertychanged.n:1-2` declare
  `MACRO: Nemerle.WPF.dll` → the `CompilerTests → _Wpf` edge is a hard
  test-suite dependency (see decision row 1 below).
* `git grep -n "IdeEngineTests" -- .` → referenced only as a `DependsOnTargets`
  name (`NemerleAll.nproj` 305, 553, 555, 556) and defined NOWHERE in the repo
  (the `VsIntegration/` engine tests were removed in the retarget) → any build
  asking for it died with MSB4057 at scheduling time.

## Audit table (A)

Entity (file:line) | Reachable from which `NemerleAll.nproj` targets (traced) | OS/TFM sensitivity | Guard applied

1. `_Wpf` target (`NemerleAll.nproj:532-534`) → builds `snippets/Nemerle.WPF/Nemerle.WPF/Nemerle.WPF.nproj` | `CompilerTests` (DependsOn, :359); `_PowerPack` (:537) ← `PowerPack` (:299), `InstallerFast/Full` (:308/:311), `DevBuildFull` (:556) | **Windows-only**: subproject is `TargetFrameworkVersion v4.6.1` (`Nemerle.WPF.nproj:16`) referencing `WindowsBase` (`:59`) — the WPF macro assembly (DependencyProperty/NotifyPropertyChanged macros) | Target `Condition` extended to `'$(UseMSBuild)' != '' And '$(OS)' == 'Windows_NT'` → off-Windows it SKIPS (correct semantics: its only consumer inside the Core graph is the test fixture below; the two `Copy`s of `Nemerle.WPF.dll` in `CompilerTests` (:372-373) got the same `Windows_NT` condition so they cannot fail on a missing source on other OSes). **The `CompilerTests → _Wpf` edge is KEPT deliberately** — documented exception, see Decision below.
2. `NWpf` item (`:132-134`) | consumed only by `_Wpf` (:533) | same as row 1 | inert when `_Wpf` skips; untouched.
3. `_Integration` target (`:422-428`) | `IntegrationFast` (:302), `IntegrationFull` (:305), `DevBuildQuick`/`QuickWithTests`/`2Stage`/`2StageWithTests`/`Full` (:552-556) | **Windows-only**: VSSDK-2010 + WPF project system | `DependsOnTargets` now starts with new guard target `_GuardVsIntegration` (before its `Linq;Unsafe;TestFramework` deps, so even a direct `-t _Integration` stops before any build attempt): `<Error>` when `$(OS) != Windows_NT`, `<Error>` when `-p:BuildingVsIntegration` (new documented opt-in property, next to `NInstallIntegration` at :34) is not `true`.
4. `NIntegrationProject` items (`:149-153`): `snippets/VS2010/WpfHint/WpfHint.csproj` (`WindowsBase`/`PresentationCore`/`PresentationFramework` refs :55-57, XAML `Page Include="HintWindow.xaml"` :87), `snippets/VS2010/Nemerle.Compiler.Utils/Nemerle.Compiler.Utils.nproj`, `snippets/VS2010/Nemerle.VsExtension/Nemerle.VsExtension.csproj` (`PresentationCore`/`PresentationFramework` :346-347, `WindowsBase` :502, VSSDK props :6-11, `MSBuild.Community.Tasks.TemplateFile` UsingTask :23) | consumed only by `_Integration` (:426-427) | **Windows-only** (WPF + VSSDK) | covered by the single row-3 gate (one documented gate, no per-project edits — step 3 of the atom).
5. `IdeEngineTests` (referenced `:305`, `:553`, `:555`, `:556`; **defined nowhere**) | `IntegrationFull`, `DevBuildQuickWithTests`, `DevBuild2StageWithTests`, `DevBuildFull` | n/a — missing target failed the whole invocation with cryptic MSB4057 at scheduling time | replaced by a documented stub target that `<Error>`s with a pointer to `master`; Core graph never references it.
6. `_Installer` target (`:559-564`) → `misc/packages/wix/nemerle.sln` (wixproj, WiX submodules per 5.4; packages the IDE/WPF payloads incl. `Vs2010_Extension.wxs`) | `InstallerFast` (:308), `InstallerFull` (:311) | **Windows-only** (WiX toolset; IDE payload) | `DependsOnTargets="_GuardVsIntegration"` added (same two `<Error>`s).
7. `_NugetPack` (`:541-549`) | `DevBuildQuick/QuickWithTests/2Stage/2StageWithTests/Full` (:552-556), `InstallerFast/Full` (:308/:311) | blocked since 5.4 (`<Error>` pointer to checklist 6.5) | untouched (in `DevBuild*`/`Installer*` composites it still fires first — gate D quotes it).
8. Composite entry points `IntegrationFast` (:302), `IntegrationFull` (:305), `InstallerFast` (:308), `InstallerFull` (:311), `DevBuildQuick` (:552), `DevBuildQuickWithTests` (:553), `DevBuild2Stage` (:554), `DevBuild2StageWithTests` (:555), `DevBuildFull` (:556) — all invocable via `build.ps1 -Targets`; `DefaultTargets="DevBuild2Stage"` (:1) too | Windows-only transitively (rows 3/5/6/7) | no own `Condition` — deliberate deviation from the literal "add Condition to every target" wording: a target-level `Condition="'$(OS)' == 'Windows_NT'"` on a composite makes MSBuild *silently skip* it off-Windows and report **BUILD SUCCEEDED / exit 0** having built nothing (false green). The hub guards (rows 3/5/6) + the pre-existing 5.4 `_NugetPack` block stop every one of these with an explicit `<Error>` on every OS instead. `_Wpf` (row 1) is the only place where skip semantics are right, because its absence merely downgrades a suite that cannot run off-Windows at all (CompilerTests harness is .NET Framework; `build.sh` refuses non-Windows at exit 2).
9. `snippets/VS2010/**/*.sln` (e.g. `Nemerle.VsExtension/WpfHint.sln`) | referenced by NO target — `NemerleAll.nproj` MSBuilds exactly one `.sln` (`misc/packages/wix/nemerle.sln`, :563) | — | untouched.
10. Core graph: `Stage1..4` (:245-296), `Validate` (:314-356), `CompilerTests` (:359-389), `Install` (:392-419) | `build.ps1` defaults + reseed cycle | — | **zero edges into `snippets/VS2010`** (item lists 149-153 reachable only via `_Integration`); the single `snippets/WPF` edge `CompilerTests → _Wpf` is the documented row-1 test fixture, now Windows-gated. Proven by gates B + C (Core chain byte-identical behaviour) and by the fact that no Core target `DependsOn`s or `MSBuild`s anything in rows 3-6/8.
11. `DevBuildQuickNccOnly` (:551) | pure Core (Stage1 + Install) | — | untouched (no IDE edge).
12. `NInstallIntegration`/`DeployExtension` (:34, :424), `snippets/Nemerle.WPF/Test` + `StartProgram` F5 junk, `Nemerle.WPF*.sln` | never built by any target (`Test.nproj` is referenced by no NemerleAll item list) | — | untouched.

## Decision: keep `CompilerTests → _Wpf`

The plan text allows "gate by Windows/TFM **or** exclude from the Core build".
Exclusion is impossible without losing gate coverage: `testsuite/positive/notifypropertychanged.n`
and `testsuite/negative/notifypropertychanged.n` `#1-2` declare
`MACRO: Nemerle.WPF.dll`; the harness (`NccTestDescription.n:61` →
`NccTest.n:184`) passes it as `-m:`, so without `Nemerle.WPF.dll` (built by
`_Wpf`, copied at :372-373) 2 of the 468+167 gate-C tests fail. `Nemerle.WPF`
is also not IDE code — it is a PowerPack macro library that merely happens to
live under `snippets/`. Hence: edge KEPT, entity Windows-gated (skip + matching
copy conditions), documented here as the single deliberate Core-graph edge into
`snippets/…WPF`.

## Steps

1. This progress doc (first). ✔
2. `NemerleAll.nproj` (BOM/CRLF preserved):
   a. new documented `BuildingVsIntegration` opt-in property (default `false`);
   b. new `_GuardVsIntegration` target (OS `<Error>` + opt-in `<Error>`);
   c. `_Integration` + `_Installer` `DependsOnTargets` start with the guard;
   d. `IdeEngineTests` error stub;
   e. `_Wpf` condition + comment; `CompilerTests` WPF copies get `Windows_NT`.
3. `doc/NET10-BUILD-BASELINE.md`: "Windows-only, not in the Core graph" paragraph.
4. Gates (one build at a time):
   B `./build.ps1 -Targets Stage3,Validate -Rebuild` → FC ×5, zero `: error`, exit 0 (`.tmp\phase55\validate.log`);
   C `./build.ps1 -Targets Stage1,CompilerTests` → 468+167/0, exit 0 (`.tmp\phase55\tests.log`);
   D `./build.ps1 -Targets _Integration` → new 5.5 opt-in guard `<Error>` (`.tmp\phase55\guard-integration.log`);
     `./build.ps1 -Targets DevBuildQuick` → composite stops at the pre-existing 5.4 `_NugetPack` `<Error>` (`.tmp\phase55\guard-devbuild.log`);
   E `bash build.sh --targets IntegrationFast --dry-run` (entry resolves, refuses to execute) + direct-MSBuild `OS=Unix` probe showing the 5.5 OS-`<Error>` text a non-Windows kernel would hit even with `NEMERLE_BUILD_ALLOW_UNSUPPORTED_OS=1`;
   F `git status`: only `NemerleAll.nproj` + baseline doc + this doc. NO commit.

## Results

### Static verification (after edits, before builds)

* `NemerleAll.nproj`: BOM `EF BB BF` intact, CRLF-only (CR=LF=604), XML parses;
  `git diff` hunks: property, `_Integration` header+guard target, `_Wpf`
  comment+condition, two `CompilerTests` copy conditions, `_Installer`
  DependsOn+comment, new `_GuardVsIntegration` + `IdeEngineTests` stub. Core
  targets otherwise untouched.
* Caught by the first Gate-B attempt (then fixed, ASCII-only): an XML comment
  cannot contain `--` → `error MSB4025` at project load; re-verified XML OK
  before rerunning the gate.

### Gate B — `./build.ps1 -Targets Stage3,Validate -Rebuild` — PASS

Log `.tmp\phase55\validate.log`: `FC: no differences encountered` × **5**;
`: error` lines **0**; `: warning ` lines **60** (identical to the 5.4
baseline); MSB4xxx **0**; exit code **0** (`P55-VALIDATE EXIT=0`). The guards
did not disturb the Core chain.

### Gate C — `./build.ps1 -Targets Stage1,CompilerTests` — PASS

Log `.tmp\phase55\tests.log`: positive suite **468** per-file `passed` lines
(positive block ends `All tests passed. Time taken: 00:03:47…`), negative suite
**167** (`All tests passed. Time taken: 00:00:59…`), `All tests passed.` × 2,
fail-like lines **0**, `: error` **0**, `: warning ` **5** (5.4 baseline
parity), exit **0** (`P55-TESTS EXIT=0`). The kept `CompilerTests → _Wpf`
fixture edge still delivers Nemerle.WPF.dll for notifypropertychanged.n.

### Gate D — IDE-reachable targets stop with explicit guards — PASS

`./build.ps1 -Targets _Integration` (`.tmp\phase55\guard-integration.log`,
exit 1, stops BEFORE any Linq/Unsafe/TestFramework/subproject work):

> `NemerleAll.nproj(435,5): error : _Integration/_Installer (VS IDE layer under snippets/VS2010 + MSI packaging) are opt-in since net10 plan 5.5: Windows-only and unmigrated to the net10 toolchain; the shipping IDE integration lives on the `master` branch. On a Windows host with the VS2017-era VSSDK prerequisites, re-run with -p:BuildingVsIntegration=true to attempt them anyway. (net10 plan 5.5)`

`./build.ps1 -Targets DevBuildQuick` (`.tmp\phase55\guard-devbuild.log`,
exit 1) — already Error-blocked since 5.4, nothing added there per the atom
rule:

> `NemerleAll.nproj(576,5): error : _NugetPack is blocked: NuGet.exe was removed from ExternalDependences (net10 plan 5.4). Revive nuspec packaging via checklist item 6.5 (dotnet pack) before invoking this target.`

(`DevBuildQuick → Stage1; _NugetPack; …` reaches the 5.4 block first; the 5.5
guard additionally covers `IntegrationFast`, which has no `_NugetPack` dep, and
direct `_Installer`/`_Integration` invocations. `IntegrationFull`/
`DevBuild*WithTests`/`DevBuildFull` can no longer die of the cryptic
MSB4057-on-`IdeEngineTests` scheduling error either — the stub errors
actionably.)

### Gate E — non-Windows paths — PASS

* `bash build.sh --targets IntegrationFast --dry-run` → resolves the command,
  `DryRun: not executing.`, exit **0** (executes nothing).
* Non-Windows kernel simulated (Git-Bash, `uname -s` stubbed to `Linux`):
  `bash build.sh --targets IntegrationFast --msbuild <MSBuild.exe>` prints
  `build.sh: a real build requires Windows today (MSBuild.exe host, ildasm/fc
  in Validate, .NET Framework CompilerTests harness -- see the header comment
  of this file). Nothing was executed. …` and exits **2** — the IDE target is
  never even handed to MSBuild. (Without `--msbuild` the same refusal appears
  via the host-discovery branch, also exit 2.)
* Forcing MSBuild anyway with `OS` pinned non-Windows (models
  `NEMERLE_BUILD_ALLOW_UNSUPPORTED_OS=1`):
  `MSBuild.exe NemerleAll.nproj -t:_Integration /p:OS=Unix …`
  (`.tmp\phase55\guard-os-unix.log`, exit 1):
  > `NemerleAll.nproj(434,5): error : The VS IDE/installer targets are Windows-only: snippets/VS2010 (VSSDK + WPF: WpfHint, Nemerle.VsExtension) and misc/packages/wix (WiX) cannot build on OS='Unix'. The Core compiler graph (Stage*/Validate/CompilerTests/Install) never reaches them; nothing there requires this target. (net10 plan 5.5)`

### Gate F — `git status --porcelain`

`M NemerleAll.nproj`, `M doc/NET10-BUILD-BASELINE.md`, new progress doc (plus
pre-existing untracked `AGENTS.md` and ignored `.tmp/`). NO commit.

## Status: DONE (gates A–F PASS)
