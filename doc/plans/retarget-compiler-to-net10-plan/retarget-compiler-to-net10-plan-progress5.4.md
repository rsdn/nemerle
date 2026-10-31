# Progress 5.4 — ExternalDependences/: retire NuGet.exe, NUnit console; keep MSBuild.Community.Tasks (live) + WiX isolated

Status: DONE (gates A–E PASS)
Branch: retarget-compiler
Date: 2026-10-31

## Intent

Sub-atom 5.4 (plan Phase 5.4): "ExternalDependences/: осовременить/убрать
NuGet.exe, MSBuild.Community.Tasks.dll, NUnit-консоль (Framework).
WiX-инсталлятор оставить изолированно (Windows/Framework, вне ядра
компилятора)."

Everything below is driven by a `git grep` audit of tracked consumers; the
Core build+test graph is `Stage1..3 (+boot)`, `Validate`, `CompilerTests`,
`Install` as driven by `build.ps1` (the 5.2 canonical entry); the
installer/IDE layer (`_Integration`, `_Installer`, `_NugetPack`, `DevBuild*`,
`Installer*`) is NOT in that graph.

## Audit method

1. `git ls-files ExternalDependences` — 41 tracked paths + 4 submodules
   (vssdk2008, vssdk2010, wix36, wix311; all initialized on this box per
   `git submodule status`, clean).
2. Live-consumer greps (`git grep -n`, whole repo + the scoped file set
   `*.nproj *.csproj *.cmd *.ps1 *.targets *.props *.bat snippets tools
   doc/NET10-BUILD-BASELINE.md`) for: `NuGet.exe`, `MSBuild.Community.Tasks`,
   `junction.exe`/`Junction`, `nunit`/`NUnit`, `wiX`/`wix`, `SharpZipLib`,
   `SetJunction`, `sed.exe`, `NAnt`, `nuspec`.
3. Key structural findings:
   * `MSBuild.Community.Tasks.dll`: `UsingTask FileUpdate` at
     `NemerleAll.nproj:2`; **6 live call sites inside `Validate`**
     (lines 343–344 unconditional + 347–348 Windows + 351–352 Unix — the
     IL-diff comment-normalizing shim; the 343–344 pair makes the Windows
     pair redundant, but the target is green and gate-B-exercised, so it is
     NOT touched here). Additionally `UsingTask MSBuild.Community.Tasks.Zip`
     at `misc/packages/wix/nemerle.wixproj:23` (installer path). There is no
     in-box MSBuild task for regex file replacement, so per the audit rule
     the DLL stays while FileUpdate call sites exist. `snippets/VS2010`
     csprojs reference `TemplateFile` via `$(MSBuildCommunityTasksPath)`
     (IDE layer, same DLL). The old `Nemerle.Sdk.targets` consumer was
     already removed in 2.4a (grep: no hits under `tools/msbuild-task`).
   * `NuGet.exe` (v **2.7.3 / 2.7.41115.310**, 2015 Framework binary): the
     ONLY consumer chain is `<Nuget>` property (`NemerleAll.nproj:176`) →
     `_NugetPack` `Exec`s (555–556, legacy `nuget pack *.nuspec`) →
     `DevBuildQuick/QuickWithTests/2Stage/2StageWithTests/Full` +
     `InstallerFast/InstallerFull`. ZERO consumers in the Core graph
     (confirmed in 5.3: "installer/IDE-path targets only"). All of those
     composites additionally run `_Integration` (snippets/VS2010: VSSDK-2010
     csprojs + `TemplateFile` from the same Community Tasks DLL) and the
     `Installer*` ones `_Installer` (WiX submodule) — no gate 0–5.x ever ran
     any of them green; `NemerleAllTest.cmd` is fully dead (calls targets
     `NccFast/NccFull/VSIntegration/Shell/RegProgFiles/RunTests` that no
     longer exist). NuGet packaging revival is checklist item **6.5**.
     Restore needs no NuGet.exe: it runs through MSBuild's own `Restore`
     targets over PackageReferences + the 5.3 repo `nuget.config`.
   * NUnit: the CompilerTests harness is Nemerle-native
     (`Nemerle.Compiler.Test.exe`); no target, script or in-graph project
     uses the NUnit 2.6 console distribution. Whole-repo consumers of the
     console set are only IDE-era artifacts OUTSIDE the graph: `StartProgram`
     F5 settings (`LinqTests.nproj`, `Calculator*.nproj`,
     `Nemerle.Json.Tests-VS_2010.nproj` — the latter already dangling,
     pointing at `g:\P\nemerle\...`) and HintPath references in the dead
     `Linq/Tests/LinqTests-VS_2010.nproj` (nunit.core/interfaces/util/
     console-runner/pnunit.framework). `nunit.framework.dll`+`.xml` keep
     HintPath consumers in 9 checked-in snippet/Linq-test projects → KEEP
     (a test-time library, not the console). `nunit.mocks.dll`: zero
     consumers → delete with the console set.
   * `junction.exe`: never Exec'd by ANY MSBuild target — `$(Junction)` is
     only assigned (`InitTools`, 175/182) and printed (`Message`, 190). The
     only Exec consumers are the FX-era root scripts `Reg-bins*.cmd`
     (gacutil/ngen v2.0.50727, VS9 registry keys — same family as the
     scripts 5.1 already scoped for a later retirement). Per the audit rule
     ("note it for 7.1, do not delete") → KEEP, 7.1 candidate.
   * Zero-consumer orphans: `ICSharpCode.SharpZipLib.dll` (no reference
     anywhere), `sed.exe` (no reference; the earlier apparent hits were the
     substring "…pressed.exe" in `snippets/raytracer/Makefile`),
     `SetJunction.exe` (only reference is a COMMENTED-OUT `<File>` line in
     `misc/packages/wix/Sources/NemerleRuntime.wxs:45`).
   * `NAnt.Core.dll`/`NAnt.DotNetTasks.dll`: consumer
     `tools/Nemerle.NAnt.Tasks.nproj`, built only by the orphan `Tools`
     target (nothing DependsOn it) and listed in `Tools.sln`. NAnt-path
     retirement is explicitly 3.5f→7.1 scope → KEEP files, 7.1 candidate.
   * `dnlib.dll`: live (`Nemerle.Compiler.nproj`, report 2.5) → KEEP.
   * WiX (`wix36`/`wix311` submodules) + vssdk submodules: consumed
     exclusively by the isolated installer/IDE layer
     (`misc/packages/wix/nemerle.wixproj` `WixToolPath`,
     `snippets/VS2010/*.csproj` `VsSDKInstall`) — plan says keep isolated →
     KEEP untouched; isolation documented with one line in
     `ExternalDependences/README.md` (audit step e).

## Disposition table (A)

file | bytes | live consumers | action
---|---|---|---
NuGet.exe | 1 611 264 | `_NugetPack` Exec only (installer/IDE layer, outside Core graph; revival = checklist 6.5) | **DELETE** + remove dangling `<Nuget>` property + block `_NugetPack` with an explanatory `<Error>` (no silent half-run)
MSBuild.Community.Tasks.dll | 869 888 | `Validate` FileUpdate ×6 (Core graph, gate B) + `nemerle.wixproj` Zip + snippets/VS2010 TemplateFile | **KEEP** (no native MSBuild regex-replace task; retire with Validate at 6.3/7.1)
NUnit console set (33 files): nunit.exe/.config, nunit-x86.exe/.config, nunit-console.exe/.config, nunit-console-x86.exe/.config, nunit-agent.exe/.config, nunit-agent-x86.exe/.config, nunit-console-runner.dll, nunit.core.dll, nunit.core.interfaces.dll, nunit.util.dll, nunit.mocks.dll, pnunit-agent.exe/.config, pnunit-launcher.exe/.config, pnunit.framework.dll, pnunit.tests.dll, runpnunit.bat, NUnitFitTests.html, NUnitTests.VisualState.xml, NUnitTests.config, NUnitTests.nunit, TestResult.xml, agent.conf, agent.log.conf, launcher.log.conf, test.conf | 561 440 | ZERO in Core graph; only dead IDE `StartProgram`/HintPath refs in out-of-graph VS_2010-era projects | **DELETE** (plan names the console explicitly)
nunit.framework.dll | 143 360 | HintPath in 9 checked-in projects (Linq/Tests, snippets) | **KEEP** (library, not console)
nunit.framework.xml | 572 340 | doc file of the above | **KEEP**
junction.exe | 158 520 | 6× `Reg-bins*.cmd` (FX-era scripts); no target Execs `$(Junction)` | **KEEP**, note for 7.1
NAnt.Core.dll | 376 832 | `tools/Nemerle.NAnt.Tasks.nproj` (orphan `Tools` target, `Tools.sln`) | **KEEP**, 7.1 candidate
NAnt.DotNetTasks.dll | 131 072 | same | **KEEP**, 7.1 candidate
ICSharpCode.SharpZipLib.dll | 143 360 | none anywhere | **DELETE**
sed.exe | 49 152 | none anywhere | **DELETE**
SetJunction.exe | 8 704 | none (commented-out wxs line only) | **DELETE**
dnlib.dll | 1 143 296 | `Nemerle.Compiler.nproj` (live) | **KEEP** (untouched)
README.md | 169 | — | **EDIT**: one line documenting the isolated Windows/Framework submodules
vssdk2008/vssdk2010/wix36/wix311 (submodules) | n/a | installer/IDE layer only | **KEEP ISOLATED** (plan)
Total deletions: **37 files, 2 373 920 bytes (~2.3 MB)**.

Dangling references deliberately NOT edited (IDE-era junk in dead
out-of-graph projects; 7.1 owns snippets//Linq-Tests cleanup): `StartProgram`
→ nunit-console.exe in `LinqTests.nproj`, `Calculator{,-VS_2010}.nproj`,
`Nemerle.Json.Tests-VS_2010.nproj` (already dangling before this change);
HintPaths in `Linq/Tests/LinqTests-VS_2010.nproj`. `nunit.framework.dll`,
the library those projects actually compile against, stays, so no project
loses a build input.

## Steps

1. This progress file (first). ✔
2. `git rm` the 37 files above.
3. `NemerleAll.nproj`: delete the `<Nuget>` property line (176); replace the
   `_NugetPack` body (identity/props/dir-prep + the two `nuget pack` Execs)
   with a documented `<Error>` pointing at checklist 6.5. BOM/CRLF/XML must
   stay intact; Validate untouched.
4. `build.sh` header line 28: drop the now-stale `NuGet.exe` mention
   (dangling doc reference); `junction.exe` stays listed.
5. `ExternalDependences/README.md`: add the isolation one-liner.
6. Gates (one build at a time):
   B `./build.ps1 -Targets Stage3,Validate -Rebuild` → FC ×5, zero `: error`,
   exit 0 (`.tmp\phase54\validate.log`);
   C `./build.ps1 -Targets Stage1,CompilerTests` → 468+167/0, exit 0
   (`.tmp\phase54\tests.log`);
   D `./build.ps1 -Targets Stage1,Install` → exit 0
   (`.tmp\phase54\install.log`) — Install itself consumes no
   ExternalDependences file, but `NemerleAll.nproj` (which defines it) was
   edited. `Install` cannot run standalone by design (`Error` when
   `NCurBin` is unset; it is set by the preceding Stage target in the same
   invocation), hence Stage1,Install. Install is also part of the standard
   chain (build.ps1 default targets `Stage1;_PegAndCSharp;
   _ComputationExpressions;_Async;Install`, mirroring Build-core.cmd).
7. `git status`: deletions + dangling cleanups + this doc only. NO commit.

## Results

### Static verification (after edits, before builds)

* `NemerleAll.nproj`: BOM `EF BB BF` intact, CRLF-only (CR=LF=565), XML
  parses (36 targets); `git diff` shows only the two intended hunks
  (`<Nuget>` property removal in `InitTools`, `_NugetPack` body → comment +
  `<Error>`).
* Dangling-reference sweep: `git grep "NuGet.exe|Nuget>|\$(Nuget)"` → only
  the 5.4 `<Error>`/comment in `NemerleAll.nproj` and historical plan/progress
  docs. NUnit-console sweep outside `snippets/`/`Linq/` → only
  `nunit.framework.xml` doc text, commented-out lines in the legacy root
  `configure`, and `tools/nemerle-unit/lib/*` (a separate bundled NUnit copy,
  not under ExternalDependences; out of 5.4 scope).

### Gate B — `./build.ps1 -Targets Stage3,Validate -Rebuild` — PASS

Log `.tmp\phase54\validate.log` (112 lines; boot → Stage1..3 up-to-date,
forced-Rebuild of the last stage + full Validate):
* `FC: no differences encountered` × **5**; `: error` lines: **0**;
  exit code **0** (`P54-VALIDATE EXIT=0`).
* `: warning ` lines: **60** — identical to the `.tmp\phase53\validate.log`
  baseline (60); zero MSB4062/4061/0001/4132 (the `MSBuild.Community.Tasks.
  FileUpdate` UsingTask loads and runs — Validate is green with the DLL kept).

### Gate C — `./build.ps1 -Targets Stage1,CompilerTests` — PASS

Log `.tmp\phase54\tests.log`:
* positive suite: **468** `passed` lines; negative suite: **167**;
  `All tests passed.` × **2**; fail-like lines: **0**; `: error`: **0**;
  exit code **0** (`P54-TESTS EXIT=0`).
* `: warning ` lines: **5** — identical to the phase-5.3 tests.log baseline
  (5); no missing-assembly warnings (grep `MSB4062|cannot be found|could not
  load`: 0).

### Gate D — `./build.ps1 -Targets Stage1,Install` — PASS

`Install` cannot run standalone by design (it `Error`s when `NCurBin` is
unset; `NCurBin` is set by the preceding Stage target in the same
invocation), so it was run Stage1-prefixed. Log `.tmp\phase54\install.log`:
`Install binaries from: 'C:\RSDN\nemerle\bin\Debug\net-4.0\Stage1'`,
`Build succeeded.`, `: warning `/`: error ` lines: 0/0,
exit **0** (`P54-INSTALL EXIT=0`). Install is also part of the standard
chain (`build.ps1` default targets `Stage1;_PegAndCSharp;
_ComputationExpressions;_Async;Install`, mirroring `Build-core.cmd`). No
Install-consumed file was deleted (junction.exe kept; Install never touched
NuGet.exe — it only copies the `$(NBin)\Nuget\*.*` *output* dir, an empty
glob when `_NugetPack` has not run).

### Gate E — `git status --porcelain`

37 `D` (ExternalDependences deletions) + `M NemerleAll.nproj` (dangling
`<Nuget>` property + `_NugetPack` Execs → `<Error>` pointer to 6.5) +
`M build.sh` (stale header mention) + `M ExternalDependences/README.md`
(isolation one-liner) + this progress doc + the pre-existing untracked
`AGENTS.md` (untouched). NO commit.

## Status: DONE (gates A–E PASS)

