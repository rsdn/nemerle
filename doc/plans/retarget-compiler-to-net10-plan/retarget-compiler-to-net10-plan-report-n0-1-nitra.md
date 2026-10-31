# N0.1 — nitra `kl/retarget-nitra` inventory vs nemerle `retarget-compiler` TOP drop

Date: 2026-10-31. Mode: read-only research + throwaway-worktree build probe. No changes to any
repo; probe tree: `C:\RSDN\nitra-n01` (worktree detached at `kl/retarget-nitra` = `b50357c33`,
2022-09-29; the user's `C:\RSDN\nitra` untouched, still on `test`).
Logs: `C:\RSDN\nemerle\.tmp\nitra-tdl\phaseN01\` (`probe1-buildboot.log`, `probe2-stage1.log`,
console twins, `msbuild-path.txt`).
Build host: classic **MSBuild 18.11 Full**
(`C:\Program Files\Microsoft Visual Studio\18\Professional\MSBuild\Current\Bin\MSBuild.exe`,
vswhere-resolved), `MSBUILDENABLEALLPROPERTYFUNCTIONS=1`.

**Headline: the probe got much further than expected — the whole core Stage1 chain (all three
TDL-target outputs) already builds green with the TOP drop's `ncc-core.exe` out of the box.
Only 2 projects fail, both outside the TDL dependency set** (details in Q3).

---

## Q1. Build topology

### Entry points

| Script | What it does |
|---|---|
| `Build.cmd` | `Build-base.cmd Debug Build` — full chain |
| `Build-base.cmd` | 1) `msbuild Common\BootTasks.proj /t:BuildBoot /p:NemerleBinPathRoot=..\Nemerle\bin\Debug`; 2) `msbuild Nitra-Stagt1.sln /t:Restore`; 3) `msbuild Nitra-Stagt1.sln /t:Build`. Hard-pins VS2017 via registry `HKLM\SOFTWARE\WOW6432Node\...\VS7\15.0` (dead on this machine — invoke MSBuild directly instead) |
| `Build-boot.cmd` / `Build-wo-boot.cmd` | same, skipping the other step |
| `RebuildBoot.cmd` | `BootTasks.proj /t:BuildBoot /p:BuildTarget=Rebuild` (also VS-dev-cmd dependent) |
| `ShiftBoot.cmd` | `BootTasks.proj /t:ShiftBoot` — the boot **reseed** operation |
| `test.cmd` | empty stub (just sets up VS env) — tests are `Tests\run*.cmd` |

### Chain (Boot/Stage sequence)

```
Boot1/ (checked-in FX seed: nemerle 1.2.0.556 ncc.exe + Nitra.Compiler/Grammar.dll,
        Nitra.Runtime.Boot142.dll, DotNetLang.Boot142.dll, version.xml=143)
  │  BuildBoot (Common/BootTasks.proj:76-98): compiles checked-in snapshots Boot2\{Nitra.Compiler,
  │  Nitra.Grammar,Nitra.Runtime,DotNetLang} + System.Collections.Immutable.Light.csproj
  │  with Stage=Boot, NameSuffix=Boot143 → OutputPath=Boot2\Net-4.0
  │  Toolchain: Boot2 nproj force Nemerle=Boot1\Net-4.0 when Stage=='Boot'
  │  (Boot2\{Nitra.Grammar,Nitra.Runtime,DotNetLang}\*.nproj:21-22).
  │  EXCEPTION: Boot2\Nitra.Compiler.nproj has NO Boot override (lines 17-20) → it compiles
  │  with Nemerle=$(NemerleBinPathRoot)\Net-4.0, i.e. WITH WHATEVER NEMERLE DROP IS PASSED IN.
  │  (It has zero MacroReference items, so the new host never loads FX macros here.)
  ▼
Boot2/Net-4.0/ (fresh Boot-generation Nitra.Compiler.dll/Nitra.Grammar.dll + *.Boot143.dll)
  │  Stage1 (Nitra-Stagt1.sln): real sources Nitra\*, DotNet.BackEnd.{CCI,Dnlib}, Grammars,
  │  Nitra.LanguageCompiler, Ide\* — Nemerle=$(NemerleBinPathRoot)\Net-4.0 (nproj:18-20,
  │  default $(ProgramFiles)\Nemerle), macro-ref Boot2\Net-4.0\Nitra.Compiler.dll
  │  (Boot2\..\..\ condition Stage!='Boot'; e.g. Nitra\Nitra.Runtime.nproj:20,77,87).
  │  Output bin\Debug\Stage1, Grammars\bin\Debug, Nitra.LanguageCompiler\bin\Debug.
  ▼
Stage2 (optional): *.Stage2.nproj shims (Stage=Stage2) recompile the same sources with the
  Stage1 output (bin\Debug\Stage2); not run by Build-base.cmd. UpdateStage1Metadata.cmd copies
  Stage2 .nitrametadata2 back.

ShiftBoot (reseed, BootTasks.proj:100-125): Boot2\Net-4.0 → Boot1\Net-4.0 (xcopy), then copies
  nemerle drop files into Boot1 from the list @NccFile (lines 28-74: Nemerle[.Compiler/.Macros/
  .Peg].dll, CSharpParser.dll, ncc.parser.csharp.dll, ncc.exe, ncc32.exe, ncc64.exe,
  Nemerle.MSBuild.Tasks.dll, Nemerle.MSBuild.targets, dnlib.dll, System*.dll, Microsoft*.dll,
  netstandard.dll), increments Boot1\version.xml, snapshots current sources into Boot2\*,
  rebuilds Boot2.
```

### Which nemerle generation is the checked-in seed

**Pre-dnlib, .NET Framework generation (2019-era).** Evidence:
- `Boot1\Net-4.0`: `ncc.exe`/`ncc32.exe`/`ncc64.exe` are 12.8 KB **MSIL/mscoree** exes (not
  net10 apphosts); no `ncc-core.*`, no `*.runtimeconfig.json`, no `dnlib.dll`.
- `Nemerle.Compiler.dll` contains no `dnlib` reference; `Nemerle.dll` identity =
  **v1.2.0.556** (TOP drop is v10.0.33.0).
- `Boot1\Net-4.0\Nemerle.MSBuild.targets` = old FX shape (Ncc UsingTask from the tasks dll,
  `CompilerPath=$(Nemerle)`, no Sdk split); `Nemerle.Sdk.props` absent from Boot1 (boot nproj
  do not import it, so fine).
- `git log`: Boot1 last touched 2019-03-10 (“New boot”), Boot2 snapshots 2020-03-05
  (“Sync with Nemerle compiler”); no reseed commit on the branch. `Boot1\version.xml` = 143.

### Consumer API surface (how nitra talks to nemerle)

- Every nproj: `Nemerle = $(NemerleBinPathRoot)\Net-4.0` (default `$(ProgramFiles)\Nemerle`),
  `NemerleLibs` likewise; imports `$(Nemerle)\Nemerle.MSBuild.targets` (end) and — **only the
  `Nitra\*` stage projects** — `$(Nemerle)\Nemerle.Sdk.props` (e.g. `Nitra\Nitra.Compiler\Nitra.Compiler.nproj:55`).
- `Common/NitraBuild.targets:18`: `UsingTask Nemerle.Tools.MSBuildTask.GetGitTagRevision
  AssemblyFile=$(Nemerle)\Nemerle.MSBuild.Tasks.dll`.
- `Common/NitraBuild.targets:36-41`: nemerle version derived from `$(Nemerle)\Nemerle.dll`
  assembly identity (→ `NemerleProductVersion` for nuspecs).
- `Nitra.MSBuild.Sdk/Sdk.props:25-29`: consumer layout `NitraAndNemerleSourcesPath` →
  `Nemerle=$(...)\nemerle\bin\Debug\net-4.0\Stage1`, `NitraPath=$(...)\nitra\bin\Debug\Stage1`,
  `NitraGrammarsPath=$(...)\nitra\Grammars\bin\Debug` (matches the real `C:\RSDN\` layout; no
  `Nitra.user` present on this machine, override mechanism = `Nitra.user` above the project).
  Note the **SDK default points at nemerle `Stage1`, not the Install dir** — TDL/consumers should
  pin `Nemerle=`/`NemerleBinPathRoot=` explicitly or accept Stage1 (exists in the drop).
- Grammars/LanguageCompiler/TDL-style consumers get references+macros from
  `Nitra.MSBuild.Sdk/Sdk.targets` (`Nemerle.dll`, `Nitra.Runtime.dll`,
  `System.Collections.Immutable.Light`, MacroReference `Nitra.Compiler.dll`, then imports
  `$(Nemerle)\Nemerle.MSBuild.targets`).

---

## Q2. Consumer-API mismatches vs TOP drop

| # | Item | Evidence | Verdict | Minimal adaptation |
|---|---|---|---|---|
| a | `UsingTask Nemerle.Tools.MSBuildTask.GetGitTagRevision` from `$(Nemerle)\Nemerle.MSBuild.Tasks.dll` | `Common/NitraBuild.targets:18`; TOP `tools/msbuild-task/GetGitTagRevision.cs:8-10` (same namespace); `bin\Debug\net-4.0\Nemerle.MSBuild.Tasks.dll` present, TaskName `Nemerle.Tools.MSBuildTask.Ncc` + GetGitTagRevision | **compatible** (proven: probes ran GetGitTagRevision + Ncc fine) | none |
| b | `$(Nemerle)\Nemerle.MSBuild.targets` + `Nemerle.Sdk.props` imports | nitra nproj:55/456; both files exist in TOP drop; Ncc is a ToolTask and **auto-remaps `ncc.exe`→`ncc-core.exe`** when present (`tools/msbuild-task/MSBuildTask.cs:108-130`); runtimeconfig/deps co-located in drop | **compatible**, with a hard constraint: TOP targets route `MacroReference` resolution only on the **Full MSBuild host** and hard-`Error` on the Core host (`Nemerle.MSBuild.targets:207-269`) | always build nitra with classic Full MSBuild (≥18, as nemerle's `build.ps1` resolves), never `dotnet msbuild` |
| c | Macro execution path | Ncc ToolTask spawns `ncc-core.exe` (net10) out-of-proc; nitra macro assemblies (`Boot2\Nitra.Compiler.dll` for Stage1; `bin\Debug\Stage1\Nitra.Compiler.dll` for downstream/Grammars) are loaded **in-proc in ncc-core on net10**. Stage nproj TFMs: `netstandard20` (`Nitra\Nitra.Compiler.nproj:29`); boot projects `TargetFrameworkVersion=v4.5` (`Boot2\Nitra.Compiler.nproj:29`) | **compatible** — proven: ncc-core executed the netstandard2.0 Nitra.Compiler macro to compile `Nitra.Grammar`/`CSharp.Grammar`/samples (probe 2); FX-metadata boot outputs load fine as references | none observed; keep macro assemblies ns2.0+ |
| d1 | Hardcoded `ncc.exe`, `ncc32.exe`, `ncc64.exe` in ShiftBoot `@NccFile` copy list | `Common/BootTasks.proj:47-55`; TOP drop has none of them | **blocker for ShiftBoot only** (Copy → MSB3030) | update the list to the TOP seed set (`ncc-core.exe/.dll/.deps.json/.runtimeconfig.json`, `Nemerle*.dll`, `dnlib.dll`, `System.CodeDom.dll`, `netstandard.dll`, CSharpParser/ncc.parser.csharp) |
| d2 | `$(ProgramFiles)\Nemerle` defaults, `Clear.cmd` `%ProgramFiles%\Nemerle\Net-4.0\*.nupkg`, `Tests/sub-env.cmd:4` | nproj:18-20 everywhere; `Clear.cmd:57`; `Tests/sub-env.cmd:4` | **adaptable** | pass `-p:NemerleBinPathRoot=C:\RSDN\nemerle\bin\Debug` (drop = `bin\Debug\net-4.0`, matches the old `<root>\Net-4.0` layout — verified working) or `-p:Nemerle=<drop>`; set env in test scripts |
| d3 | VS2017 registry pin in `Build*.cmd`/`test.cmd` | `Build-base.cmd:24-31` etc. | **adaptable** | documented direct-msbuild invocation (probe commands below); no repo change needed |
| d4 | `Nemerle.MSBuild.targets` old shape inside `Boot1` (no Sdk split, launches Boot1 `ncc.exe`) | `Boot1\Net-4.0\Nemerle.MSBuild.targets` (15.7 KB) vs TOP 20.3 KB | **compatible for boot** (self-contained FX chain ran green under MSBuild 18) | none while Boot1 stays; after reseed it disappears |
| e | Version derivation from `$(Nemerle)\Nemerle.dll` (ns2.0 in TOP) | `NitraBuild.targets:36-41`; TOP identity `Nemerle, Version=10.0.33.0` | **compatible** | none (cosmetic: `NemerleProductVersion` regex targets `x.y.0.z` → `10.0.0.33`? format only affects nuspec metadata) |
| f | `Nemerle.Macro.Resource` (`.resx`) usage in one project under Core host | probe 2: `Nitra.ClientServer.Server\Properties\AssemblyInfo.n:5` → nemerle Core-port refuses (System.Windows.Forms `ResXResourceReader` is FX-only) | **project-level blocker (only this one nitra project)** | port the project off the resx macro (embed via `EmbeddedResource`+`System.Resources.ResourceManager` directly) or exclude it from the N1 gate (it is IDE/ClientServer-only, not in TDL's dependency set) |
| g | Old-style VSIX/IDE projects (`NitraCommonVSIX` etc., packages.config, VS2017 SDK) | probe 2 error `EnsureNuGetPackageBuildImports`; `Ide\NitraCommonVSIX\Vsix2017\*` | **out of scope** (retarget rule: no VSIX/IDE legacy on retarget branch goals) | build a core subset sln (or `Nitra.Compiler.sln` + grammars) instead of the whole `Nitra-Stagt1.sln`; don't fix VSIX |
| h | `Nemerle.Peg.dll`/`CSharpParser`/`ncc.parser.csharp.dll` expectations | BootTasks `@NccFile`; Grammars bin shows `Nemerle.Peg.dll` copied from drop | **compatible** — all present in TOP drop | none |

---

## Q3. Build probe (results)

Commands (from `C:\RSDN\nitra-n01`, MSBuild 18.11 Full, `MSBUILDENABLEALLPROPERTYFUNCTIONS=1`):

```
# attempt 1 (first invocation failed instantly with MSB4212 until the env var was set — the
# official Build-base.cmd sets it itself, so this is wiring parity, not a repo defect):
msbuild Common\BootTasks.proj /t:BuildBoot /p:NemerleBinPathRoot=C:\RSDN\nemerle\bin\Debug   # GREEN (exit 0)

# attempt 2:
msbuild Nitra-Stagt1.sln /restore /t:Build /p:NemerleBinPathRoot=C:\RSDN\nemerle\bin\Debug   # exit 1, 3 errors, 6m03s
```

**Attempt 1 — BuildBoot: GREEN.** Boot deps compiled by `Boot1\Net-4.0\ncc.exe` (FX);
`Boot2\Nitra.Compiler` compiled by **`C:\RSDN\nemerle\bin\Debug\Net-4.0\ncc-core.exe`** (log lines
139/424/525 vs 775-776) — i.e. the TOP drop already serves as the boot-Nitra.Compiler compiler
thanks to the missing Boot override in that one nproj. Outputs: `Boot2\Net-4.0\` populated
(Nitra.Compiler/Grammar.dll, Runtime/DotNetLang `Boot143`, new-gen `Nemerle*.dll` copied as refs).

**Attempt 2 — Stage1: 3 distinct failure signatures, everything TDL-relevant GREEN.**
All ncc invocations used `Using Nemerle compiler: C:\RSDN\nemerle\bin\Debug\Net-4.0\ncc-core.exe`.
Produced (verbatim success lines): `bin\Debug\Stage1\{Nitra.Runtime,Nitra.Compiler,Nitra.Grammar,
DotNetLang,Nitra.Utils,DotNet.BackEnd.CCI,DotNet.BackEnd.Dnlib,Nitra.ClientServer.{Client,Macros,Messages}}.dll`,
`Grammars\bin\Debug\CSharp.Grammar.dll`, `Nitra.LanguageCompiler\bin\Debug\Nitra.LanguageCompiler.exe`,
plus the Ide C# projects (WpfHint2, NitraCommonIde, DotNetLang.VisualStudio.Plugin, …).

First failures verbatim (full text in `probe2-stage1.log:2255,2258,3004`):

1. `Nitra.ClientServer.Server\Properties\AssemblyInfo.n(5,12,5,64): error : The Resource(".../Properties/Resources.resx") macro is not supported when the compiler runs on .NET (Core): ResXResourceReader (System.Windows.Forms) is Framework-only. [Nitra.ClientServer.Server.nproj]`
   → class Q2-f (FX-only resx macro under Core host). Cascades to `Nitra.Visualizer.csproj.metaproj` FAILED.
2. `Nitra.ClientServer.Server\HtmlPrettyPrintWriter.cs(201,14,201,23): error : unbound name `PrettyPrintDoughnut' in `Resources' [Nitra.ClientServer.Server.nproj]` — pure cascade of #1 (the macro never generated the member).
3. `Ide\NitraCommonVSIX\Vsix2017\NitraCommonVSIX.csproj(394,5): error : This project references NuGet package(s) that are missing ... packages\Microsoft.VisualStudio.SDK.EmbedInteropTypes.15.0.17\build\...targets`
   → class Q2-g (legacy packages.config VSIX; `msbuild /restore` doesn't restore packages.config). Cascades to `Nitra.VisualStudio.Plugin.csproj.metaproj` FAILED.

Notes: `Json.Grammar` and `Nitra.TestsLauncher` are in the sln but disabled in its build
configuration (not built, not skipped-with-error). No compile-level incompatibility (types,
IL, macro ABI, dnlib emit) surfaced anywhere in the core chain.

**Disposition: nitra Stage1 on the TOP drop needs zero wiring changes for the TDL dependency
set.** The Q2 table's “blocker” item (d1) affects only the *reseed* (ShiftBoot) path, not the
build path.

---

## Q4. Reseed needs (plan only)

Current seed works as-is for building, so reseed is **not required to consume nitra outputs**;
it is required eventually to (i) retire the 2019 FX seed, (ii) make boot macros run on net10,
(iii) reproduce nemerle's fixed-point hygiene. Analog of nemerle 4.6:

1. Prereq: Stage1 green on TOP drop (probe-proven, needs the ClientServer.Server resx atom or
   sln subset first, so a full `Build.cmd`-equivalent is green).
2. Fix `BootTasks.proj` `@NccFile` for the TOP seed set (d1): `ncc-core.exe/.dll/.deps.json/.runtimeconfig.json`,
   `Nemerle.dll`, `Nemerle.Compiler.dll`, `Nemerle.Macros.dll`, `Nemerle.Peg.dll`,
   `Nemerle.MSBuild.targets`, `Nemerle.MSBuild.Tasks.dll`, `dnlib.dll`, `System.CodeDom.dll`,
   `netstandard.dll` (+ CSharpParser/ncc.parser.csharp if kept in seed); drop
   `ncc.exe/ncc32/ncc64`.
3. Decide boot shape: keep `Boot2\*` projects on `v4.5` compiled by the FX Boot1 chain (status
   quo), or flip boot nprojs to `netstandard20`/new targets so ShiftBoot produces a net10-hosted
   boot. Recommended: flip in the same atom — after the shift, `Boot1\Net-4.0` must contain
   ncc-core + ns2.0 nitra macros, and `Boot1\Nemerle.MSBuild.targets` becomes the new-shape file
   (ShiftBoot copies it from the drop).
4. Run `ShiftBoot` (from the throwaway tree first): Boot2→Boot1, NccFile copy, version.xml 143→144,
   source snapshot, `BuildBoot` rebuild with the new seed → must be green twice in a row
   (ShiftBoot-internal BuildBoot + a following standalone BuildBoot).
5. Then full Stage1 (+ Stage2 fixed-point sanity: Stage1↔Stage2 IL-identical for the 4 boot
   assemblies) and the Q5 test suite green.
6. Commit the reseeding shift (Boot1/ + Boot2/ + version.xml, binary) as **one dedicated commit**
   separate from the wiring commits, mirroring nemerle's seed policy; record the drop's nemerle
   version (10.0.33.0) in the commit message as the provenance note.

---

## Q5. Test surface

- Runner: `Tests\run.cmd` → `run-positive.cmd` + `run-negative.cmd` (`nopause` for CI); each calls
  `sub-env.cmd` + `sub-run.cmd`.
- `sub-env.cmd`: `NemerleBinPathRoot` (default `%ProgramFiles%\Nemerle` — override),
  `Nemerle=%NemerleBinPathRoot%\Net-4.0`, `RuntimeDllPath=CompilerDllPath=%~dp0..\bin\%Configuration%\Stage1`.
- `sub-run.cmd`: copies `Grammars\Bin\%Configuration%\*` to the out dir, then runs
  `"%Nemerle%\Nemerle.Compiler.Test.exe" %Tests%\*.n *.cs *.nitra -output:... -ref:Nitra.Runtime.dll
  -macro:Nitra.Compiler.dll` — nemerle's testsuite harness, present in the TOP drop (net10 apphost;
  invocation as bare `exe` works). TeamCity args auto-added only under `%TEAMCITY_VERSION%`.
- Corpus: `Tests\!Positive` 35 files, `!Negative` 20, `!Disabled` 6 (NUnit is not used;
  `Nitra.ClientServer.Macros.Tests` nproj + `Tests\{CSharp,Cpp}.Grammar.Test` nproj exist but are
  not part of the run-scripts chain).
- No CI config on the branch (no azure-pipelines/teamcity yml; only TeamCity flag plumbing).
- **Baseline acquisition for N1.5** (after a Stage1 exists in the real `C:\RSDN\nitra` layout):
  ```
  set NemerleBinPathRoot=C:\RSDN\nemerle\bin\Debug
  cd C:\RSDN\nitra && Tests\run-positive.cmd nopause && Tests\run-negative.cmd nopause
  ```
  Expected norm to record: pass/fail counts of the 35 positive + 20 negative files. Quick subset
  check: run `sub-run.cmd` with `Tests=!Positive` filtered to a handful of files (harness accepts
  explicit file args). Nothing runnable before Stage1 exists (runner hard-depends on
  `bin\Debug\Stage1\Nitra.Compiler.dll` + `Grammars\Bin\Debug\*`).

---

## Proposed N1 atomization

Gates use the probe commands (Full MSBuild 18, `NemerleBinPathRoot=C:\RSDN\nemerle\bin\Debug`,
`MSBUILDENABLEALLPROPERTYFUNCTIONS=1`) run against the real `C:\RSDN\nitra` after ff to kl;
commits only in `retarget-nitra`.

1. **N1.1 ff + baseline snapshot** — ff local `retarget-nitra` to kl (23 commits, 0 local); no
   build. Gate: `git rev-parse HEAD == b50357c33`-descendant, tree clean except untracked.
2. **N1.2 build entry adaptation** — replace VS2017-registry probing in `Build-base.cmd`
   (vswhere-style msbuild, or documented direct-msbuild invocation like nemerle's `build.ps1`);
   default `NemerleBinPathRoot`-style wiring to the source-layout drop
   (`..\Nemerle\bin\Debug`), keep `-p:` override. Gate: `Build-base.cmd Debug Build
   skip-build-boot` reaches the same 3-error state as probe 2 from a clean `bin\` (or, if N1.3
   already merged, green).
3. **N1.3 ClientServer.Server resx atom** — remove the Core-host `Nemerle.Macro.Resource`
   dependency (hand-written ResourceManager or drop the resx; the only consumer is
   `HtmlPrettyPrintWriter`), or (fallback) exclude the project + its dependents from the gate sln.
   Gate: that project compiles under ncc-core (or is out of the sln).
4. **N1.4 core-gate solution subset** — trim the standard entry to the non-IDE subset (drop
   `NitraCommonVSIX`/VS plugin projects from `Nitra-Stagt1.sln` build configs or provide
   `Nitra.Core.sln`). Gate: **zero-error** full chain `BootTasks /t:BuildBoot` +
   `sln /restore /t:Build` (~7 min expected).
5. **N1.5 Stage1 consumption + tests baseline** — run Q5 suite against the fresh Stage1; record
   exact pass counts (target: parity with pre-retarget behavior; document any red as baseline).
   Gate: numbers in checklist. Optional Stage2 sanity pass.
6. **N1.6 Grammars + LanguageCompiler smoke** — CSharp.Grammar + `Nitra.LanguageCompiler.exe`
   present in the three target dirs; generate/compile the Calculator sample from the
   LanguageCompiler templates. Gate: sample grammar compiles with Stage1 toolchain.
7. **N1.7 ShiftBoot list update (wiring only)** — apply Q4 step 2 (+ boot nproj TFM flip decision
   from Q4 step 3) in a **throwaway worktree**, run ShiftBoot + double-BuildBoot green, Stage1 +
   tests green. No commit yet.
8. **N1.8 reseed commit** — promote the shift (Boot1/Boot2 binaries + version.xml + NccFile
   change) as a dedicated binary commit (provenance: nemerle 10.0.33.0, drop path, gate logs).
   Gate: from a clean checkout + new seed: BuildBoot + Stage1 + tests green.
9. **N1.9 handoff to N2** — verify `C:\RSDN\nitra\{bin\Debug\Stage1, Grammars\bin\Debug,
   Nitra.LanguageCompiler\bin\Debug}` satisfy `Nitra.MSBuild.Sdk/Sdk.props` expectations for TDL
   (they do; SDK default `Nemerle=` points at `nemerle\bin\Debug\net-4.0\Stage1` — present).

Deviations from plan assumptions recorded here: no wiring mismatch turned out to block the build
path (Q2 a-e all compatible; the historical netcoreapp2.1-era assumptions of retarget-nitra still
hold against the net10 TOP); the only content-level Core-host gap found is Q2-f (resx macro), and
it is confined to one non-TDL project.
