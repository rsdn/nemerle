# N0.2 — TDL `kl/retarget-tdl` inventory for building on .NET 10 (nemerle TOP drop + new nitra)

Date: 2026-10-31. Mode: read-only research + throwaway-worktree build/test probes. No changes to
any repo working tree; probe tree: `C:\RSDN\TDL-n02` (worktree detached at `kl/retarget-tdl` =
`984c52b`; main `C:\RSDN\TDL` untouched, still on clean `master`). The worktree is left in place
with exactly one probe delta: `src/TdlCompiler/TdlCompiler.csproj` `net5.0 → net10.0` (the N2.1
change, kept for inspection).
Logs: `C:\RSDN\nemerle\.tmp\nitra-tdl\phaseN02\` (`probe1-restore/build`, `probe2-goldentests`,
`probe3-transformator-tests`, `probe4-net10-build`, `probe5-goldentests-net10core`,
`probe6-golden-with-samples`, `crossout\*`).
Build host: classic **MSBuild 18.11 Full** via VS18 `VsDevCmd.bat -arch=x64`,
`MSBUILDENABLEALLPROPERTYFUNCTIONS=1` (same host as N0.1).

**Headline: the probe built the whole `Tdl.sln` green out of the box against the nemerle TOP drop
and the N0.1 nitra Stage1 outputs, and got the full suite to 164/164 green (144 golden + 20
Transformator) after a one-line TFM bump `net5.0 → net10.0`.** The bump is not cosmetic: the
net5.0-targeted `tdlc.dll` crashes with `AccessViolationException` on the installed .NET 5.0.17
runtime (72/72 NetCore-leg failures), while the identical sources built `net10.0` run clean on
.NET 10 and pass all 144 tests. Two pre-existing branch quirks are recorded as deviations
(Q3.4, Q3.5); neither is toolchain-caused.

---

## Q1. Build topology on `kl/retarget-tdl`

### Entry points

| Script | What it does | State on this machine |
|---|---|---|
| `src\Build.cmd` | vswhere `-version 16.0` → `VsDevCmd.bat` → `msbuild Tdl.sln /t:Restore` + `msbuild Tdl.sln`, both with `/p:NemerleBinPathRoot=..\..\nemerle\bin\Debug` | **works mechanically** — plan doc claimed the vswhere pin is dead; wrong: `-version 16.0` is a *minimum* range (matches ≥16.0), resolves here to VS2022/VS18 and enters the `IF defined` branch. Its only real gap: it does not pass nitra paths, so it relies on the default layout `C:\RSDN\nitra\bin\Debug\Stage1` — which needs N1's rebuilt artifacts (today it holds the old net461/CCI drop that cannot serve this branch) |
| `src\Directory.Build.props` | default layout `NitraAndNemerleSourcesPath = <repo>\..\..\` → `NitraPath=nitra\bin\Debug\Stage1`, `NitraGrammarsPath=nitra\Grammars\bin\Debug`, `NitraLanguageCompilerPath`, `NitraCommonIdePath`, `NugetExePath`; all overridable by `src\Nitra.user` (absent on this machine, untracked by design) | matches the real `C:\RSDN\{nemerle,nitra,TDL}` layout; probes used `/p:` overrides to point at `C:\RSDN\nitra-n01` |

### Projects and wiring

All artifacts land in repo-root `bin\<Config>\` (+ `net461\`/`net5.0\` subfolders for the two tdlc
legs, via SDK `AppendTargetFrameworkToOutputPath`), `obj\<Config>\`; TestDll in `bin\Debug\Tests\`.

| Project | TFM / style | Nemerle/Nitra wiring | Verdict |
|---|---|---|---|
| `src\Tdl` (`Tdl.nproj`) | `netstandard20`, old-style nproj | `Nemerle = $(NemerleBinPathRoot)\Net-4.0`; imports `$(Nemerle)\Nemerle.Sdk.props` + `Nemerle.Sdk.targets` (end); `NoStdLib`; refs Nitra.Runtime/DotNetLang/dnlib/DotNet.BackEnd.Dnlib/System.Collections.Immutable.Light from `$(NitraPath)`, CSharp.Grammar from `$(NitraGrammarsPath)`; **MacroReference Nitra.Compiler** | in scope (language core) |
| `src\Tdl2JsonLib` (`.nproj`) | `netstandard20`, nproj | same Sdk.props/targets; refs as Tdl + PackageReference Newtonsoft.Json 10.0.3, JsonDiffPatch.Net 2.1.0; ProjectRefs Tdl, JsonDal, Tdl.VersionInfo | in scope (compiler core) |
| `src\JsonDal` | `netstandard20` SDK csproj | none (plain C#) | in scope |
| `src\Tdl.VersionInfo` | `netstandard13` Sdk.props/targets hybrid | **T4** via `$(MSBuildExtensionsPath)\Microsoft\VisualStudio\v$(VisualStudioVersion)\TextTemplating\Microsoft.TextTemplating.targets` (exists under VS18 `v18.0`; needs VS dev environment), GitInfo 2.0.26 | in scope |
| `src\TdlCompiler` (`tdlc`) | **`net5.0`** SDK csproj | C# CLI; refs Nitra.Runtime/DotNetLang/System.Collections.Immutable.Light from `$(NitraPath)`; ProjectRefs Tdl2JsonLib (PrivateAssets=all); `PackAsTool`, `GeneratePackageOnBuild`, `RollForward=Major`; LangVersion 7.3 | in scope — **the bump target** |
| `src\Tdl2Json` (`tdlc.exe`) | `net461` SDK csproj | compiles the *same* sources (`..\TdlCompiler\**\*.cs`); Costura.Fody 3.2.0/Fody 3.3.2 (PackageReference, weave targets), `EnableDefaultItems=false` | in scope (FX leg of golden tests) |
| `Tests\TestDll` | `net461` classic csproj, packages.config (MSTest.TestFramework 1.4.0) | none; referenced at test-run time as a `-r` assembly | in scope |
| `src\Tdl.Tests` | `net461` classic csproj, packages.config (NUnit 3.11.0, NUnit3TestAdapter 3.13.0, System.ValueTuple 4.5.0) | launches both tdlc legs as processes | in scope (golden harness) |
| `src\Tdl.Transformator` | `net461` SDK csproj | refs Tdl/Tdl2JsonLib + Nitra/Nemerle HintPaths unless `SolutionName=='Tdl.Transformator'` (then NuGet `Tdl` 1.15.5.252) | in scope |
| `src\Tdl.Transformator.Tests` | `net461` SDK csproj, MSTest 1.4.0 + Microsoft.NET.Test.Sdk 16.2.0 (PackageReference) | same conditional | in scope |
| `src\Tdl.VisualStudio.Plugin` | net461 VSIX, packages.config | **not in `Tdl.sln`** on this branch (AGENTS: removed); only consumer of `$(NitraLanguageCompilerPath)` | out of scope |
| `TestRunner` | — | deleted on this branch (commit `58ae456`) | out of scope |
| `src\Tdl.Transformator.sln` | secondary sln | NuGet-`Tdl`-based, not the acceptance path | out of scope |
| `src\{Json,JSonDal,VsCodeExtension,TdlLangVsPackage}` | — | no project files / gitignored / npm | out of scope |

`src\packages` (packages.config feed for Tdl.Tests/TestDll/JsonToTdl) is **gitignored and absent in
a fresh worktree**; `msbuild /t:Restore` does not restore packages.config. Probe fix: copied
`NUnit.3.11.0, NUnit3TestAdapter.3.13.0, System.ValueTuple.4.5.0, MSTest.TestFramework.1.4.0,
Costura.Fody.3.2.0, Fody.3.3.2, Newtonsoft.Json.{10.0.3,11.0.2}` from the master checkout's
populated `src\packages`. N2 must script this (nuget.exe via `$(NugetExePath)` or a copy step).

### Machine facts (probed)

SDKs: 8.0.426 / 9.0.319 / 10.0.113 / **10.0.402** (default). Runtimes include
**Microsoft.NETCore.App 5.0.17 and 10.0.13**. Targeting packs installed under
`C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref`: 3.1, **5.0.0**, 6–9, **10.0.13** — both
the as-is net5.0 and the bumped net10.0 builds need no downloads. .NET Framework reference
assemblies through **v4.6.1** installed (VS2017 legacy) → net461 projects build on modern MSBuild.
VS: 2017 Community, 2022 Pro, 18 Pro (MSBuild 18.11). vstest.console available from all three.
NuGet global cache has every PackageReference dependency of the sln (verified list).

---

## Q2. .NET 10 gap and TFM recommendation

**Probe results (decisive):**

1. *As-is (`net5.0`, no changes)*: solution builds green (SDK 10.0.402, pack 5.0.0 present; only
   `NETSDK1138` EOL warning). Golden NetCore leg: **72/72 FAIL** —
   `System.AccessViolationException` at `Tdl.EnvironmentsSymbol..ctor()` (inlined
   `Nitra.Declarations.SymbolBase::.ctor`) ← `Tdl2Json.JsonGenerator..cctor`, exit
   `0xC0000005`. Deterministic; `DOTNET_TieredCompilation=0` no effect; runtimeconfig pins
   `NETCore.App 5.0.0` with `rollForward: Major`, and since 5.0.17 satisfies it, the app executes
   **on the 5.0.17 runtime** (Major roll-forward only applies when the requested major is absent).
   Same IL assemblies run fine on .NET Framework (net461 leg 72/72 green) → old-runtime ×
   new-compiler-emit interaction, not a semantic compile defect.
2. *Bump (`net5.0 → net10.0`, one-line edit)*: restore+rebuild green (NU1510 noise about
   `System.ValueTuple` PackageReference), `dotnet tdlc.dll` completes a real compile, and the full
   golden suite is **144/144 green** (net10.0 build staged under the test's hardcoded `net5.0\`
   path — see N2.1 for the constant update).

**Recommendation: option (i) — bump `src\TdlCompiler` to `net10.0` (required by acceptance
«собирается на dotnet 10»); proven feasible end-to-end.** Option (ii) roll-forward-only is not
viable: the installed 5.0.17 runtime always wins for a net5.0 app, and that is the crashing combo;
forcing away from it would require runtimeconfig surgery anyway and still ships an EOL TFM.

**net461 companion: keep both legs — no deviation needed.** The `Tdl2Json` net461 sibling (with
Costura.Fody 3.2.0/Fody 3.3.2 weaving) and the net461 test/TestDll/Transformator projects all
build with VS18's MSBuild 18.11 and their tests pass (net461 leg 72/72, Transformator 20/20).
Dropping the net461 leg from the Core-branch test run is unnecessary; per AGENTS the leg also
documents FX-parity. Only constraint: build inside a VS developer command prompt (T4 +
packages.config restore ergonomics), which `Build.cmd` already provides.

TFM policy for the rest: keep `netstandard20` libraries (D2/D6 analog), keep net461 for the FX
companion and test/MSTest projects (they are *runners*, not the compiler; acceptance targets the
compiler/toolchain). Optional hygiene: `Tdl.VersionInfo` `netstandard13 → netstandard20`
(NETSDK1215), remove `System.ValueTuple` PackageReference from TdlCompiler (NU1510).

---

## Q3. Test surface, baseline commands, numbers

### Q3.1 Canonical commands (all verified on this machine)

```bat
:: from C:\RSDN\TDL-<tree>
call "C:\Program Files\Microsoft Visual Studio\18\Professional\Common7\Tools\VsDevCmd.bat" -arch=x64
set MSBUILDENABLEALLPROPERTYFUNCTIONS=1
msbuild src\Tdl.sln /t:Restore ^
  /p:NemerleBinPathRoot=C:\RSDN\nemerle\bin\Debug ^
  /p:NitraPath=C:\RSDN\nitra-n01\bin\Debug\Stage1 ^
  /p:NitraGrammarsPath=C:\RSDN\nitra-n01\Grammars\bin\Debug ^
  /p:NitraLanguageCompilerPath=C:\RSDN\nitra-n01\Nitra.LanguageCompiler\bin\Debug ^
  /p:Configuration=Debug /p:Platform="Any CPU"          &:: EXIT=0 (needs src\packages populated, Q1)
msbuild src\Tdl.sln /t:Build <same args>               &:: EXIT=0, ~2.5 min, zero errors

:: golden suite (72 cases x 2 toolchains; NUnit3TestAdapter 3.13 from packages folder)
"C:\Program Files\Microsoft Visual Studio\18\Professional\Common7\IDE\CommonExtensions\Microsoft\TestWindow\vstest.console.exe" ^
  bin\Debug\Tdl.Tests.dll /TestAdapterPath:src\packages\NUnit3TestAdapter.3.13.0\build\net35
:: Transformator (MSTest, net461)
<C:\...\same vstest.console.exe> bin\Debug\Tdl.Transformator.Tests.dll
```

`NitraPath/NitraGrammarsPath/...` come from the default layout once N1 rebuilds
`C:\RSDN\nitra\bin\Debug\Stage1` etc. (probe used nitra-n01 overrides instead). Test prerequisites
built by the sln itself: both tdlc legs + `bin\Debug\Tests\TestDll.dll`.
`dotnet test` is *not* the path for `Tdl.Tests` (classic csproj + packages.config); for
`Tdl.Transformator.Tests` (SDK-style with Microsoft.NET.Test.Sdk) `dotnet test` should also work
(vstest path proven; not separately probed).

### Q3.2 What was obtained (N0.1-probe nitra artifacts, nemerle TOP drop)

| Run | Result |
|---|---|
| Restore + full sln Build (as-is net5.0) | **GREEN**, zero errors; warnings only NETSDK1138 (net5.0 EOL), NETSDK1215 (netstandard13), NU5128 (pack layout, cosmetic), CS8002 (Mono.Options not strong-named) + Nemerle source warnings (N10003 etc., pre-existing) |
| `Tdl.Tests` vstest, tdlc as-is | **144 total: 72 passed (all `NetFramework`), 72 failed (all `NetCore`, net5.0-runtime AV)** — 3.4 min |
| `Tdl.Tests` vstest, tdlc bumped net10.0 (staged into `net5.0\` folder) | **144 passed / 0 failed** — 3 min 37 s |
| `Tdl.Transformator.Tests` vstest | **20 passed / 0 failed** (net461, MSTest) |
| Cross-toolchain output parity (net461 `tdlc.exe` vs net10.0 `tdlc.dll`, all 72 cases, raw `-out` no `-sample`) | **57/72 byte-identical (SHA256)**; 9 cases are negative/message cases where both legs exit non-zero symmetrically (no output); **6 differ only by JSON key/element ordering or order-derived generated names** (`ScenarioGeneratorUse{List,PlatformGroup,ProductGroup}`, `ScenarioGenerator-WithBarriers`, `test-0002`, `InitialSequenceCode` — e.g. `"win8-x86"/"win81-x86"` swapped; `Suite1` vs `Suite-Product1-1-Deploy5`), a .NET Framework vs .NET Core `Dictionary`/`HashSet` iteration-order difference, not a semantic divergence |

### Q3.3 What «all tests pass» means numerically

Baseline norm for the N2 acceptance gate: **Tdl.Tests 144/144 + Tdl.Transformator.Tests 20/20 =
164 green** (72 golden cases × {NetFramework, NetCore} + 20 MSTest). No known-red tests observed.

### Q3.4 Deviation — golden comparison is currently inert on this branch (pre-existing)

`TdlCompilerTests.RunTest` looks for `Tests\Samples\NetFramework-<Case>.json` /
`NetCore-<Case>.json` (scope-prefixed, per-leg samples introduced with the two-toolchain runner in
commit `03bbe32`), but the repo only contains plain `<Case>.json` masters (identical to `master`;
`git diff master kl/retarget-tdl -- Tests/Samples` = empty) — so *every* case silently falls into
`-compiler-messages-test` mode: today's 144 green tests validate exit codes + `// E:/W:/H:`
message expectations, **not JSON output diff**. Probe with prefixed copies of the 68 samples:
130 failed / 14 passed — the committed (master-line) goldens do not match this branch's output on
*either* leg (output intentionally diverged on the branch). Consequence for acceptance: reviving
plain-name comparison would require regenerating per-leg goldens (the per-scope naming exists
precisely to allow that, and Q3.2's ordering finding shows why one shared golden cannot work).
Decision for N0.3/orchestrator: (a) accept message-mode suite as the acceptance number (zero repo
churn, honest to current branch design), and/or (b) N2 atom generating
`NetFramework-*/NetCore-*` goldens from the current green build (both legs, per-leg).

### Q3.5 Not baselined yet → N2.x

Everything above was already baselinable with N0.1 nitra artifacts; nothing needed a nitra
rebuild. Still pending: the same runs from the *real* layout (`C:\RSDN\nitra` after N1) and from
a pristine `bin\`/`obj\` (`Rebuild` semantics), plus `Build.cmd`-as-entry verification.

---

## Q4. Consumer-API mismatches vs new nemerle/nitra drops

| # | Item | Evidence | Verdict | Minimal adaptation |
|---|---|---|---|---|
| a | `$(Nemerle) = $(NemerleBinPathRoot)\Net-4.0` + imports `$(Nemerle)\Nemerle.Sdk.props/.targets` from nproj | `Tdl.nproj:16-17,42,353`; TOP drop has `Nemerle.Sdk.props`, `Nemerle.Sdk.targets`, `Nemerle.MSBuild.targets`, `Nemerle.dll`, `Nemerle.MSBuild.Tasks.dll` (netstandard2.0 task host-agnostic) | **compatible** (proven: nproj stages compiled by TOP `ncc-core`) | none; keep passing `/p:NemerleBinPathRoot=C:\RSDN\nemerle\bin\Debug` |
| b | Macro execution: MacroReference `Nitra.Compiler.dll`, Nitra grammars | `Tdl.nproj:81-84`; nitra-n01 `Stage1\Nitra.Compiler.dll` + `Grammars\bin\Debug\CSharp.Grammar.dll` loaded into `ncc-core` on net10; `Tdl.dll` emitted with Nitra AST metadata | **compatible** (proven end-to-end: compiled + ran real compiles) | none |
| c | `Nemerle.Macro.Resource("Properties/Resources.resx")` + `Nemerle.Macro.Settings` in `Tdl2JsonLib\Properties\AssemblyInfo.n:5-6` — the N0.1 Q2-f class | **Neither `Resources.resx` nor `Settings.settings` exists** in the project; nemerle TOP `macros/Resource.n:42-43` and `macros/Settings.n:155` early-return when the default-named file is missing (before the Core-host refusal at Resource.n:67) | **compatible / no-op** — the FX-only resx blocker does NOT fire for TDL | none (do not "fix" the attributes) |
| d | HintPath refs `$(NitraPath)\{Nitra.Runtime,DotNetLang,dnlib,DotNet.BackEnd.Dnlib,System.Collections.Immutable.Light}.dll`, `$(NitraGrammarsPath)\CSharp.Grammar.dll` | all present in nitra Stage1/Grammars outputs (N1 handoff set) | **compatible** | N1 must deliver those dirs in the real layout (N1.9) |
| e | `net5.0` TFM + RollForward | Q2: builds but **AVs on 5.0.17**; net10.0 build green | **blocker → bump to `net10.0`** (one-line, proven) | N2.1 |
| f | Test code hardcodes `bin\<Config>\net5.0\tdlc.dll` (`TdlCompilerTests.cs:17`) | probe staged net10 output to work around it | **adaptable** | N2.1 changes the constant to `net10.0` |
| g | net461 FX companion + Costura.Fody 3.2.0/Fody 3.3.2 + classic csproj + packages.config | builds on MSBuild 18.11 (VS18), tests green | **compatible** — keep both legs | scripts must populate `src\packages` (not restored by `/t:Restore`) |
| h | T4 in `Tdl.VersionInfo` (`Microsoft.TextTemplating.targets`) | VS18 ships `v18.0\TextTemplating`; green under VsDevCmd | **compatible with constraint**: run build inside a VS dev env (or set `VisualStudioVersion` + `DevEnvDir`) | document in build entry |
| i | `vswhere -version 16.0` in `Build.cmd` (plan doc called it dead) | vswhere range semantics = **≥16.0** → matches VS2022/VS18 on this box; loop takes the last | **not a blocker** (plan-doc claim corrected); entry still needs nitra-path awareness (defaults suffice after N1) | optional modernization |
| j | `NitraLanguageCompilerPath` / `Nitra.CommonIdePath` | used only by `Tdl.VisualStudio.Plugin` (not in sln) | **irrelevant** for the core build | none |
| k | Non-canonical TFMs `netstandard20`/`netstandard13` | accepted by SDK 10 (NETSDK1215 advisory only) | **compatible** | optional hygiene |
| l | Hardcoded dev paths: `Tdl.nproj` Debug `StartProgram`/`CmdArgs` (`C:\RSDN\TDL\...`), `TdlCompiler.csproj` `AssemblyOriginatorKeyFile=C:\!\RSDN\TDL\src\key.snk` (`SignAssembly=false`) | IDE-debug leftovers | **inert** (never evaluated in build) | optional cleanup |
| m | `System.CodeDom` / `ResXResourceReader` / UsingTask in TDL sources | git grep: zero hits (the `Tdl2Json.targets` UsingTask is a downstream-consumer MSBuild task package, not built against the compiler) | **compatible** | none |
| n | Golden-sample lookup scope-prefix (Q3.4) | branch code vs `Tests\Samples` content | **branch defect (test semantics), toolchain-independent** | N2 decision (a)/(b) |

No mismatch class blocks the build path; the only hard blocker was (e), resolved by the bump.

---

## Q5. Proposed N2 atomization (gates end at the user acceptance)

Gate commands: the Q3.1 command block (MSBuild 18 Full in VsDevCmd18, `NemerleBinPathRoot=C:\RSDN\nemerle\bin\Debug`);
after N1, nitra paths resolve by default from `C:\RSDN\nitra`, dropping the probe overrides.
Commits only in `kl/retarget-tdl` (via local `retarget-tdl`); nothing FX/VSIX-private per AGENTS.

1. **N2.1 TFM bump + test constant** — `src\TdlCompiler\TdlCompiler.csproj`
   `net5.0 → net10.0`; `src\Tdl.Tests\TdlCompilerTests.cs:17` `"net5.0" → "net10.0"`; drop the now
   pruning-noisy `System.ValueTuple` PackageReference (NU1510); keep `RollForward=Major`.
   Gate: sln Build zero-error; `dotnet bin\Debug\net10.0\tdlc.dll` compiles one real case;
   `Tdl.Tests` 144/144.
2. **N2.2 Build entry + restore ergonomics** — make the branch self-documenting build: update
   `Build.cmd` (or add `Build.ps1` mirroring nemerle's) to: resolve MSBuild modern
   (vswhere `-latest -requires Microsoft.Component.MSBuild`), set
   `MSBUILDENABLEALLPROPERTYFUNCTIONS=1`, keep `NemerleBinPathRoot` default, and ensure
   packages.config packages are restored (nuget.exe via `$(NugetExePath)` or documented copy) —
   plus a `src\Nitra.user.example`/doc note. Gate: documented single entry green from a clean
   `bin\`/`obj\` (no manual property list).
3. **N2.3 Golden-comparison disposition** — implement the N0.3 decision on Q3.4: (a) record
   message-mode as the norm (no code change), and/or (b) commit per-leg goldens
   `Tests\Samples\{NetFramework,NetCore}-<Case>.json` generated from the green build (per AGENTS
   flow: outputs land in `%TEMP%\Tdl2Json-*`; both-leg ordering differences make per-leg files
   mandatory), then comparison is live on both legs. Gate: chosen state green; if (b),
   demonstrate the comparison bites (mutate a copy → matching case fails).
4. **N2.4 Full acceptance run (the user gate)** — real layout after N1 (no `/p:` nitra overrides),
   `src\Build.cmd` (or the N2.2 entry), `Rebuild`-clean, then vstest `Tdl.Tests` +
   `Tdl.Transformator.Tests`. Gate = acceptance: **144/144 + 20/20 green on dotnet 10**; record
   exact numbers in the checklist.
5. **N2.5 Hygiene (optional, separate small commit)** — `Tdl.VersionInfo` netstandard13→2.0
   (NETSDK1215), remove dead `AssemblyOriginatorKeyFile` path and stale Debug `StartProgram`,
   JsonToTdl HintPath pointing at a non-restored Newtonsoft 12.0.1 (currently resolved by
   fallback — verify before touching).

Handoff note for the orchestrator (checklist N0.3): plan-doc facts corrected — the vswhere pin is
NOT a blocker (Q4 i); resx-macro risk did NOT materialize (Q4 c); the single required code change
for the Core bump is 2 lines (Q5 N2.1); and «all tests pass» currently means message-mode checks
(Q3.4) unless option (b) is taken.
