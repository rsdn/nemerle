# Progress 3.2 — D2/D6 verification after the net10 host migration

Branch `retarget-compiler`. Spec: plan §D2/§D6 + checklist 3.2 — shipped Nemerle
libraries stay `netstandard2.0` (NOT `net10.0`-only) and must load BOTH from a
.NET 10 host AND from a .NET Framework 4.6.1+ host. Evidence-only atom: expected
ZERO code changes; any code-level problem = new atom candidate (proposed below).
Do NOT commit. No repository build entry points were run — probes used the
existing fresh Stage3 output only.

Scratch: `C:\RSDN\nemerle\.tmp\phase32\` (probes, ildasm dumps, PS scripts,
`fx\` and `fx-bare\` load-probe folders).

## Intentions

1. **A** Static TFM audit of every shipped library project (`git grep TargetFramework`).
2. **B** IL manifest check of the Stage3 shipped assemblies (ildasm dumps → `.tmp\phase32\`).
3. **C** net10 host load probe: `ncc-core.exe` compiles a trivial `.n` AND macro-heavy
   sources that execute code from Nemerle.Macros.dll.
4. **D** FX host load probe: `LoadFrom` the fresh Stage3 libraries inside Windows
   PowerShell 5.1 (CLR 4 / FX process), distinguish "not ns2.0-loadable" from
   "dependency not co-located"; registry-confirmed FX >= 4.6.1.
5. **E** Report + verdict.

## Results — ACCEPTANCE A–E: PASS (D2/D6 holds); zero code changes made

### A — Static TFM audit: PASS

Shipped set = Stage3 outputs (what `Install` copies): the 4 libraries + ncc-core host.

| Project | Line | TFM (verbatim) | Role | Verdict |
|---|---|---|---|---|
| `Nemerle.nproj` | :20 | `<TargetFramework>netstandard2.0</TargetFramework>` | library (shipped) | ✅ |
| `Nemerle.Compiler.nproj` | :19 | `<TargetFramework>netstandard2.0</TargetFramework>` | library (shipped) | ✅ |
| `Nemerle.Macros.nproj` | :18 | `<TargetFramework>netstandard2.0</TargetFramework>` | library (shipped) | ✅ |
| `Nemerle.MSBuild.Tasks.csproj` (repo root; compiles `tools\msbuild-task\*.cs`, lines 25-28 — there is no separate csproj under `tools\`) | :7 | `<TargetFramework>netstandard2.0</TargetFramework>` | library (shipped) | ✅ |
| `ncc-core.nproj` | :16 | `<TargetFramework>net10.0</TargetFramework>` | host executable | expected net10.0 |
| `ncc.nproj` | :16 | `<TargetFramework>net461</TargetFramework>` | host executable — dormant, removed from stage graph in 3.4b (`NemerleAll.nproj:61-64`) | expected, out of graph |

Graph membership (`NemerleAll.nproj:47-64`): `NCompilerProject` = Nemerle.nproj,
Nemerle.Compiler.nproj, Nemerle.Macros.nproj, ncc-core.nproj; `NTasksProject`
(MSBuild) = Nemerle.MSBuild.Tasks.csproj. All libraries in-graph = netstandard2.0.

Other projects found (informational, all outside the shipped stage set):

- `Nemerle.Location/Nemerle.Location.nproj:13` `TargetFrameworkVersion=v4.6.1` —
  **standalone project NOT in the stage graph**; referenced only by legacy
  `*-dnlib` projects (`ncc-dnlib`, `Nemerle.Macros-dnlib`, `ncc/Nemerle.Compiler-dnlib`,
  `Nemerle.Compiler-dnlib.sln`). Its sources are compiled INTO `Nemerle.Compiler.dll`
  via `Nemerle.Compiler.nproj:59` (`Compile Include="Nemerle.Location\**\*.n"`).
  Verified: **no `Nemerle.Location.dll` in Stage3** (directory listing). Non-finding
  for D2/D6 (nothing ns2.0-consuming sees its TFM); cleanup is Phase 5/7 territory.
- In-graph snippets libraries are also netstandard2.0: `Linq\Macro\Linq.nproj:6`,
  `snippets\peg-parser\Nemerle.Peg{,.Macros}`, `snippets\csharp-parser\*`,
  `snippets\Nemerle.Async\*`, `snippets\ComputationExpressions\*`,
  `snippets\Nemerle.Unsafe`, `snippets\Nemerle.Diff` (all `netstandard2.0`).
- Harnesses excluded per spec: `snippets\Nemerle.Test\Nemerle.Compiler.Test.nproj:13`
  = `net461` (test runner, Phase 6.1 item). Legacy FX-era `TargetFrameworkVersion`
  v2.0–v4.7.2 values live only in out-of-graph snippets/IDE/NAIntools projects.
- `Nemerle.XBuild.Tasks.csproj:12` v3.5 — mono/NAnt-only tasks project, imported
  only when `$(UseMSBuild)` is empty (`NemerleAll.nproj:50-51`); never shipped here.

### B — IL manifest check (Stage3, ildasm)

Tool note (finding, non-blocker): ildasm 4.6.1 AND 4.8.1 with `/NOIL` emit ONLY
`// warning : THIS IS A PARTIAL DISASSEMBLY...` (226 B file) for these
netstandard2.0-referencing assemblies — same family of tooling limitation as the
known-red pefverify. Without `/NOIL` (exact Validate invocation `/out=… /nobar`)
full dumps succeed. Full manifests saved as `.tmp\phase32\manifest-<dll>.txt`:

| Assembly | TargetFrameworkAttribute (blob, ildasm ASCII comment) | Key `.assembly extern` lines |
|---|---|---|
| Nemerle.dll | `[netstandard]…TargetFrameworkAttribute::.ctor(string) = ( 01 00 19 2E 4E 45 54 53 74 61 6E 64 61 72 64 2C 56 65 72 73 69 6F 6E 3D 76 32 2E 30 …` → **".NETStandard,Version=v2.0"** (`.ver 10:0:19:0`) | `.assembly extern netstandard` `.ver 2:0:0:0`, `.publickeytoken = (CC 7B 13 FF CD 2D DD 51)` |
| Nemerle.Compiler.dll | same ".NETStandard,Version=v2.0" attribute | `netstandard .ver 2:0:0:0`; `dnlib .ver 4:5:0:0`; `Nemerle .ver 10:0:19:0`; `System.CodeDom .ver 4:0:3:0` |
| Nemerle.Macros.dll | same ".NETStandard,Version=v2.0" attribute | `netstandard .ver 2:0:0:0`; `Nemerle.Compiler`; `Nemerle` |
| Nemerle.MSBuild.Tasks.dll | same ".NETStandard,Version=v2.0" attribute | `netstandard .ver 2:0:0:0`; `Microsoft.Build.Utilities.Core/.Framework .ver 15:1:0:0` |
| ncc-core.dll | ".NETCoreApp,Version=v10.0" (attribute host = `[System.Runtime]`) | `System.Runtime .ver 10:0:0:0` — Core/shared-runtime surface, as expected for the host |

All four shipped libraries reference ONLY the `netstandard 2.0.0.0` facade as
runtime surface → ns2.0 confirmed at IL level.

### C — net10 host load probe: PASS

Host: `bin\Debug\net-4.0\Stage3\ncc-core.exe` (`ncc-core.runtimeconfig.json`:
`"tfm": "net10.0"`, framework `Microsoft.NETCore.App 10.0.0` — no roll-forward
hack). Run from `C:\RSDN\nemerle\.tmp\phase32`:

```
& C:\RSDN\nemerle\bin\Debug\net-4.0\Stage3\ncc-core.exe -nologo -g -target:library -out:probe-core.dll ProbeTrivial.n
  (ProbeTrivial.n = `class C { }`)
→ warning N10003 only; EXIT=0; probe-core.dll 2,048 B + probe-core.pdb 280 B ✅
```

Macro-heavy sources (all `-nologo -g -target:library`):

1. `testsuite\positive\AsLongBug-1.n` — top-level `macro macro1 (expr)` with
   `<[ ]>` quotations, `$(x : long)` antiquotes, PExpr `match`
   (+ `-r:…Stage3\Nemerle.Compiler.dll`): **EXIT=0**, probe-macro.dll 6,656 B + pdb.
2. `testsuite\positive\Issue-1290-macro.n` — `[MacroUsage(WithTypedMembers, Assembly)]`
   macro calling `Macros.ImplicitCTX()` (+ `-r:Nemerle.Compiler.dll`): **EXIT=0**.
3. Direct **Nemerle.Macros.dll** execution — `ProbeMacrosDll.n` uses
   `Nemerle.Imperative.ResetMutableFields` (`using Nemerle.Imperative;` + call):
   **EXIT=0**, probe-macros.dll. Proof the implementation came from
   Nemerle.Macros.dll: `git grep ResetMutableFields -- lib macros ncc` hits ONLY
   `macros/core.n` (→ Nemerle.Macros.dll) + ncc's host bootstrap; 0 hits in the
   ildasm dump of Nemerle.dll (`.tmp\phase32\Nemerle-classes.il`).

Side observation (no action): macro-attribute custom macros from an explicitly
`-r:`'d Nemerle.Macros.dll (`[Record, StructuralEquality]` consumer) do not get
discovered via plain `-r` on the standalone command line (harness `//MACRO:`
uses a dedicated macro-assembly path); the `Nemerle.Imperative` route above is
the standalone equivalent and works. Not a D2/D6 concern (harness-based
CompilerTests are green per 3.4b).

### D — FX host load probe: PASS

Host process: `C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe`
→ `PSEdition=Desktop PSVersion=5.1.26100.9711 CLR=4.0.30319.42000` (real
.NET Framework process, not pwsh).

Registry: `(Get-ItemProperty 'HKLM:\SOFTWARE\Microsoft\NET Framework Setup\NDP\v4\Full').Release`
= **"533509"** (`Version 4.8.09221`). Mapping used: 394254 = 4.6.1 (MSDN table);
533509 ≥ 394254 → **4.6.1+ requirement satisfied** (machine is 4.8.x).

Attempt 1 — `fx-bare\` (only the 3 Nemerle dlls copied; netstandard facade served
from the 4.8 GAC): `fx-probe.ps1` →

```
LOAD-OK Nemerle.dll        => Nemerle, Version=10.0.19.0, PublicKeyToken=e080a9c724e2bfcd
LOAD-OK Nemerle.Macros.dll => Nemerle.Macros, Version=10.0.19.0, PublicKeyToken=5291d186334f6101
LOAD-OK Nemerle.Compiler.dll => Nemerle.Compiler, Version=10.0.19.0, PublicKeyToken=5291d186334f6101
ACTIVATE-OK Nemerle.dll => Nemerle.Internal.NemerleAttribute (Activator.CreateInstance) ✅
TYPE-OK Nemerle.Compiler.dll => Nemerle.Compiler.Macros
```

Deep binding (`fx-deep.ps1`, `Assembly.GetTypes()` = bind EVERY type signature):

- `fx-bare\`: `DEEP-OK Nemerle.dll` (412 types), `DEEP-OK Nemerle.Macros.dll`
  (528 types), `DEEP-PARTIAL Nemerle.Compiler.dll` — every LoaderException is
  `FileNotFoundException: Could not load file or assembly 'dnlib,
  Version=4.5.0.0, …' / 'System.CodeDom, Version=4.0.3.0, PublicKeyToken=cc7b13ffcd2ddd51'`
  → **"dependency not co-located", NOT "library not ns2.0-loadable"** (no
  BadImageFormat / BadFileVersion / manifest exceptions at all).
- `fx\` retry (fresh Stage3 `dnlib.dll` 1,178,624 B + `System.CodeDom.dll`
  25,192 B copied alongside): **`DEEP-OK Nemerle.Compiler.dll` (1774 types)** —
  all three shipped libraries fully bound in the FX process. ✅

Extra: `Nemerle.MSBuild.Tasks.dll` → `LOAD-OK Nemerle.MSBuild.Tasks, Version=1.0.0.0`
(manifest level). Full type binding needs `Microsoft.Build.*` — supplied by any
MSBuild host, not a runtime-shipped dependency; expected, non-blocker.

Residual FX binding notes (all non-blockers):

| # | Item | Rating |
|---|---|---|
| D-1 | `Nemerle.Compiler.dll` needs `dnlib.dll` + `System.CodeDom.dll` co-located on FX (LoadFrom context probes the app dir — satisfied automatically in a real appbase). Both are present in Stage3 and shipped by `Install` (`$(NCurBin)\*.*`). | non-blocker |
| D-2 | `netstandard 2.0.0.0` bound silently from the 4.8 GAC facade (ships since 4.7.2). Hosts pinned to 4.6.1–4.7.1 would need the co-located `netstandard.dll` facade (98,616 B — present in Stage3). Not testable on this machine (no ≤4.7.1 runtime). | non-blocker / untested edge |
| D-3 | `Nemerle.MSBuild.Tasks.dll` types bind `Microsoft.Build.{Framework,Utilities.Core} 15.1.0.0` — host-provided in MSBuild/VS. | non-blocker |

### E — Verdict

**D2/D6 HOLDS.** Every shipped library remains `netstandard2.0` at project level
(A) and IL-metadata level (B); the freshly rebuilt (green-validated) Stage3 set
loads and executes on .NET 10 (compiler drives its own ns2.0 libs incl. macro
execution, C) AND fully binds inside a real .NET Framework 4.8 process (D).
Zero code changes were needed; none were made. No commits.

### Proposed follow-up atoms (report-only, NOT implemented)

1. **`nuspec-pack-list`** — `Nemerle.Compiler.nuspec:19-20` still packs `ncc.???`
   / `ncc??.???` (no longer produced since 3.4b) and both `Nemerle.nuspec` /
   `Nemerle.Compiler.nuspec` pack the ns2.0 assemblies under `lib\net\`. Before
   any `_NugetPack`/Phase-6 packaging work: fix the file list (drop `ncc`, keep
   `ncc-core` artifacts) and choose the folder moniker deliberately (`lib\net\`
   targets FX consumers; `lib\netstandard2.0\` is what Core/SDK consumers
   restore). Files: `Nemerle.Compiler.nuspec`, `Nemerle.nuspec`. Non-blocking
   for 3.2 (packaging is Phase 6/7).
2. **`legacy-ildasm-noil`** (Phase 5.1 synergy) — record/retire the fact that
   NETFX 4.6.1/4.8.1 `ildasm /NOIL` emits only a PARTIAL-DISASSEMBLY warning for
   these ns2.0 assemblies (manifest-only dumps unusable; full dumps fine);
   same tool-generation issue as the known-red pefverify step.
3. **`nemerle-location-cleanup`** (already tracked from 3.4b) —
   `Nemerle.Location.nproj` standalone project is FX-era `v4.6.1` with dead
   `StartProgram→Stage1\ncc.exe`; not in the stage graph (sources compile into
   Nemerle.Compiler.dll); rehome or delete in Phase 5/7.
