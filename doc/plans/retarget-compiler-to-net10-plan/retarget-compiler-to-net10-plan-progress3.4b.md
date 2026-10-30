# Progress 3.4b — drop `ncc.nproj` from the stage build graph

Branch `retarget-compiler` @ `f0ae1b1b0` (3.4a = `ae2611a61` is in). Do NOT commit.
Spec: `retarget-compiler-to-net10-plan-report3.1-3.5.md` row **3.4b** — “delete
`<NCompilerProject Include="$(NRoot)\ncc.nproj"/>` (`NemerleAll.nproj:61`); `Validate`'s
`ncc*.exe` glob keeps matching the apphost” + retire `ncc\packages.config`.
Flat-dir / Validate design context: `retarget-compiler-to-net10-plan-report3.1c-3.1d.md`.

## Intentions

1. `NemerleAll.nproj`: remove the `ncc.nproj` entry from `NCompilerProject` (line 61).
   Keep `ncc.nproj` + `ncc\App.config` tracked on disk (file deletion = Phase 7.1).
2. `git rm ncc\packages.config` — after confirming nothing consumes it.
3. One-time manual cleanup of STALE `ncc.exe` / `ncc.exe.config` (and the `ncc.pdb` /
   `*.il` siblings they leave behind) in `bin\Debug\net-4.0\Stage{1..4}\` so the
   Validate `ncc*.exe` globs and `fc *.il` don't see pre-3.4b copies.
4. Zero Validate edits expected — the `ncc*.exe` (Exclude `ncc-*.exe`) globs
   self-shrink to empty once no stage project emits `ncc.exe`; `Il2/Il3` keep the
   4 `Nemerle*.dll` + `ncc-core.dll`.
5. Gates: A) forced `Rebuild "Stage3,Validate"`; B) Stage2/Stage3 dir contents +
   `ncc-core.exe -version`; C) `Build "Stage1,CompilerTests"` → 468+167/0.

## Consumer audit (`git grep -n "ncc\.nproj\|ncc\.exe"` over `*.nproj snippets tools
## doc/NET10-BUILD-BASELINE.md Build-core-base.cmd` + nuspec/targets/scripts)

| # | Consumer | Verdict | Action |
|---|---|---|---|
| 1 | `NemerleAll.nproj:61` `<NCompilerProject Include="$(NRoot)\ncc.nproj"/>` | the entry itself | **remove** |
| 2 | `NemerleAll.nproj` Validate `Asm2/Asm3` globs `ncc*.exe` Excl `ncc-*.exe` (321-324) | matches `ncc.exe` only while it lands in stage dirs; goes empty after this change (glob, not hard requirement); `ncc-core.exe` stays excluded from ildasm/peverify as before | none (self-shrinks) |
| 3 | `NemerleAll.nproj` Validate `fc *.il` (351) + pefverify over `%(Asm3.FullPath)` (357) | `fc` globs `*.il` → auto-shrinks; pefverify item set = `Nemerle*.dll` only after stale `ncc.exe` removed from Stage3 | none; pre-clean stale files |
| 4 | `NemerleAll.nproj:226-238` `NPrepareBoot` copies `$(NRootBoot)\*.config` | boot seed (`boot-dnlib\`) config for the seed's `ncc.exe`, NOT an `ncc.nproj` output | none (do-not-touch area) |
| 5 | `NemerleAll.nproj` `Install` (419) copies `$(NCurBin)\*.*` | wildcard, no explicit `ncc.exe` | none |
| 6 | `CompilerTests` target (363-393) | harness hosts compiler in-process; copies `TestFramework\*.*` + testsuite; no `ncc.exe` | none |
| 7 | `ncc32.nproj:9` / `ncc64.nproj:9` `<Import Project="...\ncc.nproj"/>` | not referenced anywhere in `NemerleAll.nproj` (grep `ncc32|ncc64` → 0 hits) — dead in graph; project file itself stays | none |
| 8 | `Nemerle.Location/Nemerle.Location.nproj:30` `<StartProgram>...\Stage1\ncc.exe` | VS IDE F5 debug setting only; not consumed by any build target; after 3.4b the file won't be produced → F5 would fail until someone re-points it at `ncc-core.exe` (IDE-only, out of scope) | report only |
| 9 | `snippets/**` Makefiles/`compile.bat`/`debug_build.bat`, `tools/{cs2n,htmldumper,nant-task,nemerle-unit}/makefile*`, `tools/msbuild-task/ncc-mono*` | legacy NAnt/make/mono paths, outside the MSBuild stage graph (report3.1-3.5 row 11: “clean up in Phase 5/7”) | none |
| 10 | `tools/msbuild-task/MSBuildTask.cs:102` `ncc.exe` fallback + `:113` prefer `ncc-core.exe` | 3.4a design: fallback kept intentionally; not in do-not-edit list but requires no edit | none (do-not-touch anyway) |
| 11 | `tools/snippetcompiler-addin/src/AssemblyLoader.n:61` (App Paths registry) | IDE add-in, not in build graph | none |
| 12 | `doc/NET10-BUILD-BASELINE.md`, `Build-core*.cmd`, `*.nuspec`, `Nemerle.MSBuild.targets`, `Nemerle.Sdk.{props,targets}` | grep: zero `ncc.exe`/`packages.config` hits | none |
| 13 | `ncc\packages.config` (dnlib 3.2.0/net461) | grep `packages.config` in `ncc.nproj`/`NemerleAll.nproj`/`ncc-core.nproj` → 0 hits; ncc.nproj resolves dnlib 4.5.0 via its own assets, the file is a NuGet-2.x leftover; with `ncc.nproj` out of the graph it is never restored | **git rm** |
| 14 | stale `bin\Debug\net-4.0\Stage{1,3}\{ncc.exe,ncc.exe.config,ncc.pdb}`, `Stage{1,2}\*.il` | would pollute `ncc*.exe` globs / `fc` if left; nothing deletes them automatically | **one-time delete** |

No live consumer hard-requires the built `ncc.exe` at build/test time → proceed.

## New expected Validate norm

Pre-3.4b FC pairs ×6 = Nemerle.dll, Nemerle.Compiler.dll, Nemerle.Macros.dll,
Nemerle.MSBuild.Tasks.dll, **ncc.exe**, ncc-core.dll.
Post-3.4b expected FC pairs ×**5**: `Nemerle.Compiler.il`, `Nemerle.il`,
`Nemerle.Macros.il`, `Nemerle.MSBuild.Tasks.il`, `ncc-core.dll.il`.
peverify iterates `Asm3` = the 4 `Nemerle*.dll` only (known-red on
`Stage3\Nemerle.Compiler.dll` stays the sole MSB error).

## Changes made

1. `NemerleAll.nproj` — removed `<NCompilerProject Include="$(NRoot)\ncc.nproj"/>`
   from the `NCompilerProject` item group (with a 3.4b comment); single hunk at
   `@@ -58,7 +58,9 @@`. File size 36,738 B, UTF-8 BOM (`EF BB BF`) intact.
2. `git rm ncc\packages.config` (legacy `dnlib 3.2.0/net461` NuGet-2.x leftover;
   audit row 13: zero consumers).
3. One-time stale delete in `bin\Debug\net-4.0\Stage{1,2,3}\`: `ncc.exe`,
   `ncc.exe.config`, `ncc.pdb`, leftover `*.il` (Stage4 dir absent).
4. Zero Validate edits — globs self-shrank exactly as predicted.

## Results — ACCEPTANCE A–D: PASS

### A — forced `Rebuild "Stage3,Validate"` (exit 1 = known-red only)

Log `.tmp\phase34b-logs\stage3-validate.log` (+ `-exit.txt`, `exit=1`).
`FC: no differences encountered` ×**5** (new norm). `fc` pairs:

```
Comparing files ...\STAGE2\ncc-core.dll.il and ...\STAGE3\ncc-core.dll.IL
Comparing files ...\STAGE2\Nemerle.Compiler.dll.il and ...\STAGE3\Nemerle.Compiler.dll.IL
Comparing files ...\STAGE2\Nemerle.dll.il and ...\STAGE3\Nemerle.dll.IL
Comparing files ...\STAGE2\Nemerle.Macros.dll.il and ...\STAGE3\Nemerle.Macros.dll.IL
Comparing files ...\STAGE2\Nemerle.MSBuild.Tasks.dll.il and ...\STAGE3\Nemerle.MSBuild.Tasks.dll.IL
```

Sole `: error MSB` (×2 echoes of the same error):
`NemerleAll.nproj(359,5): error MSB3073: The command "...\peverify.exe"
"...\Stage3\Nemerle.Compiler.dll" exited with code 1` — the known-red
(FX-4.6.1 pefverify on the netstandard2.0 `Nemerle.Compiler.dll`, 0x80131252
[MD] noise). Zero “no valid CLR header”. pefverify iterated only
`Stage3\Nemerle.Compiler.dll` (first Asm3 item, fails there;
**no ncc.exe** among its inputs). ildasm ran 5 invocations per stage dir
(4 × `Nemerle*.dll` + `ncc-core.dll`, zero for `ncc-core.exe`).

### B — stage dirs contain only the net10 host

`dir /b bin\Debug\net-4.0\Stage3\ncc*` and same for Stage2 →
`ncc-core.deps.json ncc-core.dll ncc-core.dll.il ncc-core.exe ncc-core.pdb
ncc-core.runtimeconfig.json`; explicit probes:
`dir Stage3\ncc.exe` → **File Not Found**, `dir Stage3\ncc.exe.config` →
**File Not Found** (same for Stage2). Both hosts run:

```
Stage3\ncc-core.exe -version → Nemerle Compiler (ncc) version 10.0.19.0 (git) ... exit 0
Stage2\ncc-core.exe -version → Nemerle Compiler (ncc) version 10.0.19.0 (git) ... exit 0
```

### C — `Build "Stage1,CompilerTests"` (exit 0)

Log `.tmp\phase34b-logs\compilertests.log` (+ `-exit.txt`, `exit=0`):
**468** positive `passed` lines + **167** negative `passed` lines, zero lines
ending in `failed`; `All tests passed. Time taken: 00:02:04.53` and
`All tests passed. Time taken: 00:00:31.23`; `Build succeeded.`; zero MSB errors.

### D — `git status --short` scope

```
 M NemerleAll.nproj                                   <- 3.4b
D  ncc/packages.config                                 <- 3.4b (git rm)
?? .../retarget-compiler-to-net10-plan-progress3.4b.md <- this doc
 M doc/plans/retarget-compiler-to-net10-checklist.md   <- NOT this atom: pre-existing (checklist row 3.4b marked “выполняется” by the orchestrator)
 M .../...-progress3.4a-fix.md                         <- NOT this atom: 3.4a-fix executor checked their own boxes after this session started
?? AGENTS.md                                           <- pre-existing untracked
```

3.4b-touched files = `NemerleAll.nproj` + `ncc/packages.config` (deleted) +
this progress doc. No live consumer file needed an edit (audit rows 2-12 all
“none”). Not committed, per instruction.

## New FC norm (record for future gates)

**FC ×5**: `Nemerle.Compiler.il`, `Nemerle.il`, `Nemerle.Macros.il`,
`Nemerle.MSBuild.Tasks.il`, `ncc-core.dll.il`. pefverify Asm3 set = 4
`Nemerle*.dll` (known-red on `Stage3\Nemerle.Compiler.dll` first → sole MSB
error). The `ncc*.exe` globs in Validate now match nothing and may be removed
together with the Asm/Il item split in a later cleanup (Phase 5.1/7); left
in place per “prefer zero Validate edits”.
