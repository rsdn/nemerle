# Progress — Phase 2.5: dnlib 3.3.0 → 4.5.0 bump + emitted-metadata characterization

Branch `retarget-compiler` @ a42c896d0, working dir `C:\RSDN\nemerle`. Orchestrator
runs the full CompilerTests/Validate chain; this task did NOT commit and did NOT
run that chain. Report: `retarget-compiler-to-net10-plan-report2.5.md`.
Scratch/IL dumps: `%TEMP%\opencode\il-diff\`.

## Step 1-2 — Bump (DONE)

`Nemerle.Compiler.nproj:39-40` (single 2-line diff, CRLF preserved):

| Package | before | after |
|---|---|---|
| dnlib | 3.3.0 | **4.5.0** |
| System.CodeDom | 4.7.0 | **10.0.14** (latest stable per nuget.org flatcontainer; 11.0.0 has only preview/RC) |

Nothing else added; no explicit System.Reflection.Emit* refs.

## Step 3 — Restore (DONE)

`dotnet restore Nemerle.Compiler.nproj` green. Resolved closure in
`obj\Nemerle.Compiler\project.assets.json` (target netstandard2.0):
dnlib/4.5.0, System.CodeDom/10.0.14, and transitive
**System.Reflection.Emit / .ILGeneration / .Lightweight all 4.7.0** (was 4.6.0;
matches report "Package-asset facts"). These facades produce **no compile-time
assemblyrefs** (see Step 5), only deps.json/runtime entries.

## Step 4 — Classic Stage1 build (DONE)

`cmd /c "`"C:\RSDN\nemerle\Build-core-base.cmd`" Debug Build Stage1"` → **exit 0**,
0 errors, 20 pre-existing warnings, ~36 s. **Zero ncc source changes needed** —
confirms report Q2/Q5 (API-identical for the 127 repo-used members).
Stage1 dnlib.dll = 4.5.0.0.

## Step 5 — IL characterization vs `C:\RSDN\dnlib330-snapshot` (DONE)

Method: SHA256 per file; for differing files ildasm (`/text /nobar /utf8 /out=`)
both sides, normalize `^//.*` → `// REPLACED`, `fc.exe /N`, then hunk
classification with an additional normalization of compiler-generated unique
names (`_N_*_<digits>` → `_N_<n>`, fixpoint).

Gotchas found and fixed en route:
- **PDB asymmetry**: snapshot has no `.pdb` files, Stage1 does → ildasm
  auto-loads them and emits local-var names/sequence points only for the new
  side. Redone with both sides dumped from PDB-free copies (`bin-old/`,
  `bin-new/`).
- **Baseline is git-drifted**: snapshot = `pre-net10-baseline-4` (assembly
  version 10.0.4.0); current HEAD = `-6-` (10.0.6.0) — version stamp comes from
  `git describe --tags --long` via `GetGitTagRevision` MSBuild task. To
  separate drift from the dnlib variable, a **control Stage1 build at HEAD with
  dnlib 3.3.0** (stash→build→`control-330-at-HEAD\`→pop→rebuild 4.5.0) was run.

SHA256 results (snapshot vs Stage1-4.5.0): **Nemerle.dll IDENTICAL**; ncc.exe,
Nemerle.Compiler.dll, Nemerle.Macros.dll differ. Attributed hunk classes after
normalization:

1. **Assemblyref version bumps (in scope, in Nemerle.Compiler.dll only):**
   - dnlib **3.3.0.0 → 4.5.0.0** (the bump itself)
   - System.CodeDom **4.0.3.0 → 10.0.0.0** (from our CodeDom package bump)
   - Nemerle.Compiler self-version + `Nemerle`/`Nemerle.Compiler` extern refs
     **10.0.4.0 → 10.0.6.0** and AssemblyFileVersion attr blob `10.0.4`→`10.0.6`
     — pure git-revision drift (snapshot 2 commits older), proven identical
     in the 3.3.0 control build; NOT dnlib.
   - Expected "SRE 4.6.0.0→4.7.0.0" diff: **does not exist** — Nemerle.Compiler
     has zero `System.Reflection.Emit*` assemblyrefs in either version (facades
     are runtime-only via deps.json).
2. **Out-of-assemblyref/version scope — exactly one class:** in
   **Nemerle.Macros.dll** only, 28 changed custom-attribute blob lines (56 ±
   lines): compiler-generated unique-name **counter suffixes embedded in macro
   string blobs** shift by a constant ~35-43 (e.g. `..._5342` → `..._5377`,
   `N_wildcard_5143` → `_5178`). Control-attribution: absent in
   snapshot→control (drift-only) diff, present in control→new (dnlib-only)
   diff ⇒ genuinely caused by the dnlib 4.5.0 emitter (unique-name counters
   advance slightly differently). Cosmetic name-heap/CA-string values only —
   zero instruction, method-body, layout-order, or table-structure changes
   (all IL_ offsets and member lists match 1:1).

Pure-effect matrix (normalized changed lines):
- dnlib only (control→new): ncc.exe **0**, Nemerle.Compiler.dll **4**
  (= exactly the 2 assemblyref bumps), Nemerle.Macros.dll **56** (counter blobs).
- drift only (snapshot→control): ncc.exe 6, Nemerle.Compiler.dll 4,
  Nemerle.Macros.dll 6 — all version-stamp lines (`.ver 10:0:4:0→10:0:6:0`,
  AssemblyFileVersion blob).
- ncc.exe is IL-identical between control and new (0 normalized lines; raw SHA
  differs only in non-IL PE fields ildasm does not print, e.g. checksum).

**Verdict: no diff class outside (a) assemblyref/version stamps and (b) compiler
unique-counter suffix values.** No behavior-visible metadata change detected at
Stage1; the report's MEDIUM risk (SigComparer/TypeSig semantics) stays gated on
the Stage2 `Validate` IL-diff, which the orchestrator runs.

## Step 6 — PDB format fixation (Phase 2.5.6 note, DONE)

Stage builds still emit **Windows (native MSF) PDBs**, unchanged by the bump:
all four Stage1 `.pdb` files (Nemerle.Compiler, Nemerle, Nemerle.Macros, ncc)
start with `4D 69 63 72 6F 73 6F 66 74 20 43 2F 43 2B 2B` = "Microsoft C/C++ "
(MSF 7.00 magic), **not** `BSJB` (portable). Default `PdbType=Windows` path
confirmed alive on dnlib 4.5.0 with the .NET Framework classic host (diasymreader
available there). Report Q4's real work (Microsoft.DiaSymReader.Native or
portable default) stays a **net10-host** requirement, not a bump blocker.

## Step 7 — Sanity runtime (DONE)

`bin\Debug\net-4.0\Stage1\ncc.exe -help` → **exit 0** on the Framework host with
the stage folder's dnlib 4.5.0.0 (final rebuild after control experiment
verified again).

## Status log

- [x] Plan reviewed, baseline snapshot located (`C:\RSDN\dnlib330-snapshot\`)
- [x] Step 2: nproj bump (dnlib 4.5.0, System.CodeDom 10.0.14)
- [x] Step 3: restore + assets record (SRE* transitive = 4.7.0)
- [x] Step 4: Stage1 build exit 0, zero source changes
- [x] Step 5: IL characterization (PDB-asymmetry fixed; control build isolates
      dnlib effect; only counter-suffix blobs outside version scope)
- [x] Step 6: PDB format fixation (Windows MSF PDBs unchanged)
- [x] Step 7: ncc.exe -help exit 0

## Not done (by instruction)

No commit; no CompilerTests/Validate/Stage2 (orchestrator). Working tree keeps
the 2-line `Nemerle.Compiler.nproj` bump + this progress file.
