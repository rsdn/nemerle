# 9.1 — Branch-convergence research: `master` vs `retarget-compiler`

Research-only deliverable of plan phase 9 ("converge branches with master").
No tracked file was modified by this task; all builds ran in the untracked
output tree of the `sync-master` worktree (`nemerle-master` @ `b5f704623` =
`master`). Scratch data: `C:\RSDN\nemerle\.tmp\phase91\` (rename map
`layout_map.tsv`, build logs `q2_*.log`).

- Merge base: `01e527527` "Move tests outside ncc dir." (**2019-07-09**).
- `master` since base: **15 commits** (dormant line; tip 2025-03-05), touching
  only **18 files** — of the shared compiler source, just
  `ncc/external/LibraryReferenceManager.n`, `ncc/generation/HierarchyEmitter.n`,
  `ncc/main.n`, `testsuite/positive/Bugs/lexer1.n`.
- `retarget-compiler` since base: **319 commits** (dnlib backend, the
  frontend/backend/shared layout split, .NET 10 host, `Nemerle.Location/`).
- Full tip-to-tip diff: 1653 files (dominated by the `VsIntegration/` →
  `snippets/VS2010/` IDE move and `doc/` net10 plans — both excluded from
  sync-pain accounting per AGENTS.md).

## Q1 — Inventory of divergence

`git diff --name-status -M40% master retarget-compiler -- ncc/ lib/ macros/ testsuite/`
(2-dot tip-to-tip; three-dot and two-dot give the same categories because
master's own changes fall inside buckets retarget already touched):

| kind | count | meaning |
|---|---|---|
| `R100` | **21** | pure renames — move already reduces the diff to **zero** |
| `R<100` | **45** | moved **and** content-drifted (the manual-port pain) |
| `A` | 128 | new on retarget (split children, dnlib-only, Location, tests) |
| `M` | 71 | same path, content drift (mainly `macros/` + testsuite markers) |
| `D` | 32 | deleted on retarget (9 genuinely dropped, rest split parents / FX-only tests) |

The `ncc/` subtree alone: 224 files differ (+27.4k/−29.5k lines), from 98
master-side files to 194 retarget-side files.

### Content drift of the 45 renamed+drifted shared files

Raw: **+6215/−7768** lines. But a large share of it is **style noise on the
retarget side**: license headers were deleted and code reformatted
(`this (` → `this(`, alignment whitespace) during the port. Whitespace- and
header-insensitive (`git diff -w`, per file), drift concentrates hard:

| file | ±changed lines (ws-insensitive) | character of drift |
|---|---|---|
| `ncc/backend/generation/ILEmitter.n` | 1692 | **intentional** (SRE→dnlib emit) |
| `ncc/frontend/typing/Typer/Typer3.n` | 959 | mostly backend-facing (emit typing) |
| `ncc/frontend/typing/Typer/Typer.n` | 797 | Location + dnlib usings + style |
| `ncc/backend/hierarchy/InternalTypeClass.n` | 546 | dnlib type model |
| `ncc/backend/hierarchy/LibraryReferenceManager.n` | 463 | dnlib reflection + missing master fix (below) |
| `ncc/frontend/ManagerClass.n` | 387 | host/options drift |
| `ncc/frontend/typing/FixedType.n` | 385 | Location refactor |
| `ncc/frontend/typing/Typer/Typer4.n` | 324 | emit typing |
| `ncc/frontend/parsing/MainParser.n` | 313 | Location refactor |
| `ncc/frontend/typing/Typer/Typer2.n` | 257 | misc |
| 35 more files | 2…210 each | mostly small, Location/`#if`/API drift |

≈ 4.3k of the ws-insensitive drift sits in the 6 files above (backend/host
machinery — *intentionally* divergent); the remaining ≈ 3.2k across 39 files
is the shared-language surface drift.

`M` (same-path) drift is **trivially small**: all 51 testsuite `M` files are
±1…±31 lines (`// NO-CORE` markers, `BEGIN-OUTPUT-CORE` blocks on the retarget
side); `macros/` (16 files, +533/−721) and `lib/core.n` (−31, header removal)
carry the dual-host/`#if` compat drift; `ncc/main.n` +24/−122 is the net10
host reduction.

### A/D buckets

- 16 `A` files are pure **split children** of master files (ExternalMemberInfo/*
  → `backend/hierarchy/Members/`, ExternalTypeInfo/* → `backend/hierarchy/Types/`,
  `Lexer.n` → `frontend/parsing/Lexer/`, etc.), plus the split-children of
  `ParseTree.n` (22 files), `TypedTree.n` (14), `AST.n`/ParseTree members.
  The split itself was content-preserving (verified at the 2019 "Refactoring"
  commits, e.g. `3a8b429cf` shows `R100`/`R09x` renames and header-preserving
  splits) but happened via **intermediate dirs** (`ncc/backend/external/…`)
  that no longer exist — so the map must be derived tip-to-tip, not from history.
- 13 `D` files have no retarget counterpart: `ncc/optimization/*` (8, the SRE
  CFG optimizer, dropped on retarget) and `ncc/generation/HierarchyEmitter.n`
  (SRE emitter) + split parents `ParseTree.n`, `TypedTree.n`, `Source.n`,
  `ClassMembers.n`. FX-only tests `gtk.n`, `pointer-type-caching.n` (deleted
  on retarget), `Bugs/lexer1.n` (master-side test retarget never got).
- All 21 `R100` + 45 `R<100` + split/basename map (85 entries) is saved in
  `.tmp/phase91/layout_map.tsv` and reproduced in the **Appendix**.

### Three-way view: who touched what (the real conflict set)

Files changed on **both** sides since the merge base: `LibraryReferenceManager.n`,
`HierarchyEmitter.n`, `main.n`, `MSBuildTask.cs`, `NemerleAll.nproj`,
`lexer1.n`. Master→retarget fix port status (checked content-wise, not by
patch-id — `git cherry` reports all "+" because fixes were absorbed in
different shape):

| master fix | in retarget? |
|---|---|
| `4dec65b86` missing compiler messages (`main.n` `bomb`/`exitValue`) | **yes** (identical code present) |
| `e402767fe` MSBuildTask message parsing | **yes** (39/41 added lines present) |
| `f5fe58328` external-parser detection (`GetCustomAttributes` moved inside `try`) | **NO** — retarget's `ncc/backend/hierarchy/LibraryReferenceManager.n` still computes `parserDefs` before the `try` → port in 9.3 |
| `f7ebf8f2c` ICE fix (`HierarchyEmitter.DefineProperty` simplification) | n/a — file deleted on retarget (dnlib emitter); 9.3 must check whether the dnlib property-emit path has the equivalent behavior |
| `c306248d2` `testsuite/positive/Bugs/lexer1.n` | **NO** — file absent on retarget → port test in 9.3 |

### Build files / misc (not sync pain, but sized)

`NemerleAll.nproj` +282/−110 (survivable delta — retarget kept master's
structure); ~60 `.nproj`/`.sln` differ, dominated by the IDE move and new
dnlib/core projects (`ncc-core.nproj`, `Nemerle.Compiler-dnlib.nproj`,
`Nemerle.Location/`); `misc/`, CI (`.github/workflows/build.yml` +131/−30)
excluded from pain accounting per instructions.

### Q1 verdict — residual sync pain after hypothetical layout alignment

After reproducing retarget's layout on master (renames + splits):

- **Path-mapping pain → 0.** All 66 renames + ~20 split parents become
  same-path; `git cherry-pick` of retarget fixes stops dying in modify/delete
  conflicts (the exact failure mode documented in `doc/MASTER-CHERRY-PICKS.md`
  R4-01).
- **~130 shared files still differ in content** (45 moved-drifted + ~19 split
  children with drift + ~66 same-path `M`, of which 51 are trivial testsuite
  markers): honest semantic residual ≈ **65 files / ~3.2k lines of
  non-whitespace drift**, and the top hot files (`Typer.n`, `MainParser.n`,
  `FixedType.n`, `ManagerClass.n`) carry the Location-refactor context
  changes **permanently** (never ported to master per MASTER-CHERRY-PICKS
  policy). Consequence: future fixes in hot files will cherry-pick with
  *context conflicts but same-path resolution* — mechanically tractable
  instead of the current manual file archaeology. Residual pain is real but
  is bounded and no longer structural.
- Master-only (`optimization/`, `HierarchyEmitter.n`, IDE) and retarget-only
  (dnlib, Location, `*Backend*.n` interfaces) files stay branch-specific —
  intentional, cost nothing after alignment.

## Q2 — FX build feasibility on master (GATE for 9.2) — **GO**

**Premise correction: VS 2017 Community 15.9 IS installed on this host**
(vswhere lists 15.9.37627.17; `HKLM\SOFTWARE\WOW6432Node\Microsoft\VisualStudio\SxS\VS7\15.0`
→ `C:\Program Files (x86)\Microsoft Visual Studio\2017\Community\` and the
path is real). MSBuild 18 (VS "18" Pro) and 17.14 also exist. master's
`DevBuildQuick-VS2017.cmd` registry probe therefore even works as-is — but the
canonical entry for sync work should be a direct MSBuild call (the `.cmd` also
runs `Reg-bins`/`vsixinstaller` GAC/IDE actions we don't want).

Toolchain assumptions verified in master's `NemerleAll.nproj`: classic MSBuild
(`UseMSBuild` probe), `GetFrameworkPath`/`GetFrameworkSdkPath` tasks,
`NRootBoot = $(NRoot)\boot-4.0\` for `/p:TargetFrameworkVersion=v4.0`,
`/tv:15.0` in the VS2017 script (master CI used the *in-box FX* msbuild
`v4.0.30319 /tv:4.0` on the old `windows-2016` image). No nuget restore, no
submodules needed for Stage1/Stage2.

### Exact working command lines (all verified 2026-10-31, worktree root)

```powershell
# Stage1 — boot-4.0 seed compiles the working tree: GREEN (~3 min)
& 'C:\Program Files (x86)\Microsoft Visual Studio\2017\Community\MSBuild\15.0\Bin\MSBuild.exe' `
  NemerleAll.nproj /t:Stage1 /p:Configuration=Debug /p:TargetFrameworkVersion=v4.0 /p:NTargetName=Build /v:m

# Stage2 — Stage1 compiler recompiles the same sources (self-compile): GREEN (~4 min)
& '...MSBuild.exe' NemerleAll.nproj /t:Stage2 /p:Configuration=Debug /p:TargetFrameworkVersion=v4.0 /p:NTargetName=Build /v:m

# CompilerTests — same entry, one session (Stage1 rebuilt incrementally):
& '...MSBuild.exe' NemerleAll.nproj /t:Stage1,CompilerTests /p:Configuration=Debug /p:TargetFrameworkVersion=v4.0 /p:NTargetName=Build /v:m
```

**Bootstrap feasibility is proven**: FX `boot-4.0` seed → Stage1 → Stage2 all
green on pristine master with **zero** host-side changes.

### CompilerTests baseline on this host (pristine master, needed as the 9.2 gate reference)

- positive: **463 passed, 4 failed** — all four fail *identically and
  deterministically* at `Process.Start` of the freshly compiled test exe with
  `Win32Exception 0x80004005 Access is denied` (`printf.n`,
  `Issue-git-0348.n`, `Issue-git-0442.n`, `Issue-git-0479.n` — all
  `//REFERENCE:`-companion tests; compilation itself succeeds; this is an
  environment/AV artifact of *this host*, not a compiler defect).
  Plus 2 environment errors: `gtk.n` (no `gtk-sharp`),
  `pointer-type-caching.n` (no `Tao.OpenGl`) — both FX-only tests that
  retarget already deleted.
- negative (run standalone from `bin\Debug\net-4.0\Tests\negative\`, the
  166-test set): **all passed**.
- The `Stage1,CompilerTests` session aborts after the positive set, so the
  negative set must be run standalone (command above in "negative" dir) or
  via `-p:` properties once the 4 host failures are allowlisted.

**Verdict: GO.** The canonical FX gate for 9.2 = Stage2 + CompilerTests
compared against the recorded baseline (463 pass/4 known-fail/2 known-err +
negative all-pass). Logs: `.tmp/phase91/q2_stage1_msbuild15.log`,
`q2_stage2_msbuild15.log`, `q2_compilertests_msbuild15.log`.

## Q3 — Strategy recommendation for 9.2

**(a) master-side mechanical re-layout (pure `git mv` + content-preserving
splits + `.nproj` glob rewrite) — RECOMMENDED.**

- The map is derivable *mechanically right now* (Appendix): 66
  `-M40%`-detected renames + ~20 split-parents + glob rewrites. Do **not**
  replay retarget's 2019 history — it moved files through dead intermediate
  directories (`ncc/backend/external/`) across 15+ interleaved
  "Refactoring"/content commits.
- `.nproj` surgery is tiny: master's `Nemerle.Compiler.nproj` has 14
  `Compile Include` entries (12 of them directory globs like `ncc\typing\*.n`);
  retarget's `Nemerle.Compiler-dnlib.nproj` collapsed the whole list to three
  recursive globs (`shared\**`, `backend\**`, `frontend\**`) — master can
  adopt the same 3+2 shape (plus keeping `ncc\optimization\*.n` and
  `ncc\generation\*.n` globs for the SRE-only files). `ncc.nproj` needs 2
  path edits.
- **Acceptance gate — one empirical caveat found:** file paths are embedded
  as strings in the compiled IL (Nemerle assert metadata,
  `ldstr "ncc\external\Codec.n"` — 444 such lines in `Nemerle.Compiler.dll`),
  so a raw `Validate`-style ildasm+`fc` will *legitimately* differ after a
  rename. Define the IL acceptance as **path-normalized, header-normalized IL
  identity**: ildasm-dump the 5 Stage2 assemblies before/after the refactor,
  replace `//`-header lines (as Validate does) **and** normalize
  `"[path]\.n"` string literals to the mapped basename, then `fc`. Any
  residual diff = refactor bug. Plus Stage1+Stage2 green and the CompilerTests
  baseline diff (Q2).

**(b) Merge/filter retarget's rename history into master — impractical,
rejected.** The renames are not isolated commits (they arrive via a 2019
`external → backend/external → backend/hierarchy` two-step across many
commits), each carrying content edits; `git merge -s`, `-X` rename heuristics
or `filter-branch` cannot reconstruct a master that must *also* keep
self-compiling at every step, and would drag dnlib/Location content in. There
is no mechanism to inject "layout only" from 319 entangled commits.

**(c) Status quo + mapping doc — insufficient as the end state.**
`MASTER-CHERRY-PICKS.md` already documents the mapping table (R4-01), and it
already proved its limit: merge-tree simulations of the three port candidates
end in modify/delete conflicts, and resolving them "keep both files" silently
breaks master's explicit-include build. Fine as a fallback if 9.2 stalls, but
every future fix pays the tax.

### Proposed commit breakdown

**9.2 (on `master`/`sync-master`, each commit independently FX-green):**
1. `[refactor] ncc: move shared compiler files to retarget's frontend/backend/shared layout (pure renames)` —
   the 66 renames from the Appendix as `git mv` (git records `R100` for the 21
   content-identical ones; the 45 are renames on master's side, content
   untouched). Acceptance: Stage1 incremental.
2. `[refactor] ncc: split monolithic tree/lexer/typed-tree files per retarget layout (content-preserving)` —
   per-parent split commits: `ParseTree.n`→`frontend/parsing/ParseTree/*`,
   `TypedTree.n`→`frontend/typing/TypedTree/*`, `Lexer.n`→`frontend/parsing/Lexer/*`,
   `external/ExternalMemberInfo/*`→`backend/hierarchy/Members/*`,
   `external/ExternalTypeInfo/*`→`backend/hierarchy/Types/*`,
   `TypeBuilder.n`/`TypeInfo.n`/`NamespaceTree.n` splits. Acceptance:
   path-normalized IL identity per commit.
3. `[build] Nemerle.Compiler.nproj/ncc.nproj: retarget-style recursive globs` —
   the `.nproj` include rewrite. Acceptance: Stage1+Stage2+Validate + the full
   IL-identity diff vs pre-refactor baseline.
4. `[docs] AGENTS.md/ncc/README: post-alignment layout note` (optional).

**9.3 (fix porting, now mechanical):** retarget→master: the
`MASTER-CHERRY-PICKS.md` candidates (`e9741864a`, `5a7dadbf1`, `f35161ae9`)
cherry-pick with same paths; master→retarget: `f5fe58328` LRM parser-defs fix
(port the `try` move), `c306248d2` `lexer1.n` test, review `f7ebf8f2c`
against dnlib property emit. FX gate per cherry-pick: Stage1, then Stage2 +
CompilerTests (vs Q2 baseline) before merge.

**Canonical FX build entry for the sync work** (master has no `build.ps1`; do
not add tooling in 9.2, just document + optionally a thin `build-fx.ps1` in
9.4): the MSBuild-15 command lines in Q2 above, run from the repo root;
`vswhere -latest -products * -requires Microsoft.Component.MSBuild
-property installationPath` remains the discovery recipe when VS2017 is
absent (MSBuild ≥ 15 Full works; `dotnet msbuild` does not — same MSB4803
class of failure as retarget's `InitTools` note).

## Appendix — master → retarget layout map (verified 2026-10-31)

Renames (git `-M40%`, tip-to-tip). 21 R100 entries marked `*`:

```
ncc/CompilationOptions.n -> ncc/frontend/CompilationOptions.n
ncc/passes.n -> ncc/frontend/ManagerClass.n
ncc/completion/CodeCompletionEngine.n -> ncc/frontend/Changes/Completion.n
ncc/codedom/*.n,*.txt,test.xml (6 files) -> ncc/frontend/codedom/*
ncc/hierarchy/DelegateClassGen.n,GlobalEnv.n,MacroClassGen.n,ScanTypeHierarchy.n,XmlDump.n -> ncc/frontend/hierarchy/*
ncc/hierarchy/BuiltinMethod.n -> ncc/frontend/hierarchy/Members/BuiltinMethod.n
ncc/hierarchy/TypesManager.n -> ncc/frontend/hierarchy/Types/TypesManager.n
ncc/misc/ComponentsFactory.n,ExprWalker.n,PrettyPrint.n,Stats.n,TopDeclPrint.n -> ncc/frontend/misc/*
ncc/parsing/MainParser*.n,Parser.n,PreParser*.n,PreprocessorEvaluator.n,Utility.n -> ncc/frontend/parsing/*
ncc/typing/ConstantFolder.n,DecisionTreeBuilder.n,ErrorMessage.n,FixedType.n,LocalContext.n,LocalValue.n,MacroColorizer.n,Macros.n,Messenger.n,OverloadPossibility.n,README,RestartInErrorMode.n,Solver.n,StaticTypeVar.n,Subst.n,TraverseExpr2.n,TypeVarEnv.n -> ncc/frontend/typing/*
ncc/typing/TypeVar.n -> ncc/frontend/typing/TypedTree/TypeVar.n
ncc/typing/Typer*.n (6 files) -> ncc/frontend/typing/Typer/*
ncc/generation/Typer3.n,Typer4.n -> ncc/frontend/typing/Typer/*
ncc/generation/DecisionTreeCompiler.n,ILEmitter.n,ImplementsWrapperMaker.n -> ncc/backend/generation/*
ncc/external/Codec.n,ExternalType.n,LibraryReferenceManager.n,NumericKind.n -> ncc/backend/hierarchy/*
ncc/external/InternalTypes.n -> ncc/backend/hierarchy/InternalTypeClass.n   # the R4-01 case
ncc/misc/AssemblyInfo.n -> ncc/shared/AssemblyInfo.n                        *
ncc/Makefile -> ncc/frontend/Makefile
testsuite/negative/yield3.n -> testsuite/negative/disabled/yield3.n
testsuite/positive/Issue-git-0297.cs -> testsuite/positive/Issue-git-0297-cs.cs
```

(+21 byte-identical R100 moves within the above groups; full per-file TSV in
`.tmp/phase91/layout_map.tsv`.)

Split parents (content-preserving split in 9.2 step 2):

```
ncc/parsing/ParseTree.n     -> ncc/frontend/parsing/ParseTree/ (22 files)
ncc/typing/TypedTree.n      -> ncc/frontend/typing/TypedTree/ (14 files)
ncc/parsing/Lexer.n         -> ncc/frontend/parsing/Lexer/ (6 files)
ncc/parsing/Source.n        -> absorbed by Nemerle.Location/ (master: keep as ncc/frontend/parsing/Source.n or split by hand)
ncc/hierarchy/ClassMembers.n-> members split under frontend/backend hierarchy (manual)
ncc/external/ExternalMemberInfo/* (5) -> ncc/backend/hierarchy/Members/*
ncc/external/ExternalTypeInfo/* (3)    -> ncc/backend/hierarchy/Types/*
ncc/hierarchy/TypeBuilder.n -> frontend/hierarchy/Types/TypeBuilder.n + backend/generation/TypeBuilder.n
ncc/hierarchy/TypeInfo.n    -> frontend/hierarchy/Types/TypeInfo.n + backend/hierarchy/Types/TypeInfo.n
ncc/hierarchy/NamespaceTree.n -> ncc/frontend/hierarchy/NamespaceTree*.n (+ backend Node.Hacks)
ncc/hierarchy/CustomAttribute.n -> keep master copy; retarget moved it out of ncc
ncc/parsing/AST.n           -> ParseTree split (retarget removed it from ncc)
ncc/typing/MacroRegistry.n  -> ncc/frontend/typing/MacroRegistry.n
ncc/external/LibraryReference.n -> ncc/backend/hierarchy/LibraryReference.n
```

Stay on master only (SRE line; keep under their current or nearest aligned dir):
`ncc/optimization/*` (8), `ncc/generation/HierarchyEmitter.n`, `ncc32/64`,
`ncc/main.n` (same path both sides already), IDE (`VsIntegration/`).
