# 9.2 — `sync-master`: align `master` source layout to `retarget-compiler` (mechanical, content-preserving)

Executor progress report. Branch: `sync-master` @ `b5f704623` in worktree
`C:\RSDN\nemerle-master`. Reference: `retarget-compiler` (main checkout).
Spec authority: `retarget-compiler-to-net10-plan-report9.1-convergence.md`
(rename-map appendix + proposed commit breakdown).

## Status: COMPLETE

- [x] Research/plan (this file, "Plan" section below)
- [x] G0 baseline (pristine Stage1/Stage2 re-confirm + IL dumps + CompilerTests reuse)
- [x] Commit A — pure renames (66-entry map + split-parent/branch-file moves) + nproj path rewrite — `56788fbeb`
- [x] Commit B1 — `ParseTree.n` + `AST.n` splits — `5bc802816`
- [x] Commit B2 — `Lexer.n` + `TypedTree.n` splits — `82c2b850a`
- [x] Commit B3 — `ClassMembers.n` + `TypeInfo.n` + `BuiltinMethod.n` splits — `7202c99b3`
- [x] Commit C — nproj finalization (dead globs, Folder items, comments) — `050868325`
- [x] G2 Stage2 green
- [x] G3 IL identity vs baseline (path-normalized, header-normalized)
- [x] G4 CompilerTests vs 9.1 baseline
- [x] G5 diff summary + spot checks

## Plan (decisions made from the 9.1 report + on-code verification)

### Empirically established constraints

1. **Emit order = compile order.** Verified with the pristine Stage2 `ncc` on a
   two-file test: class metadata order in IL follows the `Compile` item order
   exactly (for independent classes; related classes are base-first sorted with
   compile order as tie-break). Therefore the refactor MUST preserve the exact
   baseline `Compile` sequence, and the report's `shared/**,backend/**,frontend/**`
   3-glob shape (commit "C" idea) would permute types and blow up G3.
2. **`/line`-less ildasm dumps** (Validate-style `ildasm <asm> /out=x.il /nobar`,
   cwd = stage dir) embed source paths only as `ldstr` string literals
   (assert metadata) — no `.line` directives. Those are the expected-different
   strings the report quantifies (444 in Nemerle.Compiler.dll).

### Deviations from the report (recorded up front)

- **D1 — nproj glob shape.** `Nemerle.Compiler.nproj` keeps an **ordered list**
  of globs+explicit items reproducing the baseline compile sequence 1:1
  (group-wise 1:1 where order+set are preserved by one glob), instead of the
  3-recursive-glob retarget shape. Rationale: constraint (1); G3 near-empty
  diff is the core proof and the report itself makes IL identity the gate.
  Path-alignment (the actual 9.2 goal) is unaffected.
- **D2 — split granularity for nested/renamed retarget children.** Retarget
  un-nested and renamed some classes while splitting (`Typarms`→`TypePparms`
  +alias, `ParmKind`→`ParamKind`, `MatchCase`→`PMatchCase`, `SyntaxElement`
  moved out of nesting, `LexerBase`→`Lexer`). Master split keeps **master's
  names/nesting byte-for-byte**, only moving them into the retarget-named
  child files. Where a master class spans two retarget children (e.g.
  `LexerBase` family vs `Lexer.n`), master content goes to the child that
  carries the class in retarget; the dnlib-only sibling children are
  retarget-only (A-bucket) and not created.
- **D3 — no-split single-file "splits".** `TypeBuilder.n` (backend twin is pure
  dnlib content), `NamespaceTree.n` (nested `Node`/`TypeInfoCache` cannot be
  split without adding `partial` = content change), `TypeInfo.n`'s class
  (backend twin is dnlib-only): each moves as a whole to the retarget
  frontend-side path.
- **D4 — branch-specific files.** `ncc/optimization/*` and `ncc/main.n`,
  `ncc/g`, `ncc/README`, `ncc/ncc.build`, `ncc/policy...template` stay put
  (same paths on both branches where they exist). `ncc/generation/HierarchyEmitter.n`
  and `ncc/hierarchy/CustomAttribute.n` stay in place as master-only files
  (retarget deleted/relocated them; keeping them out of aligned dirs avoids
  implying a counterpart). The root `Makefile` is not updated — retarget's own
  `Makefile` is equally stale (`ncc/misc/AssemblyInfo.n`), matching behavior.
- **D5 — testsuite map moves change test-set counts by design.**
  `yield3.n` → `negative/disabled/` removes it from the non-recursive negative
  glob (166 → 165 expected on the standalone negative run);
  `Issue-git-0297.cs` → `Issue-git-0297-cs.cs` may shift the positive count
  depending on harness same-basename pairing (measured at G4).

### Split map (class → new file), all content-preserving

`ncc/parsing/AST.n` (namespace Nemerle.Compiler) splits into:
`ParseTree/Location/TextPoint.n` (TextPoint), `ParseTree/ParsedBase.n`,
`frontend/typing/TypedTree/TypedBase.n`, `ParseTree/NemerleModifiers.n`
(NemerleAttributes+NemerleModifiers), `ParseTree/AttributesAndModifiers.n`
(Modifiers alias+AttributesAndModifiers), `ParseTree/Literal.n`,
`ParseTree/FunBody.n`; **remainder stays `frontend/parsing/AST.n`**
(Location+Located — retarget moved these to `Nemerle.Location/`, which master
does not have; branch-specific residual, like HierarchyEmitter).

`ncc/parsing/ParseTree.n` (namespace Nemerle.Compiler.Parsetree) → 19 files in
`frontend/parsing/ParseTree/`: TopDeclaration, ClassMember, MemberBase,
DeclarationBase, Constraint, TypePparms(Typarms), SubstitutingTypeParams,
CopyTypedTypeParams, Name, Namespace, PExpr, Splicable, PParameter,
IParametersProvider, PFunHeader, FunctionDecl(Function_decl),
PMatchCase(MatchCase), TryCase, SyntaxElement.

`ncc/parsing/Lexer.n` → `frontend/parsing/Lexer/`: Token.n, Region.n,
Comment.n, Directive.n, Lexer.n (LexerBase+LexerStringBase+LexerFile+LexerString),
LexerCompletion.n.

`ncc/typing/TypedTree.n` → `frontend/typing/TypedTree/`: TypeDeclaration,
TParameter, FunctionUsage, TFunHeader, Pattern, TryCase(Try_case),
TMatchCase(Match_case), ConversionKind, TExprFlags, CacheDesc, TExpr,
ParamKind(ParmKind), Param(Parm). (TypeVar.n, TypedBase.n land there too.)

`ncc/hierarchy/ClassMembers.n` → `frontend/hierarchy/`: RefTo.n,
RestartRunBodyTyperException.n + `Members/`: MemberBuilder, FieldBuilder,
PropertyBuilder, MethodBuilder(+nested BeforeBodyTypingHandler), EventBuilder.

`ncc/hierarchy/TypeInfo.n` → `frontend/hierarchy/Accessibility.n` +
`Members/`: BindingFlags, MemberKinds, BuiltinMethodKind, IMember, IField,
IEvent, IMethod, IProperty, MemberInfo; class TypeInfo stays whole in
`Types/TypeInfo.n`.

`ncc/hierarchy/BuiltinMethod.n` → `Members/BuiltinWrapMethod.n` +
`Members/BuiltinMethod.n` (order preserved: Wrap then Base).

`ncc/external/ExternalMemberInfo/*` → `backend/hierarchy/Members/*`;
`ncc/external/ExternalTypeInfo/*` → `backend/hierarchy/Types/*`;
`ncc/external/LibraryReference.n` → `backend/hierarchy/` (pure moves).

### Compile-order contract (final nproj sequence)

Baseline expansion order (captured from nproj globs + confirmed against the
first-appearance path order in the baseline IL dump): CompilationOptions,
passes, parsing/*(AST,Lexer,MainParser-Ext,MainParser,Parser,ParseTree,PreParser,
PreParserIndent,PreprocessorEvaluator,Source,Utility), codedom/*,
completion/*, external/*, EMI/*, ETI/*, generation/*, hierarchy/*, misc/*,
optimization/*, typing/*. The new item list maps each position to the new
path(s) of the same content, with each split parent's children occupying the
parent's slot in parent-declaration order (so `TypedBase.n` is compiled in
AST.n's slot even though it lives under `frontend/typing/TypedTree/`, and
`Typer3/4.n` in generation slots, `TypeVar.n`/`TypeVarEnv.n` in their
baseline typing positions).

### Gates

- G1: Stage1 green after each commit (MSBuild 15 entry from the report).
- G2: Stage2 green at the end.
- G3: Validate-style dumps of Nemerle.dll, Nemerle.Compiler.dll,
  Nemerle.Macros.dll, ncc.exe (+Nemerle.MSBuild.Tasks.dll) before/after;
  normalize: banner lines, `.mvid`, assembly `.ver` if build-stamped, and
  **any `ldstr` path string ending in `.n` → `SOURCE.n`** (stronger than the
  report's basename normalization — needed because split parents' basenames
  change too). Residual diff must be empty or individually explainable.
- G4: `Stage1,CompilerTests` + standalone negative set vs the 9.1 baseline
  (positives 463 pass/4 host-AV fails/2 env errors; negatives all pass) —
  modulo the two recorded D5 test-layout changes.
- G5: `git log --oneline master..sync-master`, `git diff --stat master...sync-master`,
  rename-detection proof (`git show --stat -M` R100 for commit A), 5-file
  byte-equality spot checks.

## Results

### G0 baseline (complete)

- Re-confirmed pristine builds from `sync-master` @ b5f704623:
  `g0_stage1.log` EXIT=0, `g0_stage2.log` EXIT=0 (no-op incremental over the
  9.1 outputs; Stage2 assemblies unchanged, timestamps 10:43).
- Validate-style IL dumps (cwd = Stage2 dir, `/out=x.il /nobar`) of
  Nemerle.dll, Nemerle.Compiler.dll, Nemerle.Macros.dll, ncc.exe,
  Nemerle.MSBuild.Tasks.dll in `.tmp\phase92\baseline-il\` — taken before any
  edit.
- Empirics that shaped the plan: (1) compile order determines metadata
  ordering (two-file probe with the pristine Stage2 ncc: AB.dll vs BA.dll
  class order follows the argument list; a dependency probe shows base-first
  hoisting with compile order as tie-break); (2) IL path strings are relative
  (`"ncc\\parsing\\AST.n"`, 648 hits in Nemerle.Compiler.dll), MVID/Image-base
  are comment lines; assembly version revision is build-stamped (575→576
  between builds), hence the normalization set in `normdiff.ps1`.

### Commit A — `56788fbeb` renames (complete, all gates green)

- 85 `git mv` renames (66-entry map + whole-file split-parent/child moves);
  `git show --stat -M` records **R100 for all 85, 0 insertions/deletions**.
- `Nemerle.Compiler.nproj`: ordered item list reproducing the old glob
  expansion item-for-item (verified: 89 old vs 89 new items, basename diff =
  exactly the 3 map renames CodeCompletionEngine→Completion,
  InternalTypes→InternalTypeClass, passes→ManagerClass; no duplicates).
  `ncc.nproj`: AssemblyInfo path update.
- G1: Stage1 EXIT=0 (`g1_commitA_stage1.log`); Stage2 EXIT=0
  (`g2_commitA_stage2.log`).
- G3 (early, per-commit): normalized IL diff **IDENTICAL** for all four
  assemblies (normalized sizes byte-equal, `git diff` reports IDENTICAL).
  Normalization classes: banner/MVID/Image-base/ildasm-.res-comment drops,
  `.ver 1:2:0:<build-stamp>` + AssemblyFileVersion blob tokens, and
  source-path IL strings (merged across ildasm `ldstr` + `+ "..."`
  continuations) → `"<SRC>"`.

### Commit B1 — `5bc802816` AST.n + ParseTree.n split (complete, all gates green)

- `ncc/parsing/AST.n` → `frontend/parsing/ParseTree/*` (TextPoint, ParsedBase,
  TypedTree/TypedBase, NemerleModifiers, AttributesAndModifiers, Literal,
  FunBody) + remainder stays `frontend/parsing/AST.n` (Location+Located, D4);
  `ncc/parsing/ParseTree.n` → 19 files in `frontend/parsing/ParseTree/`.
  Splitter asserts byte-exact reconstruction (header+regions+trailer == parent)
  per split.
- G1: Stage1 EXIT=0; Stage2 EXIT=0 (`g1_commitB1_stage1.log`,
  `g2_commitB1_stage2.log`).
- G3 (per-commit): normalized IL diff = **24 lines only**, all `ldc.i4`
  assert-line constants of the split classes; other three assemblies
  IDENTICAL.

### Commit B2 — `82c2b850a` Lexer.n + TypedTree.n split (complete, all gates green)

- `ncc/frontend/parsing/Lexer.n` → `Lexer/{Token,Region,Comment,Directive,
  Lexer,LexerCompletion}.n`; `ncc/frontend/typing/TypedTree.n` → 13 children
  in `typing/TypedTree/` (TypeDeclaration … Param). Compile in the parents'
  former nproj slots.
- G1: Stage1 EXIT=0; Stage2 EXIT=0 (`g1_commitB2_stage1.log`,
  `g2_commitB2_stage2.log`).
- G3 (per-commit): Nemerle.dll / Nemerle.Macros.dll / ncc.exe IDENTICAL;
  Nemerle.Compiler.dll 451 residual lines — mechanical verification
  (`verify_asserts.ps1`): **all residual lines are `ldc.i4` constants or
  their encoding-width/offset ripple, inside 33 assert-bearing methods** of
  the split classes; aggressive-mode normdiff (masks `ldc.i4`/`IL_x`/Code
  size) IDENTICAL.

### Commit B3 — `7202c99b3` hierarchy splits (complete, all gates green)

- `Members/BuiltinMethod.n` → `BuiltinWrapMethod.n` + `BuiltinMethod.n`;
  `Members/ClassMembers.n` → `frontend/hierarchy/{RefTo,
  RestartRunBodyTyperException}.n` + `Members/{MemberBuilder,FieldBuilder,
  PropertyBuilder,MethodBuilder,EventBuilder}.n`; `Types/TypeInfo.n` →
  `frontend/hierarchy/Accessibility.n` + `Members/{BindingFlags,MemberKinds,
  BuiltinMethodKind,IMember,IField,IEvent,IMethod,IProperty,MemberInfo}.n`,
  class TypeInfo remains in `Types/TypeInfo.n`. File boundaries now match
  retarget. (Deviation note: `TypeInfo.n` is CRLF while the other split
  parents are LF; splitter preserves EOLs per file, BOM preserved too.)
- G1: Stage1 EXIT=0; Stage2 EXIT=0 (`g1_commitB3_stage1.log`,
  `g2_commitB3_stage2.log`).
- G3 (per-commit): three assemblies IDENTICAL; Nemerle.Compiler.dll 600
  residual lines, same verified classes (55 assert-bearing methods);
  aggressive-mode normdiff IDENTICAL.

### Commit C — `050868325` nproj finalization (complete)

- Dropped the dead `ncc\CompilerMessage\*.n` glob (directory absent since
  long ago; glob expanded to zero items — the after-C IL dump is byte-identical
  to after-B3, git blob hash `4dda0e7` on both sides of the normalized diff).
- Refreshed `<Folder Include>` items to the aligned tree (IDE-noise only).
- Kept the ordered Compile list + the order-warning comment (D1).
- G1: Stage1 EXIT=0 (`g1_commitC_stage1.log`).

### Final gates

- **G2**: Stage2 EXIT=0 after the last commit (`g2_final_stage2.log`).
- **G3 final** (`afterC-il` vs `baseline-il`, Stage2↔Stage2):
  Nemerle.dll, Nemerle.Macros.dll, ncc.exe normalized-**IDENTICAL**;
  Nemerle.Compiler.dll 600 residual lines = assert-line `ldc.i4` constants +
  encoding-width/offset ripple, all 55 enclosing methods assert-bearing
  (`verifyC.out`: "ALL RESIDUAL HUNKS ARE ldc.i4 IN ASSERT-BEARING METHODS");
  aggressive-mode normdiff IDENTICAL (`aggC.out`). No unexplainable
  difference exists.
- **G4** (`/t:Stage1,CompilerTests`, `g4_compilertests.log`): positives **464
  passed, 3 failed** + 2 env errors. All non-passes are exactly the
  documented baseline set: host-AV `Win32Exception 0x80004005` at
  `printf.n`, `Issue-git-0348.n`, `Issue-git-0479.n` (`Issue-git-0442.n`,
  the fourth flaky AV site, *passed* this run → 463+1=464; pass+fail total
  identical to baseline: 467), and env errors `gtk.n` (gtk-sharp),
  `pointer-type-caching.n` (Tao.OpenGl). **No new failures.**
  Renamed `Issue-git-0297-cs.cs` passed. Standalone negatives
  (`g4_negative.log`): **All tests passed** — 165/0 (166→165 as predicted by
  D5: `yield3.n` now in `negative/disabled/`).
- **G5**: `git log --oneline master..sync-master` = the five commits above.
  `git diff --stat -M master...sync-master`: 149 files, R 82 / A 62 / D 3 /
  M 2 — the 3 D are the fully-consumed split parents (`ClassMembers.n`,
  `AST.n`, `ParseTree.n`), the 2 M are exactly `Nemerle.Compiler.nproj` and
  `ncc.nproj`. 5-file byte-equality spot check (`spotcheck.ps1`, Latin1
  byte-exact substring vs the b5f704623 parent blobs): TopDeclaration.n,
  LexerCompletion.n, TExpr.n, MethodBuilder.n, Accessibility.n — every
  child's member region embedded verbatim in its parent, headers prefix-equal.
  **All 5 PASS.**

### Deviations (final list)

D1–D5 as planned (see "Plan"). Two execution notes: (a) commit A moved 85
(not 66+few) renames because split parents + single-file D3 moves are also
pure renames; (b) `Issue-git-0442.n`'s flaky host-AV did not reproduce at
G4, so the observed positives are 464/3 instead of 463/4 — same total,
subset of the baseline failure set (this is environment flakiness at
`Process.Start`, not a code difference; the 9.1 report classifies it as a
host artifact).

### Baseline artifacts

- 9.1 pristine builds (same commit, same host, logs
  `C:\RSDN\nemerle\.tmp\phase91\q2_stage1_msbuild15.log`,
  `q2_stage2_msbuild15.log`, `q2_compilertests_msbuild15.log`):
  Stage1 GREEN, Stage2 GREEN, CompilerTests: positives 463 pass + 4
  deterministic host-AV fails (`printf.n`, `Issue-git-0348.n`,
  `Issue-git-0442.n`, `Issue-git-0479.n`) + 2 env errors (`gtk.n`,
  `pointer-type-caching.n`); negatives 166/0. Reused as the G4 baseline per
  the task's allowance.
- Validate-style IL dumps of the pristine Stage2 outputs stored under
  `C:\RSDN\nemerle\.tmp\phase92\baseline-il\` (taken **before** any edit).
