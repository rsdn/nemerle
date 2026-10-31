# 9.3 — `sync-master`: port the shared language fixes from `retarget-compiler`

Executor progress report. Branch: `sync-master` in worktree
`C:\RSDN\nemerle-master` (starting HEAD `050868325`, the 9.2 layout-alignment
tip). Reference: `retarget-compiler` (main checkout, `C:\RSDN\nemerle`).
Spec authority: `doc/MASTER-CHERRY-PICKS.md` (per-candidate port procedures) +
`...-progress9.2.md` (layout now aligned) +
`...-report9.1-convergence.md`.

## Status: COMPLETE

- [✅] Plan (this file, "Plan" section below)
- [✅] P1 — `e9741864a` InitSystemTypes Void/Object/ValueType reorder — `bd55c53da` (ported-manual)
- [✅] P2 — `5a7dadbf1`(a) Delegate Combine/Remove reorder — `a49006ff0` (ported-manual, (b) skipped)
- [✅] P3 — `171f1f227` FNV determinism — `c1e632084` (ported-cherry, code file only)
- [✅] P4 — `f35161ae9` keyed-IVT fix — `dbe955e91` (ported-manual, StrongNameKeyPair)
- [✅] Evaluate remaining candidates — dispositions recorded below
- [✅] G1 Stage1 green after EACH commit (logs in `.tmp\phase93\`)
- [✅] G2 Stage2 green at the end
- [✅] G3 FX CompilerTests vs 9.1/9.2 baseline — no new failures + per-candidate evidence
- [✅] G4 `git log --oneline master..sync-master`, per-port `git diff --stat`, provenance notes
- [✅] Reverse-direction record: `f5fe58328` + `testsuite/positive/Bugs/lexer1.n` (master-only)

## Plan

### Re-adjudication of the "DO NOT raw cherry-pick" warning

The MASTER-CHERRY-PICKS.md warning (R4-01) predates 9.2: it assumed master's
monolithic layout made every cherry-pick a modify/delete silent no-op. After
9.2 the three target files exist at the retarget paths
(`ncc/backend/hierarchy/InternalTypeClass.n`,
`ncc/backend/hierarchy/LibraryReference.n`,
`ncc/frontend/hierarchy/MacroClassGen.n`), so the silent-no-op trap is gone.
However the 9.2 port was content-preserving (master's content in retarget's
layout), and retarget-side content drifted, so CONTEXT conflicts remain:

- `InternalTypeClass.n` (master): no `Pointer_tc`/`CacheTypeInfo` preamble in
  `InitSystemTypes` (retarget-only feature); casts spelled
  `:> LibraryReference.ExternalTypeInfo` (retarget: `:> ExternalTypeInfo`);
  attribute block ends at `AssemblyProductAttribute_tc` (no
  AssemblyConfiguration/Security/Debuggable/RuntimeHelpers/MethodBase/
  Runtime*Handle lookups — those lookups are retarget-line additions).
- `LibraryReference.n` (master): `GetIsFriend` is bool-style (`checkFriend : bool`
  returning `true`/`false`; retarget uses a void loop assigning `IsFriend = true`);
  dead-branch guard formatted across two lines.
- `MacroClassGen.n` (master): FNV hunk context byte-identical to retarget's —
  verified, the hunk applies cleanly.

Dispositions: P1/P2/P4 = manual ports (hunk content, master's formatting);
P3 = apply the single-file patch (equivalent to the rename-detected
cherry-pick) with the progress-doc addition EXCLUDED (retarget-plan docs do
not belong on `sync-master`); commit message carries the provenance note.

### Port order (per MASTER-CHERRY-PICKS.md "Port order recommendation")

1. P1 `e9741864a` → P2 `5a7dadbf1`(a) → P3 `171f1f227` (init-order + determinism chain)
2. P4 `f35161ae9` (behavior fix, labeled as such in the commit message)

### Per-candidate procedures

- **P1** — `ncc/backend/hierarchy/InternalTypeClass.n`: insert the
  Void/Object/ValueType lookup trio (verbatim lines + explanatory comment from
  the retarget diff) at the top of `InitSystemTypes` (master has no Pointer
  preamble, so insertion point is right after `{`); delete the trio from its
  old positions (Object/Void inside the "and here not" run, ValueType after
  `UInt64_tc`); the `(Object_tc :> LibraryReference.ExternalTypeInfo).AddBuiltins ();`
  call site stays put.
- **P2** — same file: move `def is_right`/`def single` + `Delegate_Combine`/
  `Delegate_Remove` assignments (master's bodies verbatim) to immediately after
  `ParamArrayAttribute_tc`, ahead of the `FlagsAttribute_tc`..
  `AssemblyProductAttribute_tc` attribute block, carrying the retarget
  explanatory comment; `String_Concat` stays after the attribute block.
  Hunk (b) (PdbType host gate in `CompilationOptions.n`) SKIPPED — inert on FX
  (FX default already `Windows`).
- **P3** — `ncc/frontend/hierarchy/MacroClassGen.n`: apply the
  `171f1f227` code hunk verbatim (`fnv1a32` helper + call-site swap).
- **P4** — `ncc/backend/hierarchy/LibraryReference.n`: `snKey()` →
  `publicKey() : array[byte]` implemented with
  `SR.StrongNameKeyPair(File.Open(...)).PublicKey` behind an
  `IsNullOrEmpty(StrongAssemblyKeyName)` null-return + try/catch→null (master
  has no dnlib; doc prescribes StrongNameKeyPair); `SHA1Managed()` →
  `SHA1.Create()`; dead branch repair in `checkFriend`: guard
  `string.IsNullOrEmpty(StrongAssemblyKeyName)` dropped, inner split on `'='`,
  single `publicKey()` read hoisted to `def pubKey` with `null` ⇒ `false`
  (master's bool-style arms preserved). Commit message labels the behavior
  change (keyed IVT becomes honored on FX).

### Remaining candidates — disposition plan (evaluate, do not port blind)

- `1231f73af` (a) fixture rename: ALREADY LANDED via 9.2 commit A
  (`Issue-git-0297-cs.cs` present on `sync-master`) — verified.
  (b) `BEGIN-OUTPUT-CORE` blocks: SKIP — FX harness ignores the markers; doc:
  "pointless on master alone; needed only if the test files are ever unified".
- `9ecc5c8a2`: SKIP per doc ("no") — FX hosts ignore `// NO-CORE`; harness
  plumbing is net10 work.
- `544a17cdf`, `da2b5cdee`, `8561fda03`, `252bc791d`, `2e031d283`: NO-PORT per
  doc (host-gated Core branches, FX byte-identical).

### Reverse direction (record-only, per 9.1)

`f5fe58328` (NemerleAll.nproj + external-parser loading detection fix) and
`testsuite/positive/Bugs/lexer1.n` (verified present on `sync-master`, absent
in retarget) are master-only — future retarget-side atom; NOT ported here.

### Gates

- **G1**: Stage1 green after each commit — FX entry
  `& 'C:\Program Files (x86)\Microsoft Visual Studio\2017\Community\MSBuild\15.0\Bin\MSBuild.exe' NemerleAll.nproj /t:Stage1 /p:Configuration=Debug /p:TargetFrameworkVersion=v4.0 /p:NTargetName=Build /v:m`;
  logs `.tmp\phase93\g1_p<n>_stage1.log`.
- **G2**: Stage2 (analogous `/t:Stage2`) after P4.
- **G3**: `Stage1,CompilerTests` FX run vs the 9.1/9.2 baseline (positives
  463–464 pass + documented host-AV set {printf.n, Issue-git-0348.n,
  Issue-git-0442.n(flaky), Issue-git-0479.n} + 2 env errors {gtk.n,
  pointer-type-caching.n}; negatives standalone 165/0) — NO new failures.
  Candidate evidence: P1/P2 — grep Stage2 source/IL that the reordering landed;
  P3 — baseline captured pre-port from 9.2 Stage2 `Nemerle.Macros.dll`
  (`_N_operator67864986_4667Macro` etc., 25 classes,
  `.tmp\phase93\baseline_stage2_Nemerle.Macros.il`); post-port spot-check that
  the digits changed to FNV values + suite green; P4 — scratch keyed-IVT pair
  under `.tmp\phase93`: matching-key friend COMPILES (pre-fix ncc fails the
  same sources), wrong-key/unkeyed cases still fail.
- **G4**: `git log --oneline master..sync-master` (5 + 4 new), per-port
  `git diff --stat` (minimal hunks), provenance notes (`cherry-picked-from` /
  manual-port ref) in commit messages.

## Results

### Disposition table

| candidate | disposition | commit | notes |
|---|---|---|---|
| `e9741864a` | **ported-manual** | `bd55c53da` | trio + comment verbatim; landed at top of master's `InitSystemTypes` (no Pointer preamble there); `(Object_tc :> LibraryReference.ExternalTypeInfo).AddBuiltins ()` call site untouched |
| `5a7dadbf1`(a) | **ported-manual (partial)** | `a49006ff0` | `is_right`/`single` + `Delegate_Combine`/`Remove` moved ahead of the attribute block (master's 10-attribute block; retarget's extra retarget-only lookups NOT added); (b) PdbType hunk SKIPPED — FX-dead per doc |
| `171f1f227` | **ported-cherry** (code-only) | `c1e632084` | hunk applies context-clean; fnv1a32 block verified byte-identical to the retarget post-image (`fnv_block_check.ps1`); commit's progress-doc addition intentionally excluded from `sync-master` |
| `f35161ae9` | **ported-manual** | `dbe955e91` | guard dropped, split `'='`, `SHA1.Create()`; `publicKey() : array[byte]` re-implemented with `SR.StrongNameKeyPair` (master has no dnlib); master's bool-style `checkFriend` arms preserved; commit message labeled BEHAVIOR FIX |
| `1231f73af`(a) | **already landed** | (via 9.2) | `Issue-git-0297-cs.cs` present on `sync-master` — rename arrived with the 9.2 layout map |
| `1231f73af`(b) | **skipped** | — | `BEGIN-OUTPUT-CORE` blocks: FX harness ignores markers; doc: "pointless on master alone" |
| `9ecc5c8a2` | **skipped** | — | per doc ("no"); `// NO-CORE` inert on master's harness, plumbing is net10 work |
| `544a17cdf`, `da2b5cdee`, `8561fda03`, `252bc791d`, `2e031d283` | **skipped** | — | NO-PORT per doc (host-gated Core branches, FX byte-identical) |

Deviations: (D9.3-1) P3's hunk was hand-applied (the `git show` pipe emits
CRLF vs master's LF file, so `git apply` refused; the hunk was applied with
the edit tool and verified byte-identical to the retarget post-image instead)
+ the cherry-pick's progress-doc file excluded. (D9.3-2) P4's first take used
an unbraced `try` body — parse error "expecting `catch' or `finally'" at the
G1 build; fixed with braces (matching retarget's braced form) and amended
into the P4 commit before any other work; both the failed attempt and the
green `g1_p4_stage1.log` are recorded.

### G1 — Stage1 after each commit (FX, MSBuild 15)

`g1_p1_stage1.log` / `g1_p2_stage1.log` / `g1_p3_stage1.log` /
`g1_p4_stage1.log`: **EXIT=0 all four**
(tail: `ncc -> C:\RSDN\nemerle-master\bin\Debug\net-4.0\Stage1\ncc.exe` etc.).

### G2 — Stage2

`g2_final_stage2.log`: **EXIT=0** (Stage2 compiled by the Stage1 output of the
fully ported tree).

### G3 — FX CompilerTests (`g3_compilertests.log`, `g3_negative.log`)

- Positives: **463 passed, 4 failed** + 2 env errors — the four failures are
  exactly the documented host-AV set (`printf.n`, `Issue-git-0348.n`,
  `Issue-git-0442.n`, `Issue-git-0479.n`, all
  `System.ComponentModel.Win32Exception (0x80004005)` at test-binary launch),
  the env errors are `gtk.n` (gtk-sharp) and `pointer-type-caching.n`
  (Tao.OpenGl). Identical to the 9.1 baseline (9.2 observed 464+3 only
  because the flaky `Issue-git-0442.n` passed that run; totals 467 both ways).
  **No new failures.**
- Negatives standalone: **All tests passed — 166/0**. One more line than the
  9.2 standalone run's 165 because that run's file list omitted the single
  `.nnn` negative test; `Issue-git-0561.nnn` (the only `.nnn` in
  `testsuite/negative/`) is the extra line and it **passed** — coverage is a
  superset of the 9.2 run with zero failures.
- Note: `/t:Stage1,CompilerTests` stops after the positive set because the
  host-AV failures make the harness exit non-zero (MSB3073), so the negative
  set is run standalone with the target's own command line (same as 9.2).

### G3 — per-candidate evidence

- **P1 (reorder in Stage2 IL)**: `post_stage2_Nemerle.Compiler.il`,
  `InitSystemTypes()` at IL_0011 `"System.Void"` → IL_002f `"System.Object"`
  → IL_0058 `"System.ValueType"` → IL_0081 `"System.Boolean"` (trio first).
- **P2 (reorder in Stage2 IL)**: same method: IL_0615
  `"System.ParamArrayAttribute"` → IL_063a `"Combine"` → IL_0653 `"Remove"` →
  IL_0665 `"System.FlagsAttribute"` → IL_0707
  `"System.Reflection.AssemblyProductAttribute"` → IL_0720 `"Concat"` —
  Delegate lookups now precede the attribute block.
- **P3 (hash digits changed)**: baseline (pre-port 9.2 Stage2
  `Nemerle.Macros.dll`) 25 `_N_operator<HASH>_<n>Macro` classes, e.g.
  `_N_operator67864986_4667Macro`, `_N_operator1974104882_4848Macro`;
  post-port Stage2: `_N_operator2811250411_4667Macro`,
  `_N_operator2881563629_4848Macro` (all 25 changed; `_<n>` indices stable).
  Spot proof the new digits are FNV-1a: `fnv_check.ps1` computes
  FNV-1a32("&&")=2881563629 and FNV-1a32("||")=1431891397, matching
  `_N_operator2881563629_4848Macro` / `_N_operator1431891397_4850Macro`.
  Suite green (above).
- **P4 (keyed-IVT probe, `.tmp\phase93\ivt\`, sn.exe keys)**:
  | case | pre-fix (9.2 Stage2 ncc) | post-fix (Stage1 ncc) | post-fix (final Stage2 ncc) |
  |---|---|---|---|
  | friend signed key1 vs `PublicKey=hex(key1)` | **FAIL** `unbound name 'IvtLib'` | **exit 0** | **exit 0** |
  | friend signed key2 (wrong key) | FAIL | FAIL `unbound name` | FAIL `unbound name` |
  | friend unsigned vs keyed IVT | FAIL | FAIL `unbound name` | FAIL `unbound name` |
  | friend unsigned vs name-only IVT (control) | exit 0 | exit 0 | exit 0 |
  (`probe_prefix_stage2.txt`, `probe_postfix_stage1.txt`,
  `probe_postfix_stage2.txt`)

### G4 — provenance

`git log --oneline master..sync-master` = 9 commits (5 from 9.2 + the 4
ports above). Per-port diffs: P1 `11+/3-`, P2 `20+/11-`, P3 `20+/1-`,
P4 `31+/15-`, each confined to its single target file. Every commit message
carries a `Provenance:` line (`manual-port-of:` P1/P2/P4,
`cherry-picked-from:` P3 code-only).

### Reverse direction (record-only)

`f5fe58328` ("Fix NemerleAll.nproj and detection of external parser loading",
master) and `testsuite/positive/Bugs/lexer1.n` are present on master/
`sync-master` but **absent in `retarget-compiler`** (verified 2026-10-31: no
`lexer1*` under retarget `testsuite/`) — **future retarget-side atom**; not
touched by this task.
