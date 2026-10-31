# Review #4 — final independent critical review of the net10 retarget (`retarget-compiler`, commits `17d3cf47c..HEAD`)

Reviewer: independent subagent #4 (final). Scope: the whole migration package at
`a4dc32f91`, with assigned attack angles: seed threat model, user-facing
cherry-pick breakage, the 6.2b default-ref design at scale, harness honesty,
group-think audit of reviews #1–#3, and final hygiene. Prior reports were read
only after the independent evidence below was collected.

Method: read-only git forensics; **`git merge-tree` cherry-pick simulations
against `master` (read-only, no working-tree changes)**; one direct timing
measurement with the already-installed `bin\Debug\net-4.0\ncc-core.exe`
(running a binary, not a build). No repo files modified, no builds run, no
commits. The full gates (FC ×10 + 461+7nr/167) were green at HEAD per the
checklist; re-running them was not required and was not done. Artifacts under
`.tmp\phase8r4\`.

## Summary verdict

**PASS WITH FINDINGS.** The migration itself (toolchain, bootstrap, harness,
gates, hygiene) survives a fourth independent attack pass: the bootstrap chain,
the fixed-point gate, the 6.2b design (as documented), the 7
`BEGIN-OUTPUT-CORE` blocks and the repo hygiene all held up under re-
examination, and my two re-checks of claims #1–#3 (R2-01 precedence fold,
6.2a per-compile cost) reproduce. But the cross-branch sync mechanism —
`doc/MASTER-CHERRY-PICKS.md`, the artifact reviews #1–#3 all signed off as
correct — **does not survive contact with `master` for the two most important
candidates**, because nobody (all three reviewers included) ever simulated
actually applying it. One major (doc/process), two minors, one nit below.
The major must be fixed before atom 8.3e pushes anything or the branch is
considered closeable.

## Attack angle 1 — THREAT MODEL OF THE SEED (explicit answer)

**What the repo guarantees that `boot-net10` == what the sources claim:
nothing technical. The guarantee is purely social (a provenance story in the
commit message plus progress docs), and it is not reproducible without
archaeology.**

* Provenance: `boot-net10/` entered in one commit, `2ff4e2fb4`, whose message
  asserts "built from the validated Stage2 set … seed binaries committed only
  after this green cycle per the seeding rule (4.6)", and
  `progress4.2.md`/`progress4.3.md` record the Stage2 manifest and the green
  cycle. There is **no checksum manifest, no signing check** (the `.snk` keys
  are in the repo, so signing is an identity marker, not a trust anchor), and
  **no forward-looking re-seed recipe** in any live doc: AGENTS.md explains the
  promotion *criterion* (Stage2/3 + fixed point) and the 10-file list, but the
  operative recipe ("which stage dir to copy, which files, compare against
  what, commit rule") lives only in the archived progress reports;
  `NET10-BUILD-BASELINE.md` never mentions reseeding.
* Tripwire analysis (the constructed thought experiments):
  * *Seed older than sources, source needs feature X* → **Stage1 fails**. Caught.
  * *Seed newer than sources, compiler source uses X* → Stage1 green,
    **Stage2 fails** (Stage1's output lacks X). Caught — this is the classic
    two-stage safety property and it works.
  * *Seed newer/different than sources, source never uses the difference* →
    **nothing catches it, and it does not need to** for bootstrap integrity,
    but it means the equality claim "seed = the validated Stage2 output of
    2ff4e2fb4" is forever unverifiable after the fact.
  * Crucially, **`Validate` compares the LAST TWO stages (Stage2↔Stage3,
    `NemerleAll.nproj:334-385`), never Stage1↔Stage2.** Stage2↔Stage3 is the
    fixed-point (self-reproducibility) check and is *blind by construction* to
    any seed codegen divergence: both compared assemblies are products of
    source-behavior compilers regardless of what the seed emitted. A seed
    whose IL differs from the current compiler's (benignly or maliciously —
    the Thompson "trusting trust" construction) passes FC ×10. `fc` on
    Stage2↔Stage3 was never a seed-verification step and no doc claims it is —
    but AGENTS.md's sentence "If Stage2 succeeds and its IL **matches
    Stage1** … a fixed point is reached" conflates the two comparisons (the
    canonical gate `Stage3,Validate` compares Stage2↔Stage3; only a
    `Stage2,Validate` run does the seed-facing Stage1↔Stage2 comparison).
    A one-off `Stage2,Validate` after each reseed would at least surface
    *benign* seed codegen drift; it is not run as a norm.
* Aggravating hygiene detail (already R1-09): `NPrepareBoot` copies the seed by
  `*.exe/*.dll/*.config/*.json` globs with no pin of the documented 10-file
  manifest — a stray dll dropped into `boot-net10/` is silently consumed into
  `$(NBoot)` and then used as compiler.

Disposition: this is inherent to *any* binary-seed bootstrap (the honest stance
is "the seed is a trusted anchor, rotation is a recorded human act"), so the
finding is minor, not major — but the cheap hardening below is worth doing.

## Attack angle 2 — USER-FACING BREAKAGE: the full cherry-pick walkthrough

Walked `doc/MASTER-CHERRY-PICKS.md` end-to-end by **simulating the recommended
cherry-picks onto `master` with `git merge-tree --write-tree
--merge-base=<sha>^ master <sha>`** (exact cherry-pick 3-way merge semantics,
read-only). Result — **the doc's advice does not survive**:

`master` still has the **pre-split monolithic compiler layout**
(`ncc/CompilationOptions.n`, `ncc/passes.n` contains `ManagerClass`,
`ncc/external/InternalTypes.n` contains `InternalTypeClass`,
`ncc/external/LibraryReference.n`; verified via `git ls-tree master` /
`git grep -l`). The frontend/backend/shared reorganization happened on this
branch line *before* `17d3cf47c` (merge-base `01e527527` still had the old
layout; `17d3cf47c` already has `ncc/backend`). The doc acknowledges "this
checkout's location of the shared `hierarchy/`/`typing/`" in one parenthetical
and then proceeds to recommend mechanical cherry-picks anyway. Simulations:

| sha | doc advice | simulation result |
|---|---|---|
| `e9741864a` | port (reorder, "safe") | **CONFLICT (modify/delete)** on `ncc/backend/hierarchy/InternalTypeClass.n` — "deleted in master … version left in tree" |
| `5a7dadbf1` | port (a), skip (b) | modify/delete on the same file + content conflict on `ncc/CompilationOptions.n` (rename detected there, but hunk (b) is to be skipped anyway) |
| `f35161ae9` | port with `StrongNameKeyPair` re-implement | **modify/delete** on `ncc/backend/hierarchy/LibraryReference.n` (master's `ncc/external/LibraryReference.n` not rename-detected) |
| `171f1f227` | port | **applies cleanly** via rename detection into `ncc/hierarchy/MacroClassGen.n` (verified resulting tree: the 21-line FNV hunk lands; progress-doc addition is cosmetic) |
| `1231f73af`, `9ecc5c8a2` | conditional / harness-no-port | ordinary add/delete + content conflicts on testsuite/harness paths (expected; doc already segregates the harness parts) |

Why the modify/delete outcome is dangerous rather than merely inconvenient:
master's `Nemerle.Compiler.nproj` uses **explicit** Compile globs
(`ncc\hierarchy\*.n`, `ncc\external\*.n` — single-`*` globs do not span
directories). Resolving the conflict the way git's own hint invites ("version
left in tree" → keep theirs) places a ~1500-line retarget-layout file at a path
master never compiles → **the port is a silent no-op while the build stays
green**. And the doc's mandatory FX gate (R1-03 pre-condition) *cannot* catch
it: the `InitSystemTypes` reorders are behavior-neutral on FX by design, so an
FX bootstrap+suite passes identically with or without them. The only detection
path is a human who knows the fix must be hand-ported into
`ncc/external/InternalTypes.n` — knowledge the doc does not provide. (The
`f35161ae9` row partially self-rescues because it already demands manual
re-implementation, but not because of any layout warning.)

The 5 hunks themselves are clean — I `git show`'d `5a7dadbf1` in full: pure
statement reorder + host-gated `PdbType` default; **no hidden dnlib-side
dependency in any hunk** (that claim of the doc holds; the failure is purely
path/identity, not code).

## Attack angle 3 — 6.2b DEFAULT REFS AT SCALE

* Cost claim: no per-test timing survives on disk (the harness leaves only a
  36-byte `test_out.txt`), so I measured directly with the installed compiler
  (3 runs each, this session): default path **0.78 s/compile** vs
  `-no-stdlib`+minimal-`-r` **0.36 s/compile** on an empty library → ≈ +0.42 s
  for the 167-ref load, consistent with 6.2a's +0.34 s (same order; suite
  overhead lands ≈ +4–5 min vs the documented +2–3 min estimate — no material
  discrepancy).
* Creep exposure is real and **unguarded**: the implicit reference set is
  "every non-native dll in the ACTIVE shared-framework dir". The next runtime
  feature release adding in-box managed assemblies silently grows the
  zero-`-r` API surface — programs start compiling that would fail on another
  installed runtime (non-reproducible compiles, the exact property FX never
  had). New *native* payloads outside the hand-maintained `is_native` blocklist
  (`ManagerClass.n:579-587`) become compile-time warnings on every invocation
  (8.3f made them loud, not fatal — the right failure mode, but noisy).
* **No test, gate or doc baseline pins the set** (grep over `testsuite/`,
  harness and CI: nothing). R2-02 recorded the drift as a "known limitation,
  accept-with-record"; I disagree that no guard is needed — see R4-03.
* The "curated list vs whole dir" trade-off: 6.2a correctly rejected the ref
  pack (absent on runtime-only installs); the ordinal-sort +
  last-registered-wins design (per #3's R2-01 adjudication, which I accept —
  the `LoadTypesFrom` prepend + head-keeping fold reading is right) makes
  duplicate resolution deterministic. The remaining gap is only the un-pinned,
  un-surfaced drift surface, which a 5-line smoke probe would close.

## Attack angle 4 — HARNESS HONESTY

* `BEGIN-OUTPUT-CORE` masking: **cannot happen through the Trim() vector.**
  The comparison (`NccTest.n:386-399`) is a positional walk over *all* lines
  with per-line `Trim()` + `Ordinal` equality, plus explicit "longer than it
  should be"/"unexpected end" errors — and it is the *same code* for FX and
  Core. Trim() weakness is therefore symmetric across hosts: there is no test
  where "the FX block's exactness is load-bearing on whitespace" and Core
  legitimately differs in indentation — whitespace-insensitivity applies to
  both. A Core block copied line-for-line from FX also fails identically when
  Core output regresses. The only masking vector is a Core block *enshrining*
  a regression as expectation; the 7 blocks are the documented runtime drifts
  (round-trippable `double.ToString` shortest form — I verified
  `linq-2-ExprTree.n`/`Issue-git-0274.n` carry the `10.799999999999999`-family
  values with explanatory comments; #1 R1-11 spot-checked the rest). Agree
  with "accept as-is".
* runtimeconfig pin: the **harness** side is self-healing — `NccTest.n:227-236`
  writes `"<harness Major>.0.0"`, so a machine running the harness necessarily
  satisfies the generated config; on a .NET-11-only box the harness itself
  never starts, so no trap there. The **seed** side is a static
  `boot-net10/ncc-core.runtimeconfig.json` pinning `Microsoft.NETCore.App
  10.0.0` with default (Minor) roll-forward: on a 10.0.1-only box it runs
  (Minor rolls forward fine), on an **11.x-only box `ncc-core.exe` refuses to
  start**. Severity call: **nit** — this is stock .NET framework-dependent
  policy (every net10.0 app behaves the same; users install the 10.x runtime or
  set `DOTNET_ROLL_FORWARD=Major`), and the whole toolchain targets net10
  anyway. But review #3 listed "runtimeconfig pin … no rollForward key needed"
  as a clean area while only analyzing the harness half; the seed half deserves
  the one-line doc note (R4-04).

## Attack angle 6 — HYGIENE FINAL

Clean. `git log --diff-filter=A --name-only 17d3cf47c..HEAD` (62 commits) adds
exactly 73 unique paths, all intentional: plan/progress/review docs, the
documented `boot-net10/` seed (10 files), `build.ps1/.sh`, `global.json`,
`Directory.Build.props`, `nuget.config`, CI workflow, `AGENTS.md`,
`testsuite/ci-known-failures-64.txt`, two App.configs. **Zero** bin outputs,
logs, `.il`, nupkgs, zips or `.tmp` paths added; the only tracked `.nupkg`
files are pre-range VS2010 template assets (`snippets/VS2010/.../*.nupkg`,
untouched by the range). `.gitignore` covers `[Bb]in*/` (subsumes
`bin\<Config>\net-4.0\Nuget`), `.tmp/`, `*.tmp`, testsuite temp globs. Working
tree clean at HEAD.

## Findings

| ID | severity | area | finding + evidence | disposition |
|---|---|---|---|---|
| **R4-01** | **major** | doc / cross-branch sync (`doc/MASTER-CHERRY-PICKS.md`) | The port guidance is inapplicable as written for the 3 code candidates: `master` retains the pre-split monolithic layout (`ncc/external/InternalTypes.n` contains `InternalTypeClass`, `ncc/external/LibraryReference.n`, etc.; the split predates `17d3cf47c` and is out of audit range but in the doc's debt). Cherry-pick simulations (merge-tree, this session): `e9741864a` and `5a7dadbf1` → modify/delete conflict leaving the whole retarget file in tree; `f35161ae9` → same on `LibraryReference.n`. Master's explicit Compile globs (`ncc\hierarchy\*.n`) never compile the stray → **silent no-op port**, and the mandatory FX gate cannot catch it because the reorders are FX-behavior-neutral. Only `171f1f227` ports mechanically (rename-detected into `ncc/hierarchy/MacroClassGen.n` — verified). Reviews #1–#3 verified the rows "against their diffs"; none simulated applying them to master. The 57-commit count is additionally still stale (62 at HEAD). | Before 8.3e: add a "target file on master" column (`ncc/external/InternalTypes.n`, `ncc/external/LibraryReference.n`, `ncc/passes.n`), state plainly that e9741864a/5a7dadbf1(a)/f35161ae9 require **manual porting into the monolithic files** (cherry-pick cannot be used; warn about the modify/delete "left in tree" trap), note `171f1f227` as the sole mechanically-portable code candidate, pin the end-sha and fix the count. |
| **R4-02** | minor | seed threat model | See angle 1: no checksum manifest or automated re-seed recipe; seed↔source equality is a commit-message assertion; `Validate` is Stage2↔Stage3 and is blind to seed drift by construction; AGENTS.md's "matches Stage1" sentence conflates the seed-facing (Stage1↔Stage2) and fixed-point (Stage2↔Stage3) comparisons; `NPrepareBoot` globs without pinning the 10-file manifest. | Add `boot-net10/SEED.manifest` (SHA-256 per file + producing commit + the green-cycle citation); fold the operative re-seed recipe (from progress4.2/4.3) into AGENTS.md; recommend one `Stage2,Validate` run per reseed as the seed-facing comparison; optionally fail `NPrepareBoot` if the seed dir deviates from the manifest file set. Residual trust (Thompson seed) is irreducible and should be stated as such rather than left implied. |
| **R4-03** | minor | 6.2b future-proofing / no drift guard | Zero-`-r` implicit surface = whole shared-framework dir, **no guard pinning or surfacing drift**: next runtime release silently extends compilable API (per-runtime compile divergence) and unlisted new natives warn on every compile. Cost re-measured this session: +0.42 s/compile (consistent with the documented +0.34 s). No test/grep/gate mentions the 167 count. | Add a CompilerTests-adjacent smoke: compile a probe `.n` and assert **zero** `cannot load default framework reference` warnings; record the current expected default-ref count in NET10-BUILD-BASELINE.md with an explicit "may grow with runtime" note. Do **not** pin 167 exactly (would break on every runtime patch — pin the behavior, not the number). Upgrades R2-02's accept-with-record disposition. |
| **R4-04** | nit | seed runtimeconfig / future .NET | `boot-net10/ncc-core.runtimeconfig.json` statically pins `10.0.0`, no `rollForward` → `ncc-core.exe` won't start on an 11.x-only machine (stock Minor policy; harness side is dynamic-major and safe). | Optional: set `RollForward=Major` on `ncc-core.nproj` so future seeds carry it, or one doc line (`DOTNET_ROLL_FORWARD=Major`). Accept-as-is is also defensible. |

No blockers; no defect in migration *code* was found — R4-01 is a doc/process
defect whose blast radius is the master branch, which is exactly what the
migration's own AGENTS.md declares most precious.

## Disagreements with colleagues (per prior finding)

| prior | their call | my call | reason |
|---|---|---|---|
| R1-03 / R1-04 / 8.3d doc (all three reviews touched it) | candidate table correct; R3: "partial-port advice is correct **against its diff**" | **upgrade the remediation's defect to major (R4-01)** | "correct against the diff" is precisely the standard that missed the failure: nobody simulated applying it to master; two of the three code candidates cannot be cherry-picked at all, and the failure mode is silent. |
| R2-02 | "record as known limitation", doc note | **upgrade disposition: guard before close (R4-03)** | a shipped always-on whole-dir scan deserves a machine-checked drift probe, not only prose, given the suite's zero-`-r` design makes it load-bearing. |
| R2-01 (review #2's original) | minor bug (first-load-wins, reorder fix) | **confirm #3's refutation** | I re-read the cited `LoadTypesFrom`/`LoadValue` paths; prepend+head-fold ⇒ last-registered wins, and #2's probe was non-discriminating. #3 right, #2 wrong — correctly adjudicated. |
| #3 clean-area: "runtimeconfig pin … no rollForward key needed" | clean | **partially reject** | true only for the dynamic-major harness config; the static seed config (the half not analyzed) has the 11.x-only trap (R4-04, nit). |
| R1-11 (BEGIN-OUTPUT-CORE accept-as-is) | accept | **agree** | positional full-sequence comparison with symmetric Trim(); a copied FX block fails exactly when Core regresses; 7 blocks are genuine documented drift (round-trip double format verified in-file). |
| R3-01 (count 57≠58/61) | minor | **agree, still open**: 62 at HEAD, 3 further doc commits added since #3 counted, none fixed it | nit-by-substance (all unaccounted shas are doc-only), but it is now stale three reviews running — fix once at close-out. |
| R3-02 (machine paths in `Nemerle.Location.nproj:30-32`) | minor | **agree — nit-tier** | re-verified present at HEAD; debug-launch properties only, no build-graph impact, no CI path; still should be made `$(MSBuildProjectDirectory)`-relative eventually. |
| R3-03 (AGENTS.md "FC ×5") | nit | **agree — nit** | re-verified at HEAD; 5 assemblies × 2 directions = 10 is the norm (checklist line 96, build.yml `-lt 10`). |
| R3-04 (duplicate 8.3d row) | nit | **agree — nit** | re-verified: checklist lines 100 (`[ ]`) and 104 (`[✅]`) both present at HEAD. |
| R3-05 (`NStageSelect` non-reset) | nit | **agree — nit** | reachable only via repeated standalone `_NugetPack` in one MSBuild process; the selected-stage Message makes any flip visible. |
| R3-06 (Sdk.targets condition partiality) | nit | **agree — nit** | props-exists-targets-missing is a pathological seed drop; guard covers the realistic stale-seed case. |
| R1-01 (CI never run on GitHub) | major, open | **cannot independently confirm** (remotes not pushed from this box), keep as-is | honestly tracked as 8.3e; remains the package's biggest unevidenced claim until the first real CI run. |
| R1-09 (boot dir never cleaned) | nit | **agree, and fold into seed threat model** | it is the concrete mechanism by which a "silently stale seed" actually happens day-to-day (stale `bin\…\boot\` mixing). |

## Group-think notes (#5)

* The one PASS-type conclusion I cannot confirm at all is R1-01's negative
  evidence (what GitHub Actions *would* do); everything else in #1–#3 I either
  re-derived (R2-01 precedence: yes; FC wildcard semantics: accepted from #3's
  reproduced experiment; 6.2a cost: re-measured; `_NugetPack` scan order:
  accepted from #3's MSBuild probes) or re-checked statically (Validate globs,
  bidirectional fc, guard targets, seed manifest vs AGENTS).
* The collective blind spot of #1–#3 on the cherry-pick doc is methodological:
  all three treated MASTER-CHERRY-PICKS.md as a *classification* artifact
  (is each commit correctly bucketed?) and verified it against diffs; none
  treated it as an *executable* artifact and ran it. That is exactly the
  difference between reviewing a map and taking the drive.
* "Accepted as is" dispositions I'd reopen: R2-02 (→ R4-03 guard). The rest of
  the accepted set (R1-11, R1-12, R2-04..R2-08, R3-05..R3-09) I concur with.

## Clean areas (one line each)

* Bootstrap chain & gates: Stage1↔Stage2 catches seed-too-old and
  source-needs-new-feature; fixed point + bidirectional fc ×10 verified sound
  by #3 and not contradicted by anything found here.
* The 5 cherry-pick *hunks* themselves: pure reorder + host-gated defaults, no
  hidden dnlib dependencies (full `git show` of `5a7dadbf1`).
* 6.2b cost claim: +0.42 s/compile measured today, matches documented +0.34 s.
* Harness output comparison: strict, positional, host-symmetric; 7 Core blocks
  are genuine runtime drift with in-file justifications.
* Harness runtimeconfig: dynamic-major, self-healing (seed half is R4-04).
* Hygiene: zero accidental artifacts in 62 commits; .gitignore adequate;
  working tree clean.

## Verdict

**PASS WITH FINDINGS** — no blocker; R4-01 (cherry-pick doc inapplicable to
master, silent-no-op failure mode) must be remediated before 8.3e/close-out;
R4-02/R4-03 (seed manifest + default-refs drift guard) are cheap hardenings
best folded into the same final atom; R4-04 optional. The remediation package
#3 audited remains faithful; my one substantive disagreement with the
collective is that MASTER-CHERRY-PICKS.md was never executed against master by
anyone — once simulated, three of the five "port" candidates turn out to be
manual-surgery items the doc doesn't tell you about.
