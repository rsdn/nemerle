# Critical review #1 — net10 retarget (`retarget-compiler`, commits `17d3cf47c..HEAD`)

Reviewer: subagent #1 (task 8.1). Date: 2026-10-31. Scope reviewed: 50 commits
`17d3cf47c..e461d3f75`, the plan, the checklist (statuses/Journal/Deviations),
progress docs, and the working tree. **No builds were run** (static
verification only; local `bin\Debug\net-4.0` from an earlier session was
inspected read-only). No files were modified except this report.

## Summary verdict

**PASS WITH FINDINGS.** The core bootstrap invariant (boot-net10 → Stage1→2→3
→ Validate FC) is structurally sound in `NemerleAll.nproj`, the seed layout and
`NPrepareBoot` are correct, the Validate `fc` gate is genuinely binding, the
shared language surface changes are carefully host-gated and port cleanly to
master, the suite norm (461+7nr/167) is backed by correct host-aware
expectations, and repo hygiene is clean. No blocker found that would poison the
bootstrap chain. The most significant problems are **process/verification gaps**
(CI never executed on a real runner while the workflow embeds unproven
runner-environment assumptions) and **post-7.4 dead defaults pointing at the
deleted `boot-dnlib` seed** in the four core `.nproj` files, plus several
minor/nit inconsistencies listed below.

## Findings

| ID | Sev | Area | Finding (evidence) | Suggested disposition |
|----|-----|------|--------------------|-----------------------|
| R1-01 | major | CI / verification | **The CI workflow has never run on GitHub.** `github/retarget-compiler` and `kl/retarget-compiler` both sit at `d9b50f721` (2021-11-17, pre-migration); none of the 50 commits are pushed anywhere. All "CI" evidence in checklist 6.3 is a *local* proxy run on the dev box (VS18/MSBuild 18.11). The workflow additionally assumes the `windows-latest` runner provides: (a) an MSBuild **≥ 18** discoverable via vswhere — the repo's own deviation note for 1.2 says global.json/SDK 10.0.402 breaks legacy msbuild 17.14 with MSB4236, and `build.ps1:100-107` takes whatever `-latest` VS vswhere finds; (b) Windows-SDK `ildasm.exe` resolvable through `GetFrameworkSdkPath` (`NemerleAll.nproj:164-184`); (c) FX 4.6.1 targeting packs for `_Wpf` inside `CompilerTests` (`NemerleAll.nproj:591-593`). Whether the 2026 `windows-latest` image ships VS/MSBuild 18 is **unverified — speculation flagged**; the *gap* (untested CI presented as green) is the defect. | Push the branch and let the workflow run (or add an explicit runner preflight step asserting `MSBuild -version >= 18` and ildasm presence); fix what breaks before claiming 6.3 done. |
| R1-02 | minor | 7.4 deletions / stale defaults | After `boot-dnlib/` was deleted (00cee342b), the four core projects still default `Nemerle=` to the deleted dir: `Nemerle.nproj:3`, `Nemerle.Compiler.nproj:3`, `Nemerle.Macros.nproj:3`, `ncc-core.nproj:3`. Three of them carry a line-5 fallback that silently re-points at `bin\Debug\net-4.0\boot` — i.e. a standalone `dotnet build Nemerle.Compiler.nproj` (the Phase 2.5 acceptance workflow) **consumes whatever stale artifacts a previous orchestrator run left there** (Debug-hardcoded; Release ignored), and `ncc-core.nproj` has no fallback at all, so standalone build dies with a confusing "boot-dnlib\Nemerle.Sdk.props not found" Import error. The orchestrator is unaffected (always passes `Nemerle=$(NPrevBin)`). | Default to `boot-net10` when it carries the MSBuild files, else to `$(MSBuildProjectDirectory)\bin\$(Configuration)\net-4.0\boot`, and fail with an actionable message when neither exists. |
| R1-03 | minor | Master compat / gates | The compiler-core reorderings in `ncc/backend/hierarchy/InternalTypeClass.n` (Void/Object/ValueType first; `Delegate_Combine/Remove` before the attribute lookups) and the `f35161ae9` IVT fix are language-surface changes that per `AGENTS.md` must land on master too — but every gate on this branch since 3.1b is Core-only (FC ×5 + Core suite); **no FX bootstrap or FX suite run has validated them on this branch**. The reentrancy argument for FX is comment-reasoned, not FX-execution-proven (progress3.1b, checklist 3.5d). A master cherry-pick that ICEs the FX bootstrap would be discovered only after the merge. | Before cherry-picking 3.1a-fix/3.5d/3.4a-fix to master, run a master-side Stage1→Stage2 bootstrap on an FX host; record it as an explicit pre-condition in the checklist. |
| R1-04 | minor | `f35161ae9` semantics | `f35161ae9` is **not semantics-neutral on FX**: removing the `IsNullOrEmpty(StrongAssemblyKeyName)` guard and switching the split from `','` to `'='` turns a previously *doubly-dead* branch into live code — keyed `InternalsVisibleTo` is now honored on FX as well (strictly permissive: only adds `IsFriend=true` on a real key match; wrong/unsigned key keeps the old "unbound name"). Suite-neutral: verified no testsuite file uses `PublicKey=` in IVT (all IVT usages are name-only: `internals-visible-to-1.n`, `gtest-friend-01-lib.n`, `Issue-git-0571/72/78/82-lib.n`), so master's suite survives the cherry-pick. But the checklist calls it "фикс поведения" only in passing; labeling it "semantics-neutral" would be wrong. | Cherry-pick to master with a commit note "behavior fix: keyed IVT now honored"; no suite risk found. |
| R1-05 | minor | Validate binding | FC ×5 overstates self-host coverage by one: `Validate`'s `Nemerle*.dll` glob (`NemerleAll.nproj:337-340`) picks up `Nemerle.MSBuild.Tasks.dll` in both stage dirs, but that file is a byte-identical copy of the boot-built tasks (`NTasks` builds it once into `$(NBoot)`, each stage just `Copy`s `@(NTasksFiles)` — lines 281/292/303/314). That comparison is tautologically green; genuine self-compiled IL comparison is ×4 (Nemerle, Nemerle.Compiler, Nemerle.Macros, ncc-core.dll). Additionally `fc "<prev>\*.il" "<cur>\*.il"` iterates only the *first* pattern: an assembly present only in Stage3 is silently unchecked (a Stage2-only file correctly fails with "FC: cannot open"). The error propagation itself is sound (non-zero fc exit → MSB3073 → red; CI double-checks via log grep). | Either drop `Nemerle.MSBuild.Tasks.dll` from the Validate globs or re-word the "5 assemblies" norm to "5 FC comparisons (4 self-compiled + 1 tasks copy)"; optionally diff the reverse direction too. |
| R1-06 | minor | Checklist honesty | Completed Phase 5 atoms were never checked off: checklist lines 70-73 mark 5.2/5.3/5.4/5.5 as `[ ]` although commits `141c005c8`, `892d0f553`, `6b0f0d5db`, `a60ddff19` and their progress docs (all "DONE") exist; no Journal entry for them either (the Journal jumps 5.1 → Phase 3/4 closure). Also item 8.3 (line 96) still names the stale acceptance norm "468/167/0" while the current Core norm is 461+7nr/167 (line 83/89). | Tick 5.2-5.5 with commit pointers, add the Journal lines, fix the 8.3 norm string. |
| R1-07 | minor | CI comments / stale docs | `.github/workflows/build.yml:115-124`: the tests-job comment and step name still say "8 known failures until 6.4" / "TODO(6.4): fix the 8, then delete" although 6.4 landed in the very same series (allowlist is comments-only) and `NKnownFailures='ci-known-failures-64.txt'` now plumbs an empty file (harmless but misleading). `NemerleAll.nproj:25-26` comment still says "boot-dnlib stays on disk as emergency fallback until 4.5 closes" — it was deleted in 7.4. `build.ps1:20-24`/`build.sh:17` promise "Phase 6.3 retires the ildasm discovery" — 6.3 shipped without retiring it (GetFrameworkSdkPath still blocks `dotnet msbuild`, which AGENTS.md correctly documents). | Cosmetic sweep in the 8.3 cleanup atom: reword CI comments, delete the stale boot-dnlib sentence, correct the 6.3 forward-reference. |
| R1-08 | nit | 7.1 deletions | `misc/packages/wix/Sources/CompilerAndTools.wxs:76-90` still declares `ncc32.exe`/`ncc64.exe` (+ companions) files whose sources were deleted in 7.1 and whose binaries no longer exist in any stage output — `_Installer` is opt-in-guarded (5.5) so nothing breaks today, but the guarded path is now more dead than before. All other 7.1 deletions verified referenced nowhere outside historical docs (grep for `GacReg|Reg-bins|Reg-boot|Build-core-with-ide|NemerleAllTest|ncc.nproj` hits only comments/plan docs and the WiX file). | Either delete the WiX atom too or note it in the 7.5-style deferred list explicitly. |
| R1-09 | nit | Bootstrap robustness | `NPrepareBoot`/`NTasks` never clean `$(NBoot)` before copying (`NemerleAll.nproj:234-255`); overwriting is same-name-only. A future seed change that *drops* a file leaves the stale one in `bin\…\boot\` and Stage1 consumes the mixed directory (observed local `boot\` already carries NTasks byproducts like `Nemerle.MSBuild.Tasks.deps.json/.pdb` alongside the seed — benign today). Similarly stage dirs are never cleaned (a pre-3.4b `ncc.exe` left in `Stage*\` would even be picked up by Validate's `ncc*.exe` glob). | `RemoveDir $(NBoot)` (or at least the seed globs) at the top of `NPrepareBoot`; document the stale-bin hazard. |
| R1-10 | nit | Packaging (6.5) | Package content verified against the old nuspecs: correct `lib\netstandard2.0` placement (D2/D6), tool closure = seed set, `Nemerle.Peg.dll`/`Nemerle.Diff.dll` kept in the Nemerle package, generated shim stays in git-ignored `$(NObj)\Nuget` — nothing checked in that shouldn't be, no temp artifacts in any of the 50 commits (added-files scan: docs + seed + config only). One intentional loss: the old `Nemerle.???` globs shipped `.xml` doc files (and `.pdb`); the new packages ship neither. XML docs are consumer-visible value, not "ghosts". | Optionally re-add `Nemerle.xml`/`Nemerle.Compiler.xml` under `lib\netstandard2.0\` (they are produced in stage dirs). |
| R1-11 | minor (speculation flagged) | Suite norm / masking | The 7 BEGIN-OUTPUT-CORE expectations were spot-checked and are genuinely correct runtime-drift, not "record whatever printed": `assert.n`/`notnullorempty.n` diff is exactly the documented Core `ArgumentException.Message` one-line `"(Parameter 'x')"` format; `Issue-git-0053` matches Core `Expression.ToString` rendering `Convert(2, Int64)`; the double cases (`overloading.n` 61.199999999999996, `linq-2-ExprTree.n` 10.799999999999999, `basic-value-types.n` 5.4000005447844055) are values computed at runtime by emitted code, where FX's G15 formatting hid sub-ULP distance from the literal. Residual masking risk (flagged as speculation): because the comparison is literal and the FX baseline G15-rounded, a *future* Nemerle evaluator/emit regression of ≤ a few ULP would also just be "re-recorded"; likewise the pattern hides that the embedded doubles for literals like 10.8 are not nearest-representable (pre-existing on both branches — ncc parses literals with the host's `Double.Parse`, `Lexer.n:1055-1062`). | Acceptable as-is for the plan. Consider a follow-up "literal-folding ULP audit" atom upstream (would also matter for master). |
| R1-12 | nit | Harness | `NccTestExecutionListener` prints the "entries that did not fail" notice per-run (already noted "косметика" in checklist 6.3); with the now-empty allowlist the whole `-known-failures` plumbing is exercised in CI only in its empty configuration — the non-empty path has local evidence only (progress6.3). | No action now; exercise a temporary entry once when the mechanism is next needed. |

## Areas checked and found clean (with what was checked)

- **Bootstrap invariant.** `NRootBoot=$(NRoot)\boot-net10` (`NemerleAll.nproj:27`)
  matches AGENTS.md; `boot-net10/` contains exactly the 10 seed files documented
  in progress4.2 (incl. `ncc-core.runtimeconfig.json/.deps.json`;
  `NPrepareBoot` copies `*.exe/*.dll/*.config/*.json`, lines 234-255 — the
  `*.json` addition is present). `Stage1` consumes only `$(NBoot)`; `StageN`
  consumes only `Stage{N-1}`; `-no-stdlib` (used by every stage build via
  `NoStdLib=true`) provably skips the 6.2b shared-framework scan (the scan lives
  in the `else` of `if (Options.DoNotLoadStdlib)`, `ManagerClass.n:532/561-596`).
  Seed binaries intentionally differ from current stage output (the seed predates
  the 6.2b ManagerClass change) — correct per "seed must be a *prior* binary".
  FC error propagation traced: `Exec` default `ContinueOnError=false`, non-zero
  `fc` exit (1=diff, 2/4=usage/cannot-open) → MSB3073 → red (see R1-05 for the
  two coverage nits). The standalone-`Validate` misuse mode fails red, not
  silently green (empty globs → `fc` cannot-open → Exec error).
- **Master compatibility of the shared surface.** Full diff of
  `ncc lib macros testsuite` over `17d3cf47c..HEAD` is 25 files; every compiler
  change is host-gated on the `System.Private.CoreLib` probe
  (`CompilationOptions`, `ManagerClass`, harness) or host-neutral-deterministic
  (`fnv1a32` in `MacroClassGen` — actually *more* master-safe since FX string
  hashes were only same-machine-stable); `PdbType.Windows` remains the 0-value
  so the FX default is byte-identical (`TypesManager.n:29-34`). Marker
  master-safety claim independently verified: `regexp match` wraps every arm as
  `^(...)$` (`macros/text.n:160`), so `^BEGIN-OUTPUT` cannot match
  `BEGIN-OUTPUT-CORE`; `NO-CORE` matches no pre-existing arm and is not a
  `NO-TEST` substring; the 7 marker-line insertions shift no line-numbered
  expectations (grep over all 7 files: no `// E:`/`// W:`/`(l,c,l,c)` content);
  `Issue-git-0297-cs.cs` rename is referenced nowhere (grep `0297` → docs only),
  so master's harness picks it up by glob and its FX run is unaffected.
- **Suite norm.** 7 NO-CORE files audited (content genuinely FX-only: WinForms,
  System.Web, CAS, BinaryFormatter, in-box CodeDom, FX resource-stream metadata);
  CORE-block diffs reduce exactly to documented runtime format changes
  (assert/notnullorempty differ only in the ArgumentException one-line parameter
  suffix; other lines identical — line-by-line diff performed). NO-CORE gate is
  host-gated in `NccTest.parseStep` and inert on FX.
- **CI structure (independent of R1-01/R1-07).** Quoting is correct
  (build.ps1 joins target arrays, comma lists pass as one argv token);
  `NKnownFailures` env → nproj property → `-known-failures <bare name>`
  resolves because `CompilerTests` copies `testsuite\*.*` into both working
  dirs; the gate step's regexes (`MSB3073.*"fc "`, `FC:.*(?<!no )differences`,
  `≥5 × "FC: no differences"`) are false-positive-free on the expected output;
  the smoke steps correctly use Stage3 in the build job, and the tests job
  correctly never references Stage3.
- **7.1/7.4 deletion residue.** Beyond R1-02/R1-08: no surviving references to
  deleted scripts in build graph or CI; `MSBuild.Community.Tasks.dll` (still
  used by `Validate`/`FileUpdate`) and `junction.exe` remain in
  `ExternalDependences`; the NTasks `_dummy` Restore split has no counterpart
  hole in the Core graph (every other MSBuild-invocation that builds
  PackageReference-consuming projects already has the split; `Install`,
  `NPrepareBoot`, `NPrepareKeys` invoke no MSBuild; `_Wpf`/`Tools`/`_Statechart`
  build PackageReference-free projects).
- **Hygiene.** Added-files scan over all 50 commits: only docs, the intentional
  seed binaries, `global.json`/`nuget.config`/`Directory.Build.props`,
  `build.ps1/.sh`, AGENTS.md, CI yaml, the harness App.config. No logs, `.tmp`,
  nupkg/zip, or stray binaries. `.gitignore` covers `bin*/ obj*/ *.tmp .tmp/`
  (generated pack shim and nupkg outputs land under them). No secrets: keys are
  the pre-existing 1-byte `misc/keys/*.snk` stubs. `global.json` (10.0.402,
  latestFeature) and the zero-delta `Directory.Build.props` (single
  `TreatWarningsAsErrors=false`) are as documented.
- **AGENTS.md spot-check (≥6 claims).** Default target list, `-Targets`
  examples, single-test command mirroring `NemerleAll.nproj:425-427`,
  `NPrepareKeys` behavior, `boot-net10`/`boot-dnlib` history, pefverify
  retirement, `MSB4803 GetFrameworkSdkPath` blocker text, `Nemerle.Location/`,
  `VsIntegration/` absence, ns2.0 library TFMs — all match the tree. Only
  pre-existing nit: "5 assemblies" fixed-point phrasing (R1-05).
- **Deviations honesty.** 3.7 skip matches the tree (no `PublishAot` anywhere);
  the System.CodeDom 4.7.0 pin with rationale comments is present in both
  `Nemerle.Compiler.nproj:45` and `ncc-core.nproj:57-70`; the dnlib bump is a
  separate commit (42d1a188a). The process deviation (acceptance folded into
  implementation subagents) is consistent with the Journal's incident rules but
  is not externally verifiable from artifacts — recorded as honest-but-unverifiable.

## Explicitly not verified

- Actual reruns of the gates (per task constraints, no builds were run; all
  "green" claims are taken as logs-were-recorded, cross-checked only where the
  tree contradicts them — R1-01 flags where even that is a local-only proxy).
- GitHub runner image contents in 2026 (VS/MSBuild version, ildasm presence) —
  flagged speculation in R1-01.
- The 3.1e "risks not reproduced" checklist item (left open `[ ]` deliberately;
  agreed low-priority).
