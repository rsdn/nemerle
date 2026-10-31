# Review #3 — independent critical review of the net10 retarget + remediation package

Reviewer: independent subagent #3. Scope: `retarget-compiler` @ `7109f5cf3`, range
`17d3cf47c..HEAD`, with a dedicated fidelity audit of the remediation atoms
(`faf2fef44`, `58338392d`, `da2b5cdee`, `9527eef14`, `1ea7eeb6f`) and the final
package docs. Method: static inspection + targeted runtime proofs (fc.exe
wildcard experiment, MSBuild property-scan order probe, build.sh OS-probe
simulation, gate-log greps); prior review reports consulted only after forming
independent views. No repo sources were modified; no commits made.

## Summary verdict

**PASS WITH FINDINGS.** The remediation package is substantively honest and
mechanically correct: every atom fixes what its finding claimed, three of the
fixes I re-proved independently from scratch (fc wildcard direction semantics,
the `_NugetPack` property-scan evaluation order/tie rule, the R2-01
type-precedence fold) confirm the remediation's claims — including the
disproof of review #2's R2-01. I found no blockers and no majors: the findings
below are two minors (stale coverage claim in `MASTER-CHERRY-PICKS.md`;
tracked machine-absolute paths in a live nproj the migration itself edited)
and a set of nits (AGENTS.md "FC ×5" not updated to the ×10 norm, a duplicate
8.3d checklist row, guard/scan edge fragments, build.sh message ordering).

## Adjudication of the R2-01 dispute (review #2 vs remediation)

**The remediation is correct: LAST-registered assembly wins duplicate-type
lookups. Review #2's "first-load-wins" is false, and its proposed reorder fix
would have introduced the very bug it reported.**

Evidence (current tree):

1. `ncc/backend/hierarchy/LibraryReferenceManager.n:476-481` (`LoadTypesFrom`)
   *prepends* each newly registered type entry to the node's cache list:
   `| NotLoaded(x) => NotLoadedList([e, x]);` and
   `| NotLoadedList(xs) => NotLoadedList(e :: xs);` — newest registration at
   the HEAD of `elems`.
2. `ncc/backend/NamespaceTree.Node.Hacks.n` (`LoadValue`) folds
   `val.elems.FoldLeft(...)`: the first entry per generic arity is constructed
   and kept (`by_parms_amount.Add(count, ...)`), every later same-arity entry
   is skipped with `using type [used] ... [e] (the first version was used)`.
   "first" = first *list entry* = last *registered* (prepend).
3. Registration order: the Core default framework set loads in
   `ManagerClass.LoadExternalLibraries` (`ManagerClass.n:575-601`), the
   explicit `-r` loop (`foreach (lib in Options.References)`, ~line 625) runs
   after; `LoadLibrariesContents` iterates `_referencedAssemblies` in
   registration order. Hence explicit `-r` entries end up at the fold's head
   and win.
4. Reinforcing path: the `| _ => NotLoaded(e)` branch of step 1 *replaces* an
   already-`Cached` node with the new entry wholesale — again last-registered
   wins.
5. Review #2's probe (report-review2.md R2-01: `-r <fwdir>\System.Console.dll
   -r System.Runtime.dll -r mscorlib.dll ...` → 5× duplicate warnings) is
   non-discriminating: it `-r`'d *the same files* the default set already
   loaded from the same directory, so both list entries name the identical
   assembly and the warning direction proves nothing. The "first-load-wins"
   conclusion was inferred from the warning phrase "(the first version was
   used)", whose "first" refers to the list head, not to load order.
6. The remediation's own shadow-assembly probe (distinct assembly carrying a
   new API, compiled successfully against a `-r` of it) is the discriminating
   experiment, and it is consistent with the code reading above.

The permanent comment added to `ManagerClass.n:591-598` states the mechanism
correctly (prepend + head-keeping fold + "do not reorder" + hard-error
guarantee for a missing `-r` file — verified: `LibraryReference.n` ctor uses
`File.ReadAllBytes`, and `tryEx` only swallows in IntelliSense mode).

## Findings

| ID | severity | area | finding + evidence | suggested disposition |
|---|---|---|---|---|
| R3-01 | minor | docs / MASTER-CHERRY-PICKS.md | Coverage claim is stale: `doc/MASTER-CHERRY-PICKS.md:9,83-86` says "**57 commits** in `17d3cf47c..HEAD` … **Every sha in the range is accounted for**", but the range is **58** commits already at the doc's own commit `1ea7eeb6f` (a moving-`HEAD` range can't include the commit that adds the doc) and **61** at `7109f5cf3`. Unaccounted: `1ea7eeb6f` itself, `e26affc54`, `7f08067ab`, `7109f5cf3` (all doc/checklist-only, so the substance is unaffected). The 11-candidate / 46-no-port classification I spot-verified is internally consistent and the f35161ae9 / e9741864a / 5a7dadbf1 rows match the real diffs (incl. the "no `PublicKey=` IVT in testsuite" claim — `git grep "PublicKey=" -- testsuite` is empty). | Pin the reviewed range to an end-sha (`17d3cf47c..1ea7eeb6f`) and add the 4 doc-only commits to the NO-PORT docs group, or re-run the count at close-out. |
| R3-02 | minor | repo hygiene / live files | Machine-absolute paths persist in a tracked, migration-touched project: `Nemerle.Location/Nemerle.Location.nproj:30-32` (`StartProgram=C:\RSDN\nemerle\bin\Debug\net-4.0\Stage1\ncc-core.exe`, `WorkingDirectory=c:\RSDN\nemerle\...`, `CmdArgs=C:\RSDN\nemerle\testsuite\...`) and the dead-but-tracked `Nemerle.Macros-dnlib.nproj:29-31`. The lines predate the range, but 7.1 (`996a1486b`) *edited the very StartProgram line* and kept the absolute path, and 7.4 (`00cee342b`) commits "Build config proven path-relative (grep for absolute machine paths in build inputs: empty)" — defensible only under a narrow "build inputs" reading. Not in the NemerleAll graph (no target references; CI unaffected), IDE F5 convenience only. | Replace with `$(MSBuildProjectDirectory)\...`-relative values (or `$(NRoot)`-style), or explicitly record these two files as accepted machine-specific debug leftovers. |
| R3-03 | nit | docs / AGENTS.md | `AGENTS.md:67` still describes the standard green gate as "IL fixed point (`FC` ×5 assemblies)" while `58338392d` moved the norm to FC ×10 (5 assemblies × 2 directions; `build.yml:62` gates `-lt 10`; checklist line 96 states "Норма гейтов: FC ×10"). BASELINE's dated "FC 5/5" entries are fine as history; AGENTS.md is the current-norm doc. | Reword to "5 assemblies × 2 directions (10 FC comparisons)". |
| R3-04 | nit | checklist | Duplicate 8.3d atom with contradictory status: `retarget-compiler-to-net10-checklist.md:100` `- [ ] 8.3d …` vs `:104` `- [✅] 8.3d …`. The remediation-commit cleanup (`e26affc54`/`7109f5cf3`) added the done row without deleting the planned one. | Delete line 100. |
| R3-05 | nit | build / _NugetPack (8.3g) | `NStageSelect` is never reset: after a standalone `_NugetPack` run, a *second* `_NugetPack` in the same MSBuild process finds `NStageSelect` still `'true'` and re-runs the scan, which can now silently overwrite the previously *selected* `NCurBin` (it is a target-set property, not global). Pathological-only (`-t "_NugetPack;%3C…"` repeats; composites never set it — probe-verified guard-off), and the new `packaging stage '…'` Message makes any such flip visible. | Optional: reset `<NStageSelect Condition=" '$(NCurBin)' != '' ">` handling or accept; the honesty Message is adequate mitigation as-is. |
| R3-06 | nit | build / 8.3a guard | The `Sdk.targets` import is conditioned on `Exists('$(Nemerle)\Nemerle.Sdk.props')` — a partial drop (props present, targets missing) still yields the raw MSB4019 the guard exists to prevent. Also the guard text could note that `boot-net10` lacking `Nemerle.Sdk.props` is *by design* (users pointed there by the new default are the primary audience). | Condition the targets import on its own path (or both), extend the message. Cosmetic. |
| R3-07 | nit | build.sh refusal path | On a genuine Linux/Darwin kernel *without* `NEMERLE_MSBUILD`, `resolve_msbuild` fails first and the user gets "no MSBuild host found… (exit 2)" instead of the deliberate OS-refusal paragraph; the documented refusal text is only reached when msbuild resolution succeeds (e.g. env set, then the `is_windows=0` gate at lines 164-171 fires). Both exit 2 without executing anything (verified by code path; direct execution on Linux not possible in this environment — my uname-shadow probe under git bash never overrode the real `uname`). Also: no CI job exercises the refusal at all. | Move the `is_windows` check before `resolve_msbuild` (or merge the messages); optionally add a `--dry-run`-style Linux CI smoke that only asserts the refusal. |
| R3-08 | nit | commit-message accuracy (8.3f) | `da2b5cdee`'s message describes the R2-01 probe as "warning names the framework copy as 'the first version was used' = shadow used" — under the actual template (`using type [used] … [e] (the first version was used)`), a shadow win means the warning names the *shadow* in the `used` slot and the framework copy as the skipped duplicate; the phrasing is self-contradictory as written. The empirical result (shadow's new API compiles) and the permanent `ManagerClass.n` comment are correct. | History is immutable; record the clarification here (done). |
| R3-09 | nit | Validate (8.3c) asymmetry | The reverse fc was added Windows-side only; the Unix branch (`NemerleAll.nproj:389`, `for il in $(NPrevBin)/*.il; do diff …`) remains single-direction and additionally depends on the same first-side iteration semantics it never had. Harmless (Validate is documented Windows-only, build.sh refuses non-Windows builds), but the "closed both blind spots" claim is Windows-true. | Either mirror the loop or comment the Unix branch as legacy single-direction. |
| R3-10 | info | review process / incident | During this review a build.sh probe (uname-shim that silently failed to shadow) accidentally launched a real incremental default-target build (MSBuild PID 35076), killed ~2 min later. Impact confined to incremental `bin\Debug\net-4.0\PowerPack\ComputationExpressions*` timestamps and the `boot\Nemerle.MSBuild.Tasks.dll` copy; Stage1/Stage2/Stage3 outputs and Install verified untouched (`ncc-core.dll` timestamps 8:09/4:17 preserved), `git status` clean, fully recoverable by any rebuild. Disclosed for the orchestrator's awareness (re-run the gate before the next promotion if paranoid). | No action; note it. |

## Per-remediation fidelity verdicts

- **8.3a (faf2fef44) — FIXED, complete.** All four standalone-build defaults now
  resolve to `$(MSBuildProjectDirectory)\boot-net10`; both silent `bin\boot`
  fallbacks removed; imports conditional; `GuardNemerleCompilerSeed`
  InitialTargets fails with resolved path + actionable message. Orchestration
  cannot misfire: stages pass `Nemerle=$(NBoot)` (or stage dirs), which contain
  `Nemerle.Sdk.props` produced by NTasks *before* the sub-project invocations
  (Stage1 `DependsOnTargets` order; clean-clone restore fix `00cee342b` and the
  green `Stage1,CompilerTests` gate with the guard in place are the empirical
  proof). No legitimate flow is broken — the only thing that "worked" via the
  old fallback was the stale-seed hazard the finding named. Residual nits:
  R3-06. Remaining `$(NemerleBinPathRoot)`/`$(ProgramFiles)` defaults in
  snippets/-dnlib nproj are out of the finding's scope (never pointed at
  boot-dnlib; not in the core graph).
- **8.3b (58338392d, comments/WiX/AGENTS) — OK.** `git grep boot-dnlib` across
  nproj/props/targets/ps1/sh/yml returns only the three deliberately historical
  comments in `NemerleAll.nproj`; WiX `CompilerAndTools.wxs` retains only a
  comment mentioning the removed ncc32/ncc64 payload; the AGENTS.md 10-file seed
  list matches `boot-net10/` exactly (10/10, names identical); the stale-`bin`
  Gotcha matches reality (neither `Install` nor `Stage*` is cleaned — I used
  that fact in the fc-trap analysis below). One doc line missed: R3-03.
- **8.3c (58338392d, bidirectional fc) — FIXED and independently re-proven.**
  My own scratch experiment reproduced the exact claimed semantics: `fc
  "prev\*.il" "cur\*.il"` iterates only the FIRST wildcard — a cur-only file is
  silently skipped (exit 0), a first-side-only file fails "FC: cannot open"
  (exit 2). The reverse Exec therefore genuinely closes the Stage3-only blind
  spot. Trap analysis ("would a 6th Stage3 assembly redden Validate
  permanently?"): **no — feature, not trap.** The compared `.il` sets are
  regenerated each run from directory *globs* (`Nemerle*.dll`/`ncc*.exe`), and
  the target deletes stale `*.il` in both dirs at entry and exit
  (`NemerleAll.nproj:336,395-396`), so a new assembly is picked up symmetrically
  (12 lines ≥ gate 10 → still green); a red reverse-fc means a genuine
  Stage2/Stage3 output asymmetry, which is the point. The `-lt 10` lower bound
  is robust to growth and only fires on *shrinking* the compared set — an
  acceptable tripwire (its message would then be mildly misleading).
  `phase83f\validate.log` confirms exactly 10 "FC: no differences" lines.
  Nit: R3-09.
- **8.3d (1ea7eeb6f, MASTER-CHERRY-PICKS.md) — FIXED with staleness (R3-01).**
  Substance verified: `f35161ae9` row matches the real commit (guard/split fix,
  `StrongNameKeyPair`→dnlib, `SHA1Managed`→`SHA1.Create`, strictly-permissive
  keyed-IVT behavior, FX re-implementation advice — master has no dnlib);
  `5a7dadbf1` partial-port advice is correct against its diff (InternalTypeClass
  reorder port / `CompilationOptions` PdbType hunk inert on FX);
  `e9741864a`'s reorder described accurately; the "no `PublicKey=` IVT in the
  testsuite" suite-neutrality claim verified empty. The mandatory FX-validation
  pre-condition (R1-03) is prominently stated.
- **8.3f (da2b5cdee) — FIXED (R2-02) and CORRECTLY REBUTTED (R2-01).** The
  catch-all now warns with dll name + exception type, sits after the native
  filter, and is confined to the `System.Private.CoreLib` gate (FX
  byte-identical). Gate-log evidence (`phase83f`): zero warning hits on the
  healthy framework, zero error lines, `All tests passed` ×2. The precedence
  comment is accurate (see adjudication above); nit R3-08 on the commit message.
- **8.3g (9527eef14) — FIXED, mechanism empirically sound.** I probed the exact
  property pattern with scratch MSBuild projects: within one target-level
  `PropertyGroup`, classic MSBuild evaluates strictly top-down (each
  condition/property-function line sees the previous line's value); strict `>`
  with a highest-number-first scan yields "strictly newer wins, exact tie keeps
  the higher-numbered stage" (probe1 → `chosen=Stage3` over a 4-stage mock);
  with `NStageSelect` unset (composites / explicit `-p:NCurBin=`) every
  comparison degenerates to `0 > 0` and `NCurBin` is never touched (probe2,
  run in a fresh process — note target-level properties *do* persist across
  targets in one process, which is the basis of nit R3-05). Both entry scripts
  export `MSBUILDENABLEALLPROPERTYFUNCTIONS=1`, so the `GetLastWriteTimeUtc`
  property functions resolve; the friendly no-stage `Error` is preserved and the
  chosen stage is printed.

## Package consistency — clean areas

- AGENTS.md command table matches `build.ps1` defaults (`Stage1;_PegAndCSharp;
  _ComputationExpressions;_Async;Install` at build.ps1:75) and the BASELINE's
  canonical commands; the 461+7nr/167 norm is stated consistently.
- `.gitignore` covers the new outputs: `[Bb]in*/` (line 1) subsumes
  `bin\<Config>\net-4.0\Nuget`, `.tmp/` (line 68) covers all scratch/log trees.
- The 7 `// NO-CORE` tests carry per-file reasons; all are genuine FX-only
  surfaces (CAS attributes ×2, System.Web, WinForms, BinaryFormatter, FX
  StreamReader metadata, in-box System.CodeDom) — no trivially fixable one under
  the zero-`-r` harness design (System.CodeDom exists on Core only as a
  non-in-box package, outside the shared-framework default scan).
- Harness runtimeconfig pin (`NccTest.n:223-233`): `"<Major>.0.0"` + default
  Minor roll-forward correctly admits any 10.0.x patch / 10.x runtime; no
  rollForward key needed.
- No machine-absolute paths in the live *build graph* (`git grep -i 'C:\RSDN'`
  over nproj/ps1/sh/yml/props/targets is clean except the R3-02 debug-launch
  properties); R1-01 (CI never run on GitHub) remains an honest open decision
  item (8.3e), not a doc-fiction.
- `build.yml` gate logic (errors→red, `<10` FC→red, MSB3073 comment) is
  consistent with the actual Validate Execs; both jobs invoke exactly the
  documented canonical commands.

## What everyone missed (beyond the findings above)

1. The 7.4 "absolute machine paths … empty" claim's narrow scope (R3-02) —
   nobody grepped `.nproj` debug-launch properties, which is precisely where the
   old repo's machine-specific habits hid, and the migration edited one of those
   exact lines.
2. `NStageSelect` process-lifetime stickiness (R3-05) — the guard's correctness
   argument implicitly assumes single-invocation; true for every shipped call
   path, but unstated.
3. The build.sh refusal-message ordering (R3-07) — the refusal is real, but on
   the most likely bare-Linux invocation the user reads the *wrong* explanation
   for it, and no test pins the refusal at all.
