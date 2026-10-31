# Critical review #2 — net10 retarget (`retarget-compiler`, commits `17d3cf47c..305118d30`)

Reviewer: subagent #2 (task 8.2). Date: 2026-10-31. Independent of review #1;
its report was read only after all probes below were executed. Deliberately
different attack angles: host-gate semantics, bootstrap-source fixes, test
harness honesty (reproduced live), packaging repeatability, config drift, git
hygiene. Runtime evidence used sparingly and sequentially; all scratch output
in `.tmp\phase82\`. No files modified except this report.

## Summary verdict

**PASS WITH FINDINGS.** No blocker and no bootstrap-integrity or
test-honesty defect found. The negatives suite reproduces standalone (167/0,
exit 0); no BEGIN-OUTPUT-CORE block is weaker than its FX twin; the host probe
is correct under apphost, `dotnet exec` and (provably) .NET Framework; the
migration's compiler diff is small (9 files), host-gated, and contains no
`lib/` public-behavior change. The new findings are all **minor/nit**: gaps
around the 6.2b default-reference set (no dedup with explicit `-r`, and the
implicit active-runtime set wins over user references), a stale-stage hazard in
standalone `_NugetPack`, and doc/config trade-off notes. Review #1's findings
survive critique essentially unchanged; R1-05 is strengthened with hard `fc.exe`
evidence, R1-06 is already fixed.

## Method / probes executed (evidence base)

- Read plan, checklist (statuses, Journal, Deviations), every host-gate call
  site (`CompilationOptions.n:40/73`, `ManagerClass.n:576`, `macros/Resource.n:67`,
  `NccTest.n:229/247`, `NccTestDescription.n:99`), Validate/_NugetPack targets.
- Probes on `Stage3\ncc-core.exe` (the installed `bin\Debug\net-4.0` copy is a
  stale pre-6.2b artifact — see R2-08 note): zero-`-r` compile exit 0; `-r`
  duplicating default-set framework dlls → duplicate-type warnings, first load
  wins; `dotnet exec ncc-core.dll` host → gate behaves identically (exit 0);
  default-set load cost measured (~0.8 s overhead, no perf concern).
- Negatives suite run standalone per AGENTS.md recipe: **167 passed, exit 0**
  (`.tmp\phase82\negatives-standalone.log`).
- Exact line-by-line FX-vs-CORE block extraction for all 7
  BEGIN-OUTPUT-CORE files; NO-CORE marker audit; `regexp match` arm-shadowing
  analysis (full-match anchoring ⇒ `^BEGIN-OUTPUT` cannot eat `BEGIN-OUTPUT-CORE`).
- `fc.exe` wildcard-semantics experiment (Validate gate direction).
- Full-range `git diff --name-status` (270 files) artifact scan; `git ls-files
  boot-net10` vs AGENTS.md; `git grep boot-dnlib -- ':!doc'`; remote refs check;
  build-config diff for secrets/machine paths (clean).

## Findings

| ID | Sev | Area | Finding (evidence) | Disposition |
|----|-----|------|--------------------|-------------|
| R2-01 | minor | 6.2b default refs × explicit `-r` | **No dedup between the 167-dll default set and explicit `-r`, and the implicit set wins.** `LibraryReferenceManager.AddLibrary` (`:70-74`) appends unconditionally (list + ModuleDef-keyed dict; re-loading the same file yields a second module). Defaults load at `ManagerClass.n:590-595`, user `-r` only at `:607`; duplicate-type policy is first-registered-wins. Probe: `hello.n -r <fwdir>\System.Console.dll -r System.Runtime.dll -r mscorlib.dll -r netstandard.dll` → 5× `warning: using type ... defined in more than one assembly ... (the first version was used)`. Consequence on a Core host: a user `-r` pointing at a *different version* of any assembly in the shared-framework dir (System.Text.Json, System.Diagnostics.DiagnosticSource, System.Collections.Immutable, …) is shadowed by the active-runtime copy — a much larger collision surface than FX's 4-assembly default. Suite does not exercise the overlap (zero-`-r` tests only). | Add an 8.3 item: dedup `AddLibrary` by normalized path/identity, or load user references first / prefer `isUserReference` on collision (note: `IsUserReference` is stored but used nowhere for precedence). `-no-stdlib` remains the workaround. |
| R2-02 | minor | 6.2b hermeticity / error hiding | Default set = **active shared-framework (implementation) directory**, not a targeting/ref pack: the zero-`-r` reference surface silently drifts with installed runtime patches (observed: 10.0.13's 164-file dir), unlike Roslyn which resolves the ref pack. Additionally each `AddLibrary` failure is swallowed (`catch { _ => () }`, `ManagerClass.n:594-595`) *and* an enumeration failure of `framework_dir` degrades to zero default refs, turning a broken host into a flood of `unbound name` errors with no diagnostic pointing at the cause. (6.2a documented the choice; the residual drift/swallow aspects were not flagged.) | Record as a known limitation (suggest doc note in AGENTS/testsuite README); consider one aggregated warning if the dir is missing or >50% loads fail. |
| R2-03 | minor | 6.5 `_NugetPack` repeatability | Standalone fallback (`NemerleAll.nproj:617-620`) picks the **highest existing numbered** stage (`Stage4`→`Stage1` `Exists` checks), while its comment claims "package the *newest* built stage". Scenario: fresh `Stage1` rebuild, stale `Stage3/4` dirs on disk, then `-Targets _NugetPack` standalone → silently packs the stale Stage4 payload. The version is *consistent* with the packed binaries (`GetAssemblyIdentity` reads `$(NCurBin)\Nemerle.dll` itself, `:630-634`, and the high-importance message prints `NCurBin`), so no mis-versioning — but "packing from a stale stage" is real and undocumented. Shim itself is deterministic (static `WriteLinesToFile` content, output under `$(NObj)\Nuget`). | Re-word the comment to "highest-numbered stage" or select by `LastWriteTime`; alternatively `Error` unless exactly one stage exists. |
| R2-04 | nit | host-gate duplication | The host probe is inlined **8 times** across 5 files (`typeof(...).Assembly.FullName.StartsWith("System.Private.CoreLib")`); only `CompilationOptions.IsRunningOnNetCore` is named, the others say "mirrors" in comments. Verified all copies are currently identical and the probe is sound (apphost *and* `dotnet exec` both load System.Private.CoreLib; FX cannot be fooled by oddly loaded System.Runtime because the probe is `typeof(object/int).Assembly`, always mscorlib there; Mono → FX branch by design, documented). Risk is future drift of one copy. | Optional: one shared helper in `ncc/shared` (compiler side); harness duplication is justified by master-compat. |
| R2-05 | nit | docs | AGENTS.md's `boot-net10` parenthetical lists only `ncc-core.exe/.dll + runtimeconfig/deps + dnlib.dll + System.CodeDom.dll`; the actual seed is 10 files (`git ls-files boot-net10`), omitting `Nemerle.dll`, `Nemerle.Compiler.dll`, `Nemerle.Macros.dll`, `Nemerle.MSBuild.Tasks.dll` — i.e. exactly the libraries a seed-reader must not forget when regenerating a seed. | Fold the complete list (per progress4.2) into AGENTS.md. |
| R2-06 | info | robustness, pre-existing | `-no-stdlib` without any corlib reference (and the `-r` reorder heuristic `ManagerClass.n:544-554` happily fronting a facade like `System.Runtime.dll`) ends in `Process terminated. Assertion failed. ICE! internal type System.Void not found`. Reproduced; same `Util.ice` path exists at baseline (`ManagerClass.n:725` @17d3cf47c) ⇒ upstream, not a migration regression. Noted because `-no-stdlib` is now the standard stage-build mode and the new facade fronting makes the misconfiguration easier to hit; a clean `error:` instead of an ICE would be a cheap upstream win. | Optional upstream polish; no action in this plan. |
| R2-07 | info | 5.3 nuget.config | `<clear/>` + nuget.org-only is a deliberate reproducibility trade-off (verified isolated restore, 5.3), but on enterprise/offline machines that block nuget.org it hard-blocks the only restore in the graph (Microsoft.Build.* 17.0.0 for `Nemerle.MSBuild.Tasks.csproj`) with no documented override. | One-line doc: how to point this repo at an internal feed (user-level config is cleared by design). |
| R2-08 | info | working tree | `bin\Debug\net-4.0\ncc-core.exe` (Install output) predates 6.2b — zero-`-r` compile fails with it while `Stage3` passes. Not a repo defect (bin/ is ignored), but AGENTS.md presents the Install copy as "the working compiler"; anyone reusing an old `bin` gets misleading results. Reviewer discipline: verify freshness (or rebuild Install) before concluding. | None / note in AGENTS "Day-to-day" row. |

## Clean areas (checked, one line each)

- **Git hygiene**: 270 changed files `17d3cf47c..HEAD`; no bin//obj//.tmp//logs/nupkg/zip; added files are exactly docs + seed + 6 config/CI/build files + harness App.config; no secrets or machine paths in build configs (grep `C:\RSDN|F:\RSDN|user|token`).
- **boot-dnlib residue**: outside `doc/`, references are only historical prose (AGENTS.md:97, README.md:97, NemerleAll.nproj:26 comment) plus the **functional** stale defaults already covered by R1-02/8.3a; `NRootBoot=$(NRoot)\boot-net10` confirmed (`NemerleAll.nproj:27`).
- **Negatives suite**: reproduced standalone, 167/0, exit 0 (matches the documented norm).
- **BEGIN-OUTPUT-CORE honesty**: 7 paired files; exact extraction shows no weaker Core block — 5 identical line counts, `assert.n` 16→15 and `notnullorempty.n` 7→5 differ only by the two-line FX "Parameter name: x" being folded inline as "(Parameter 'x')" (semantically equal assertions).
- **NO-CORE marker**: exactly the 7 documented FX-only-surface tests; `.*NO-CORE.*` cannot fire on block content (block lines never reach the directive match chain); full-match anchoring prevents `^BEGIN-OUTPUT`/`^END-OUTPUT` from shadowing the CORE markers; master-safety argument independently re-derived.
- **Bootstrap-source fixes**: `lib/` has zero changed files in range (no public stdlib behavior change); the only shared-surface edits are the two InitSystemTypes order-hardening moves (which *move asserts earlier in dependency order*, keeping assert strength — mitigation, not masking) and the FNV-1a name fix (removes per-process nondeterminism; the FC ×5 across distinct processes is the guard against residual hash-order leaks; remaining `Util.tmpname` sites are order-counter-based and proven stage-stable by that same gate).
- **Host gates behavior**: identical results apphost vs `dotnet exec`; FX path byte-identical by construction (gate false); stages never hit the 6.2b loop (`NoStdLib` → `Nemerle.MSBuild.targets:374` → `-no-stdlib`, loop lives in the `else` branch).
- **Config drift**: `global.json` `10.0.402`+`latestFeature` rolls forward to newer patches/feature bands ≥ 4 within 10.0 (a machine with only 10.0.1xx-3xx fails — the intended pin, and the MSBuild ≥ 18 consequence is a recorded deviation); `Directory.Build.props` is truly zero-delta (single `TreatWarningsAsErrors=false`); CI pins via `global-json-file`.
- **Validate mechanics beyond R1-05**: ildasm runs on *both* stage dirs (`:355-356`), prev-only files correctly fail (fc exit 2 reproduced); duplicated FileUpdate invocations (`:361-366`) are pre-existing (7 occurrences at baseline too); keeping `MSBuild.Community.Tasks.dll` for FileUpdate×6 is the documented 5.4 decision.

## Critique of review #1

| ID | Verdict | One-line evidence / adjustment |
|----|---------|--------------------------------|
| R1-01 | **Agree** (major stands) | `git rev-parse github/retarget-compiler kl/retarget-compiler` = `d9b50f721` (the pre-net10 baseline tag commit); zero migration commits pushed ⇒ CI never ran; runner-image assumptions genuinely untested. |
| R1-02 | **Agree** | Re-grepped: all four `.nproj:3` defaults point at the deleted `boot-dnlib`; the line-5 silent fallback to `bin\Debug\net-4.0\boot` confirmed verbatim in 3 of them. Correctly weighted as minor (orchestrator unaffected). |
| R1-03 | **Agree** | All gates in range are Core-host gates (FC ×5 + Core suite); no FX run exists in any log — cherry-pick precondition is warranted. |
| R1-04 | **Agree** | Matches my read of `f35161ae9`: the branch was doubly dead before, now live on both hosts; "strictly permissive" characterization is right. |
| R1-05 | **Agree, upgraded evidence** | Reproduced: `fc.exe "prev\*.il" "cur\*.il"` with a cur-only file prints only the paired comparison and **exits 0** (silently unchecked); a prev-only file exits 2. Tautological `Nemerle.MSBuild.Tasks.dll` pair confirmed via the identical `@(NTasksFiles)` copies (`:281/292/303/314`). Real self-compiled coverage is ×4. |
| R1-06 | **Agree, now closed** | Fixed by 305118d30 itself: checklist 5.2-5.5 are `[✅]` and 8.3's norm reads 461+7nr/167 today. Historical finding; no residual work. |
| R1-07 | **Agree** | `NemerleAll.nproj:26` "boot-dnlib stays on disk … until 4.5 closes" is false since 00cee342b; tracked in 8.3b. |
| R1-08 | **Accept (not independently re-checked)** | WiX payload claim plausible and consistent with 7.1's file list; low stakes under the 5.5 guard. |
| R1-09 | **Accept (spot-checked)** | No `RemoveDir $(NBoot)` in `NPrepareBoot` (the only RemoveDir near there is `_NugetPackCore`'s Nuget output); mixed-seed hazard reasoning is sound, severity nit is right. |
| R1-10 | **Agree** | Packaging clean-up claims match `61a4592ae`; XML-doc drop is a fair consumer-visible trade-off. |
| R1-11 | **Agree** | I re-derived the block equivalences independently (exact line extraction, all 7 files) — same conclusion: real drift, no weakening; masking concern is speculative but honestly flagged. |
| R1-12 | **Agree** | Empty-allowlist path is what CI exercises; non-empty path has local-only evidence. Nit. |

**Missed by review #1** (added above): R2-01 (default-vs-user `-r` precedence/dedup — review #1 verified the 6.2b loop is *skipped* by stages but never probed the user overlap), R2-02 (active-runtime drift + swallowed scan failure), R2-03 (stale-stage `_NugetPack` fallback vs its "newest" comment), R2-05 (AGENTS seed file list incomplete), R2-07 (enterprise/offline restore trade-off undocumented). No review-#1 finding is refuted; none is over-weighted; R1-06 is under-weighted only in the sense that it is already remediated.

## Explicitly not verified

- Positives suite not re-run standalone (cost; negatives + probes deemed sufficient evidence of harness health).
- GitHub-hosted CI execution (impossible without a push — that *is* R1-01).
- WiX/vssdk submodule internals (out of the Core graph per 5.5).
