# Progress 6.3 — CI: Stage1→Stage2→Stage3 + Validate (fixed point) on net10 + smoke `ncc-core.exe -version`

Sub-atom of the net10 migration plan (`retarget-compiler-to-net10-plan.md`
line 168): make the CI pipeline the standard green gate — full bootstrap chain
+ IL fixed point + net10 smoke, plus a CompilerTests job that is honest about
the 8 known-failing suite items until 6.4 resolves them. No commit.

## Audit (pre-implementation)

### What CI exists

`git ls-files` finds exactly one CI file: **`.github/workflows/build.yml`**
(GitHub Actions). It was restored/adapted in Phase 1.6, commit `f70d5aaa1`
("Restore CI workflow adapted to the net10 toolchain"). Its current stage
list, verbatim from the file:

1. `Checkout` (`actions/checkout@v4`, `submodules: false`)
2. `Setup .NET SDK` (`actions/setup-dotnet@v4`, `global-json-file: global.json`)
3. `Build (Stage1 + Stage2 self-host)` — `cmd /c Build-core-base.cmd Debug Build Stage2`
4. `CompilerTests` — `cmd /c Build-core-base.cmd Debug Build "Stage1,CompilerTests"`
5. `Validate (Stage3 + IL fixed-point)` — `cmd /c Build-core-base.cmd Debug Build "Stage3,Validate"`
   with `continue-on-error: true` ("peverify step is known-red until Phase 5.1
   removes it")
6. `Gate on Validate IL fixed-point` — pwsh log-gate re-asserting
   `FC: no differences` because of the step-5 `continue-on-error`.

Migration rot in it: steps 3–5 predate the Phase 5.2 `build.ps1` entry
(they call the legacy `Build-core-base.cmd` contract with the cmd.exe
comma-quoting trap) and the pefverify `continue-on-error` workaround is
obsolete since 5.1 retired pefverify (baseline doc: "peverify retired in
5.1 ... exit 0"). No VS2017 cache or Framework-only scripts remain (5.5
gated the IDE layer out; shipping IDE integration lives on `master`).

### `build.ps1` (canonical entry, 5.2) — parameter surface

`-Configuration` (Debug|Release, default Debug), `-Targets` (array or
','/';'-separated string; default Stage1;_PegAndCSharp;_CE;_Async;Install),
`-NTargetName` (Build|Rebuild), `-Rebuild` (shortcut), `-NInstall`,
`-Verbosity` (default m), `-MsBuildExe`, `-UseDotNetMsBuild`, `-DryRun`.
Resolves the host via `-MsBuildExe`/`NEMERLE_MSBUILD`/vswhere, sets
`MSBUILDENABLEALLPROPERTYFUNCTIONS=1`, passes comma target lists as ONE
argument natively, and **exits with the MSBuild exit code**. Restore needs no
step: `NemerleAll.nproj` runs `Restore` on its project lists itself (5.3's
repo-scoped `nuget.config`: `<clear/>` + nuget.org only, no machine feeds).

### Verified gates (`doc/NET10-BUILD-BASELINE.md`, re-verified after 5.2)

* `./build.ps1 -Targets Stage3,Validate -Rebuild` → **FC 5/5 identical, zero
  `: error`, exit 0** (this IS Stage1→2→3: `Stage3 DependsOnTargets="Stage2"`,
  `Stage2 DependsOnTargets="Stage1"`, nproj:273/:284; Validate fc-compares the
  fixed point for 5 assemblies).
* `./build.ps1 -Targets Stage1,CompilerTests` → same-msbuild-session
  requirement (CompilerTests depends on Stage1's `$(Nemerle)`).
* Current suite state after 6.2d: positives **453 passed / 8 failed / 7
  not-run(NO-CORE)**, negatives **167/0**. The 8 failures are exactly the 6.4
  scope: `Issue-git-0297.n` (fixture) + output-drift
  `Issue-git-0053, Issue-git-0274, assert, basic-value-types, linq-2-ExprTree,
  notnullorempty, overloading` (all `.n`, all `run...failed`).

### Harness known-failures/exclude audit → mechanism does NOT exist

* New harness (used by CompilerTests since 6.1):
  `snippets/Nemerle.Test/Nemerle.Compiler.Test/Main.n` options are exactly
  `-help -n -reference -macro -parameters -output -verifier -verifier-params
  -r -rp -v -vv -s -debugger -unix-colors -team-city-test-suite
  -visual-studio-test-suite -visual-studio-summary-result` + non-option test
  files. No exclude/skip/known-failures option.
* Exit code: `NccTestExecutionListener.GetExitCode()` = 1 iff `_failed > 0`
  (an `Exception` result also increments `_failed`); `Skipped` counts into
  `_notRun`, no exit-code effect (that is the NO-TEST/NO-CORE path).
* Legacy harness `testsuite/test.n` (`main()`): same story, no mechanism
  (it is not what CompilerTests runs; untouched, like in 6.2d).

⇒ **Option (i) chosen**: implementing a minimal allowlist mechanism is cheap
and keeps the gate binding. Design (host-agnostic, default-off):

* `Nemerle.Compiler.Test` learns `-known-failures <file>` (alias `-kf`):
  one test id per line (`#` comments and blank lines skipped); ids match the
  test's file name (`assert.n`) or its `<dir>/<file>` identity
  (`positive/assert.n`) case-insensitively, or the full path. A `Failed`
  result whose id is listed is reported in yellow as *known failure ignored*
  and counted separately; unknown failures and harness `Exception`s still
  increment `_failed` → exit 1. Entries that did not fail in a run are listed
  informationally (so 6.4 notices fixed tests). Without the option the code
  path is inert (`_knownFailures` empty) → master/legacy behavior unchanged;
  pure string matching, no host probe needed (works identically on FX, where
  the listed Core failures pass and the entries simply go unused).
* `NemerleAll.nproj` CompilerTests appends `-known-failures "$(NKnownFailures)"`
  to both Exec command lines **iff** the property is non-empty. MSBuild
  exposes environment variables as properties, so CI sets
  `$env:NKnownFailures = 'ci-known-failures-64.txt'` — no `build.ps1` change.
  Bare file name works because CompilerTests copies `testsuite\*.*` into
  `$(NBin)\Tests\{positive,negative}` (both harness cwds), so the allowlist at
  `testsuite\ci-known-failures-64.txt` is picked up by that copy.
* Allowlist content: the 8 ids above, all `positive/...` prefixed so the
  negatives run can never match them. TODO(6.4) removes the file.

### CI design (GitHub Actions schema kept: same runner, checkout/setup-dotnet conventions)

* Job `build`: checkout → setup-dotnet(global.json) →
  `./build.ps1 -Targets Stage3,Validate -Rebuild` (comment: this IS
  Stage1→2→3+Validate; binding again since 5.1, so `continue-on-error` is
  dropped) → simplified FC gate step (≥5 `FC: no differences`, no real diff —
  kept as belt-and-suspenders that the comparison actually ran) → smoke
  `bin\Debug\net-4.0\Stage3\ncc-core.exe -version` with no
  `DOTNET_ROLL_FORWARD` → zero-`-r` compile-smoke of a trivial `.n`
  (6.2b default-references UX).
* Job `tests`: checkout → setup-dotnet →
  `$env:NKnownFailures='ci-known-failures-64.txt'; ./build.ps1 -Targets
  Stage1,CompilerTests` — blocking; the 8 allowlisted failures do not redden
  it, anything else does. TODO(6.4) annotation on the step.
* No separate restore/cache step (5.3 nuget.config + nproj-internal Restore;
  MSBuild/nupkg caching left to runner defaults).

## Acceptance plan

* A. YAML parses (`ConvertFrom-Yaml` / python-yaml), command.
* B. Local proxy run, in CI order, logs under `.tmp\phase63\`:
  1. `./build.ps1 -Targets Stage3,Validate -Rebuild` → FC ×5 / 0 errors
      (`ci-validate.log`).
  2. smoke lines quoted with exit codes (`ci-smoke.log`).
  3. `$env:NKnownFailures='ci-known-failures-64.txt'; ./build.ps1 -Targets
      Stage1,CompilerTests` → exit 0 (`ci-tests.log`); plus direct-harness
      proofs: failing test allowlisted → exit 0, same test with an allowlist
      NOT naming it → exit 1 (new failures still redden; the suite itself is
      not mutated).
* C. `git status`: `.github/workflows/build.yml`,
  `testsuite/ci-known-failures-64.txt`, the two harness files, `NemerleAll.nproj`
  (allowlist plumbing), this doc. Nothing else. (`doc/plans/retarget-compiler-
  to-net10-checklist.md` was already modified before this atom — not touched.)

## Results

### A. YAML lint — PASS

`pwsh` has no `ConvertFrom-Yaml` on this host (`The term 'ConvertFrom-Yaml'
is not recognized`), so `python -c "import yaml; ..."` (pyyaml 6.0.3) parsed
the file: `python yaml OK: jobs = ['build', 'tests']`, build steps
`['Checkout', 'Setup .NET SDK', 'Build Stage1 -> Stage2 -> Stage3 + Validate
(IL fixed point)', 'Gate on Validate IL fixed-point', 'Smoke - ncc-core.exe
-version without roll-forward', 'Smoke - zero-r compile of a trivial .n']`,
tests steps `['Checkout', 'Setup .NET SDK', 'CompilerTests (Stage1 + suite; 8
known failures allowlisted until 6.4)']`. (Re-run after the smoke-step fix:
still OK.)

### B(1). CI build-job step 1 — `./build.ps1 -Targets Stage3,Validate -Rebuild` — PASS

Log `.tmp\phase63\ci-validate.log`: 106 lines, **5 × `FC: no differences
encountered`** (ncc-core, Nemerle.Compiler, Nemerle, Nemerle.Macros,
Nemerle.MSBuild.Tasks), **0 lines matching `: error`**, `Build succeeded`,
wrapper `EXITCODE=0`. This is the full CI-rebuild: boot→Stage1→Stage2→Stage3
+ the Validate IL comparison, all green with the harness/nproj/CI changes in
the tree (the fixed point held — harness code is not part of stage IL by
design).

### B(2). CI build-job smoke steps — PASS (`.tmp\phase63\ci-smoke.log`)

* `bin/Debug/net-4.0/Stage3/ncc-core.exe -version` with no
  `DOTNET_ROLL_FORWARD` in the environment:

  ```text
  Nemerle Compiler (ncc) version 10.0.40.0 (git)
  Copyright (c) University of Wroclaw 2003-2008, Nemerle Project Team 2008-2013
  All rights reserved.
  EXITCODE(version)=0
  ```

* zero-`-r` compile (6.2b UX): `.tmp\phase63\zero-r-smoke\smoke.n`
  (`using System; class M { static Main() : void { Console.WriteLine("zero-r
  smoke") } }`) compiled by Stage3 `ncc-core.exe` with NO flags at all →
  `EXITCODE(zero-r compile)=0`, `produced: out.exe, smoke.n` (ncc's default
  output name is `out.exe`), `zero-r compile smoke passed`, wrapper exit 0.
  Note: the first iteration failed (`unbound name 'Console'`) because the
  snippet lacked `using System;` — fixed in both the proxy and the CI step;
  no roll-forward/env tricks involved either way.

### B(3). CI tests job — PASS (`./build.ps1 -Targets Stage1,CompilerTests` with `$env:NKnownFailures='ci-known-failures-64.txt'`, log `.tmp\phase63\ci-tests.log`)

First two attempts FAILED honestly and the failures are recorded: the
`NKnownFailuresArg` property originally contained literal quotes
(`-known-failures "file"`, then `"-known-failures file"` as the element
text); the .NET Core MSBuild 18.11 `Exec` task turns an argument chunk
containing quote characters into ONE process argument, so the harness got
`-known-failures ci-known-failures-64.txt` glued into a single token and
rejected it (`unhandled command line argument: -known-failures ci-known-
failures-64.txt`, harness exit -1, MSB3073 — ci-tests.log of that attempt).
Fix (in scope, this atom): the property now emits a fully UNQUOTED
`-known-failures $(NKnownFailures)` — the exact proven pattern of
`-r dotnet` in the same Exec lines. `lib/getopt.n:207-218,268` confirms a
`CliOption.String` without `:` consumes the following token
(`need_following_arg`), and the CI value is a bare file name (no spaces;
CompilerTests copies `testsuite\*.*`, including the allowlist, into both
harness working directories). Paths with spaces work when invoking the
harness directly via the colon form (`option_name`/`argument_name` split at
the first `:`). Exec tokenization verified with a 30-second scratch project
reproducing the property+Exec quoting (`.tmp\phase63\scratch-exec-probe.nproj`):
harness printed its `Usage:` from `-help` with NO "unhandled command line
argument" line (before the fix that line appeared first).

A second real defect was found and fixed via targeted debug output (removed
afterwards): the search loop in `FindKnownFailure` had the short-circuit
guard inverted (`!found.IsNone` → never matched, allowlisted failures still
counted red); corrected to `when (found.IsNone && matchesId(id))`. Summary
messages were also tightened ("Known failures ignored: N"; the
`(0,0)`/"No tests to run" arm is guarded so a run consisting only of known
failures does not claim "No tests to run").

### Mechanism proofs (standalone `Nemerle.Compiler.Test.exe`, Tests\positive, single known-failing test `assert.n`, log `.tmp\phase63\ci-known-failures-proofs.log`)

* (a) DEFAULT-OFF: no `-known-failures` →
  `0 tests passed, 1 test failed.` / `EXITCODE(no allowlist)=1`.
* (b) allowlisted (space-separated, exactly as the nproj Exec passes it):
  `known failure ignored: positive/assert.n (allowlist 'ci-known-failures-64.txt'); not counted as a failure`
  → `Known failures ignored: 1 (...)` → `EXITCODE(allowlisted)=0`.
* (c) NEW-failure-still-red proof: allowlist not naming the failing test
  (throwaway `.tmp` file naming only the *passing* `positive/hello.n`; the
  testsuite itself untouched) → `0 tests passed, 1 test failed.` →
  `EXITCODE(unknown failure)=1`. Logic + observed behavior: only Failed
  results whose id is listed bypass `_failed`; `Result.Exception` results
  always count as failures; unknown ids always count.

Final run of B(3) (after both fixes above), verbatim from `ci-tests.log`:

* Positive run (468 test files): per-test lines `453 ... passed`,
  `7 ... not a test on core` (the 6.2d NO-CORE set), and exactly
  **8** `known failure ignored:` report lines:

  ```text
  known failure ignored: positive/Issue-git-0053.n (allowlist 'ci-known-failures-64.txt'); not counted as a failure
  known failure ignored: positive/Issue-git-0274.n (...)
  known failure ignored: positive/Issue-git-0297.n (...)
  known failure ignored: positive/assert.n (...)
  known failure ignored: positive/basic-value-types.n (...)
  known failure ignored: positive/linq-2-ExprTree.n (...)
  known failure ignored: positive/notnullorempty.n (...)
  known failure ignored: positive/overloading.n (...)
  ```

  summaries: `All tests passed (known failures ignored, see below). Time
  taken: 00:03:41.4781418` + `Known failures ignored: 8 (allowlist
  'ci-known-failures-64.txt'); they do not affect the exit code`
* Negative run: `All tests passed. Time taken: 00:01:04.6789446` — 167
  passed / 0 failed (the informational "entries that did not fail in this
  run" line lists all 8 positive ids → negatives matched none, as designed).
* `Build succeeded.` / wrapper `EXITCODE=0`; log has **0** `: error` /
  `MSB3073` lines. Positives 453+8+7 and negatives 167 exactly reproduce the
  6.2d "parity until 6.4" state (453+7nr / 167), now with CI exiting 0.

### C. Change scope — PASS (`git status --short` / `git diff --stat`)

```text
 M .github/workflows/build.yml                        (CI file, rewritten)
 M NemerleAll.nproj                                   (NKnownFailures plumbing only)
 M snippets/Nemerle.Test/Nemerle.Compiler.Test/Main.n (+20, -known-failures option)
 M snippets/Nemerle.Test/Nemerle.Compiler.Test/NccTestExecutionListener.n (+81, mechanism)
?? testsuite/ci-known-failures-64.txt                 (allowlist, 8 ids)
?? doc/plans/.../retarget-compiler-to-net10-plan-progress6.3.md (this doc)
```

`doc/plans/retarget-compiler-to-net10-checklist.md` was already modified in
the working tree before this atom (recorded in progress6.3's audit and in
6.2d §C) and was not touched here. No debug leftovers in the harness sources
(`git grep TODO-DEBUG` empty). No commits made.

### Deviations / notes

* Harness had TWO real defects caught only by the local proxy runs (quoted
  in B(3) above): the Exec quoting glue and the inverted search guard — the
  proxy-first strategy paid off exactly as intended.
* CI smoke `.n` needs `using System;` (Nemerle has no implicit usings);
  ncc's default output assembly is `out.exe` — both reflected in the yaml
  and proven locally (B.2).
* `TestFramework` target standalone needs `$env:NCurBin` (normally set by
  the stage targets); only relevant for manual harness prechecks.


