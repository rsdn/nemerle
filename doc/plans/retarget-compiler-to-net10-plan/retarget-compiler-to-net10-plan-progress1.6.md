# Progress — Sub-point 1.6: Restore GitHub Actions CI workflow

## Intentions

1. Inspect the removed workflow (`git show 348d6f084:.github/workflows/build.yml`)
   and the build entry point `Build-core-base.cmd` for reference (read-only, no builds).
2. Create a new `.github/workflows/build.yml` for the `retarget-compiler` branch:
   - name `CI`; triggers: push to `retarget-compiler` + all `pull_request`;
   - single job on `windows-latest` (WS2025 + VS2026 image since May 2026 →
     MSBuild 18.x, satisfying the global.json sdk 10.0.402 requirement and
     providing vswhere which `Build-core-base.cmd` needs);
   - steps: checkout (no submodules) → setup-dotnet (global-json-file) →
     Stage2 self-host build → CompilerTests (same msbuild session as Stage1:
     `"Stage1,CompilerTests"`) → Validate with `continue-on-error` (its final
     pefverify step is known-red for an environmental reason until Phase 5.1) →
     a pwsh gate that re-asserts the IL fixed-point by scanning `validate.log`
     for `FC: no differences encountered` lines and any other `FC:` diff report.
3. Verify statically: YAML indentation, quoting of `"Stage1,CompilerTests"`,
   step count, regex correctness of the gate; do NOT build anything, do NOT commit.
4. Record a static self-review checklist against the six requirements here.

## Constraints honored

- No commits, no MSBuild/dotnet runs, no files touched except this progress file
  and the new workflow.

## Result

Created `.github/workflows/build.yml` (single job `build`, six steps). Nothing
else was touched; nothing was committed; no builds were run.

### Static self-review checklist (six requirements)

| # | Requirement | Status | Evidence |
|---|---|---|---|
| 1 | name `CI`; push → `retarget-compiler`; all `pull_request` | ✅ | Parsed YAML: `name: CI`, `on: {"push":{"branches":["retarget-compiler"]},"pull_request":null}` (null = all PRs) |
| 2 | Single job, `runs-on: windows-latest` | ✅ | One job `build`, `runs-on: windows-latest` |
| 3 | (a) `actions/checkout@v4` no submodules; (b) `actions/setup-dotnet@v4` with `global-json-file: global.json` | ✅ | Steps 1–2; `submodules: false` explicit |
| 4 | (c) `cmd /c Build-core-base.cmd Debug Build Stage2`; (d) `cmd /c Build-core-base.cmd Debug Build "Stage1,CompilerTests"` (quotes preserved, same msbuild session) | ✅ | Steps 3–4, `shell: cmd` |
| 5 | (e) `… "Stage3,Validate" > validate.log 2>&1` with `continue-on-error: true` + comment about pefverify known-red until Phase 5.1 | ✅ | Step 5; comment present above the step |
| 6 | (f) pwsh gate: exit 1 on `FC:.*(?<!no )differences` OR no `FC: no differences` line, else exit 0 | ✅ | Step 6; also fails on missing `validate.log` (⊂ "no FC line" case) |

### Static verification performed

- YAML validity: parsed with `js-yaml` (`npx --yes js-yaml`) — parses OK;
  structure dumped and audited (6 steps, correct triggers/runs-on/continue-on-error).
- Gate regex logic unit-checked in pwsh against synthetic logs:
  - 5×-style `FC: no differences encountered` only → PASS (exit 0);
  - log containing `FC: … differences reported` → FAIL-diffs (exit 1);
  - log with no `FC:` lines at all → FAIL-nofc (exit 1);
  - `FC: 3 differences found` → FAIL-diffs (exit 1).
  The negative lookbehind `(?<!no )` correctly does not match "no differences".
- `Build-core-base.cmd` re-read (read-only): vswhere-located MSBuild, target is
  parameter 3, exit code propagated — matches the invocation style used in the workflow.
- `git status`: only `.github/workflows/build.yml` and this progress directory added
  by this task; other working-tree modifications are pre-existing (concurrent build work).

### Notes for the orchestrator

- Gate tolerates all 5 expected `FC: no differences encountered` lines; any real
  FC diff report line (or a Validate run that died before the FC comparisons)
  fails the job, keeping the IL fixed-point assertion binding despite step (e).
- `pull_request:` with no filters covers all PR events per spec.
- No artifact upload step was added to keep the file at exactly six steps; if the
  Validate log should be downloadable on failure, an `upload-artifact` step with
  `if: always()` can be added later (would make it seven steps — intentionally out
  of scope per the deliverable).
