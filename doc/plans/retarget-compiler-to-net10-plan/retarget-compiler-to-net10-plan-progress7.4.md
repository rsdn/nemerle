# Progress 7.4 — full cycle from a CLEAN CLONE (closes plan item 4.5)

Branch `retarget-compiler`. Plan: 7.4 <полный цикл с чистого клона> — prove
`boot-net10` seed + `build.ps1` reproduce the fixed point and the test suite
in a fresh checkout with zero dependence on local working-tree state. That
satisfies the documented condition to retire the emergency seed `boot-dnlib/`
(checklist/plan 4.5, `AGENTS.md` Bootstrapping). Do NOT commit — orchestrator
commits.

## Plan

1. `git clone C:\RSDN\nemerle C:\RSDN\nemerle-clean{,2,3}` (submodules not
   needed — installer-only per AGENTS.md gotcha); HEAD must equal the source
   repo HEAD.
2. PRE-CHECK: no absolute paths to the original checkout in the build config
   (`git grep -nE "C:[\\]{1,2}RSDN" -- build.ps1 NemerleAll.nproj nuget.config
   global.json Directory.Build.props`) → expected empty.
3. Cycle 1 (acceptance gate): `./build.ps1 -Targets Stage3,Validate -Rebuild`
   → FC ×5, zero `: error`, exit 0.
4. Cycle 2: `./build.ps1 -Targets Stage1,CompilerTests` → positives
   `All tests passed.` (461 + 7 not-run) + negatives `All tests passed.`
   (167) + exit 0.
5. Smoke: Stage3 `ncc-core.exe -version` exit 0; zero-`-r` compile of a
   trivial `.n` exit 0.
6. Failures: diagnose, fix in the ORIGINAL working tree, re-clone fresh,
   rerun from the failed step on.
7. Green: `git rm -r boot-dnlib` + AGENTS.md reword + this doc.

## Experiment setup

* Source HEAD = cloned HEAD = `53a9f5ba0244fa544886d28a5e82830f61472e36`
  (`retarget-compiler`), source working tree clean.
* Step-2 grep: empty (exit 1) in every clone — build config is fully relative
  / `$(MSBuildProjectDirectory)`-based.
* Logs: `.tmp\phase74\clean-validate.log`, `.tmp\phase74\clean-tests.log`;
  scratch repros: `bt-repro*` (Temp), `ntasks-*.log`, `diag-*.log` (`.tmp`).

## Defect #1 (iteration 1, clone `emerle-clean`) — no restore path on a clean clone

`Stage3,Validate -Rebuild` died at NTasks:

    error NETSDK1004: Assets file '...\obj\project.assets.json' not found.

`Nemerle.MSBuild.Tasks.csproj` is the only PackageReference project without
restore coverage (Stage1 restores `@(NCompilerProject)` itself; the other
`.nproj`s have no PackageReferences). Its self-heal hook
`EnsurePackagesRestored` (`BeforeTargets="Build;Rebuild"`) is DEAD CODE: with
no assets file the engine aborts in `ResolvePackageAssets` before any
BeforeTargets hook is woven — proven on MSBuild 18 + SDK 10 with a minimal
SDK-style repro (hooks for `Build`, `CoreBuild` and `ResolveReferences` all
absent from a `/v:diag` graph; an explicit `/t:EnsurePackagesRestored` works).
This was the known-open "doesn't-fire-under-NTasks" item of progress2.3a/2.3c
— masked in the original checkout because its untracked `obj\` was already
restored. A classic hidden local-state dependency: exactly what 7.4 exists to
catch.

**Fix (v1):** `NemerleAll.nproj` `NTasks` — issue `Targets="Restore"` on
`@(NTasksProject)` before the build invocation (the XBuild variant has no
PackageReferences; its Restore is a no-op).

## Defect #2 (iteration 2, clone `emerle-clean2`) — stale cached evaluation

With v1, restore ran ("Restored ... (in 448 ms)", assets complete) but the
build then died with a `CS0012: ... 'netstandard, Version=2.0.0.0' is not
referenced` / `CS0234 System.Text.RegularExpressions` storm. Repro facts: the
identical inner Rebuild SUCCEEDED standalone afterwards; only the Restore →
build pair inside one MSBuild process failed. Classic msbuild#2811:
`<MSBuild Targets="Restore">` + build with IDENTICAL global properties reuse
the pre-restore cached project evaluation — which never imported the
then-absent `obj\*.nuget.g.props`, so package/netstandard refs were lost.
The repo's own stage targets had long ago worked around this with a `_dummy`
property (see the `<!-- _dummy value is required ... msbuild/issues/2811 -->`
comment above Stage1's Restore); `NTasks` never got the treatment.

**Fix (v2):** `NTasks` pair now passes `_dummy=1` on the Restore call and
`_dummy=2` on the build call, exactly mirroring the Stage1 pattern. Unit-
verified: wiped `obj\`+`bin\`, `msbuild NemerleAll.nproj -t:NTasks
-p:NTargetName=Rebuild` → exit 0, `boot\Nemerle.MSBuild.Tasks.dll` produced.

Also: removed the dead `EnsurePackagesRestored` target from the csproj
(replaced by a comment recording the behavior + the manual recipe
`-t:Restore` first) and updated the `build.ps1` restore comment to list
NTasks.

## Results (iteration 3, clone `emerle-clean3` = HEAD + the 3 fix files)

### Cycle 1 — `./build.ps1 -Targets Stage3,Validate -Rebuild` — GREEN

    Comparing files ...\STAGE2\ncc-core.dll.il and ...\STAGE3\ncc-core.dll.IL
      FC: no differences encountered
    (... same for Nemerle.Compiler.dll, Nemerle.dll, Nemerle.Macros.dll,
         Nemerle.MSBuild.Tasks.dll — FC ×5)
    Build succeeded. Nemerle installed to 'C:\RSDN\nemerle-clean3\bin\Debug\net-4.0' ...
    EXITCODE=0

Zero `: error` lines in `.tmp\phase74\clean-validate.log`. The `boot-net10`
seed ALONE reproduces the fixed point from a clean checkout — the 4.5
criterion is met.

### Cycle 2 — `./build.ps1 -Targets Stage1,CompilerTests` — GREEN

From the same clean clone (no re-clone; the cycle appends fresh Stage1):

    All 15 tests passed.                     (DiffModule self-test)
    All tests are successfuly passed.        (TestFramework stage)
    All tests passed. Time taken: 00:03:36.8437633   (positives: 461 + 7 NO-CORE not-run)
    All tests passed. Time taken: 00:01:02.8812885   (negatives: 167)
    Build succeeded. ...
    EXITCODE=0

Zero `: error` lines in `.tmp\phase74\clean-tests.log`.

### Smoke — GREEN

In the clone, from the Validate-proven Stage3 output:

    bin\Debug\net-4.0\Stage3\ncc-core.exe -version
      -> Nemerle Compiler (ncc) version 10.0.49.0 (git)      exit 0
    ncc-core.exe .tmp-smoke\Smoke74.n -out:.tmp-smoke\Smoke74.exe   (zero -r)
      exit 0                                                 (default refs from the compiler, 6.2b)

Bonus runtime proof (not part of the criterion): with a hand-written
`Smoke74.runtimeconfig.json` next to the output (plain ncc emits none, as in
6.2c), `dotnet exec Smoke74.exe` printed `smoke74-ok`, exit 0.

## Finalization (original repo, uncommitted)

1. `git rm -r boot-dnlib` (103 tracked files) — emergency fallback retired per
   its documented condition, now met above. (Disk copies survive: git history
   + the 0.2 backup in `.tmp\`.)
2. `AGENTS.md` Bootstrapping: boot-dnlib bullet reworded to historical.
   `git grep -n boot-dnlib -- AGENTS.md doc/NET10-BUILD-BASELINE.md` → only
   the reworded bullet remains (baseline doc never mentioned it).
3. `NemerleAll.nproj` / `Nemerle.MSBuild.Tasks.csproj` / `build.ps1`: the
   defect #1+#2 fix (described above).
4. This progress doc.
