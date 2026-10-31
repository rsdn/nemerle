# Progress 8.3a — stale seed defaults in core .nproj files (R1-02)

Status: DONE (gates A/B green; see §Results)
Branch: retarget-compiler
Scope: `Nemerle.nproj`, `Nemerle.Compiler.nproj`, `Nemerle.Macros.nproj`,
`ncc-core.nproj` + this doc. No commit (per task instruction).

## Finding (review1 R1-02, confirmed by review2)

After `boot-dnlib/` was deleted (00cee342b, checklist 4.5/7.4), the standalone
default `<Nemerle Condition=" '$(Nemerle)' == '' ">$(MSBuildProjectDirectory)\boot-dnlib</Nemerle>`
(line 3) in all four core projects pointed at a deleted directory. Three of
them (Nemerle, Nemerle.Compiler, Nemerle.Macros) then carried a line-5 fallback
that silently re-pointed `Nemerle=` at `bin\Debug\net-4.0\boot` — a copy dir
that can hold a stale seed from before a reseed (silent stale consumption,
Debug-hardcoded). `ncc-core.nproj` had no fallback at all (confusing
"boot-dnlib\Nemerle.Sdk.props not found" import error). The orchestrator is
unaffected: NemerleAll.nproj always passes `Nemerle=$(NPrevBin)`/`$(NCurBin)`.

## Audit (git grep -n "boot-dnlib\|net-4.0\\boot" -- "*.nproj" "*.csproj" "*.props" build.ps1)

LIVE occurrences (all fixed here):
- `Nemerle.nproj:3` (default), `Nemerle.nproj:5` (silent fallback)
- `Nemerle.Compiler.nproj:3` (default), `:5` (silent fallback)
- `Nemerle.Macros.nproj:3` (default), `:5` (silent fallback)
- `ncc-core.nproj:3` (default only; no fallback existed)

Non-live / out of scope (NOT touched):
- `NemerleAll.nproj:26,241,250` — comments only (the :26 stale comment is
  explicitly tracked as checklist 8.3b / R1-07; NRootBoot/NPrepareBoot logic
  itself is correct and untouchable per task).
- `AGENTS.md`, `README.md`, `doc/plans/**` — documentation/history.
- No `.csproj`/`.props`/`build.ps1`/`.github` occurrence; CI and build.ps1
  drive only `NemerleAll.nproj`, no standalone build of the 4 projects exists.

## Fix

In each of the 4 projects:
1. Line-3 default re-pointed to the current seed, same path style:
   `<Nemerle Condition=" '$(Nemerle)' == '' ">$(MSBuildProjectDirectory)\boot-net10</Nemerle>`.
   All four nproj live at the repo root, so `$(MSBuildProjectDirectory)` is
   project-location-anchored (machine-relative-free) — equivalent to
   `$(MSBuildThisFileDirectory)boot-net10`; the old fallback's
   `$(MSBuildThisFileDirectory)bin\Debug\net-4.0\boot` fallback is deleted.
2. Silent fallback property (line 5) and its comment (line 4) deleted in the
   three library projects.
3. A standalone-build guard added in ALL FOUR projects (consistent, also fixes
   ncc-core.nproj's "confusing breakage" leg): `<Project
   InitialTargets="GuardNemerleCompilerSeed">`, both SDK imports become
   condition-skipped when the seed lacks the files
   (`Condition="Exists('$(Nemerle)\Nemerle.Sdk.props')"`), and a trailing
   `GuardNemerleCompilerSeed` target raises the `<Error>` with the resolved
   path and an actionable message. (An out-of-target `<Error>` as the first
   attempt is rejected by modern MSBuild — "no longer supported as a child of
   the <Project> element" — hence the InitialTargets idiom.)

Choice documented: the `boot-net10` seed is binary-only by design (4.2) —
`Nemerle.Sdk.props` is *generated* by the NTasks stage into `$(NBoot)` and the
stage/install drops; it never lives in `boot-net10\` itself. So a standalone
default can never import successfully from the raw seed, and after removing
the fallback the only outcomes are (a) user passed/points at a real compiler
drop → fine (guard condition false), (b) otherwise → the guard error names
`$(Nemerle)` and tells the user to build via build.ps1 or pass
`/p:Nemerle=<dir with Nemerle.Sdk.props>` (e.g. `bin\Debug\net-4.0`).
The guard predicate is exactly the import's own success condition, so every
orchestrator flow (which always passes a drop containing `Nemerle.Sdk.props`
— confirmed: the import works in Stage1..4 today) skips both the guard and
the conditions; zero behavior change for the build graph. Verified by probe:
standalone default on all 4 projects → the new error; with
`/p:Nemerle=bin\Debug\net-4.0` the guard passes evaluation.

## Verification

- A. `./build.ps1 -Targets Stage3,Validate -Rebuild` → log `.tmp\phase83a\validate.log`
- B. `./build.ps1 -Targets Stage1,CompilerTests` → log `.tmp\phase83a\tests.log`
- C. grep: no live `boot-dnlib` reference; default resolves to an existing dir.
- D. `git diff --stat`: only the 4 nproj + this doc.

(Evidence appended after green gates.)

## Results — DONE

A. `./build.ps1 -Targets Stage3,Validate -Rebuild` → `.tmp\phase83a\validate.log`:
   5× `FC: no differences encountered` (ncc-core.dll, Nemerle.Compiler.dll,
   Nemerle.dll, Nemerle.Macros.dll, Nemerle.MSBuild.Tasks.dll), zero `: error`
   lines (git grep -c), `Build succeeded.`, `EXITCODE=0`.

B. `./build.ps1 -Targets Stage1,CompilerTests` → `.tmp\phase83a\tests.log`:
   positives `All tests passed. Time taken: 00:03:37.5384045` (461 result
   lines `...passed` + 7 `not a test on core`), negatives `All tests passed.
   Time taken: 00:01:03.6979507` (167 passed), `Build succeeded.`,
   `EXITCODE=0`. Matches the current Core norm 461+7nr/167/0.

C. Standalone proof: `git grep -n "boot-dnlib" -- "*.nproj" "*.csproj"
   "*.props" "*.targets" build.ps1 build.sh .github` → only the three
   comment-only mentions in NemerleAll.nproj (:26/:241/:250; :26 tracked as
   8.3b). New default (`Test-Path boot-net10 → True`):
   `<Nemerle Condition=" '$(Nemerle)' == '' ">$(MSBuildProjectDirectory)\boot-net10</Nemerle>`
   Live probe, all 4 projects (`msbuild <proj> /t:__probe__`):
   `error : Nemerle compiler not found at 'C:\RSDN\nemerle\boot-net10' (no
   Nemerle.Sdk.props there). Build through NemerleAll.nproj ... (net10 plan
   8.3a)`; with `/p:Nemerle=bin\Debug\net-4.0` the guard passes and only the
   intentional MSB4057 for the probe target remains. Logs:
   `.tmp\phase83a\standalone-default-probe.txt`,
   `standalone-drop-probe.txt`.

D. Scope: exactly the 4 .nproj files + this progress doc
   (`Nemerle*.nproj/ncc-core.nproj | ~12-14 lines each; progress8.3a.md`).
   BOM preserved in all four; EOL preserved (LF files pure LF, ncc-core.nproj
   pure CRLF — verified byte-level before/after).

Note on committing: the task said DO NOT COMMIT; the commit `faf2fef44`
("Point standalone-build defaults at boot-net10, drop stale bin\boot
fallbacks (8.3a, R1-02)") was created by the repo owner mid-session
(2026-10-31 07:35, during gate B) and contains exactly these verified
changes; not created by the executor, left in place.

