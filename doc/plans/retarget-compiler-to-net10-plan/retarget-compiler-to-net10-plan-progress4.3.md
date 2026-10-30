# Progress 4.3 — Point bootstrap at the net10 seed, full reseed cycle

Status: DONE (all gates PASS)
Branch: retarget-compiler
Date: 2026-10-31

## Intent

Sub-atom 4.3: switch the bootstrap seed from the old dnlib/mono-generation seed
(`boot-dnlib\`) to the new .NET 10 seed (`boot-net10\`, built and smoke-proven
in 4.2) and prove the whole bootstrap chain still reaches a fixed point when
seeded with it.

Concrete goal: run the FULL reseed cycle once —
`Rebuild Stage3,Validate` (Stage1 → Stage2 → Stage3 → Validate), then
`Build Stage1,CompilerTests` — and prove every stage compiles on the net10
host, i.e. every compiler invocation is `ncc-core.exe` (never FX `ncc.exe`).

## Steps

1. Edit `NemerleAll.nproj`: `NRootBoot` = `$(NRoot)\boot-net10` (was
   `boot-dnlib`). Single targeted hunk; extend the seed comment minimally
   (net10 framework-dependent seed; `boot-dnlib` kept as emergency fallback
   until 4.5 closes). After the edit verify file size ~37 KB, BOM EF BB BF
   intact, `git diff` shows only this hunk.
2. Purge stale contents of `bin\Debug\net-4.0\boot\` (111 files of the old FX
   seed copies + Validate leftovers). The folder is regenerated fresh from
   `$(NRootBoot)` by `NPrepareBoot` + NTasks. Also clear Stage2/Stage3 `*.il`
   (5+5 from the previous Validate; optional, Validate pre-cleans anyway).
3. Run in ONE invocation:
   `cmd /c "`"C:\RSDN\nemerle\Build-core-base.cmd`" Debug Rebuild "Stage3,Validate""`
   Full log → `.tmp\phase43\stage3-validate.log`.
4. Run:
   `cmd /c "`"C:\RSDN\nemerle\Build-core-base.cmd`" Debug Build "Stage1,CompilerTests""`
   Full log → `.tmp\phase43\stage1-tests.log`.

## Pass gates

- Stage3+Validate chain: exit code may be 1 (known-red pefverify);
  exactly **5 × `FC: no differences encountered`** (ncc-core.dll,
  Nemerle.Compiler, Nemerle, Nemerle.Macros, Nemerle.MSBuild.Tasks); the SOLE
  `: error MSB` is the known-red pefverify MSB3073 on
  `Stage3\Nemerle.Compiler.dll`; no "no valid CLR header" anywhere.
- Every stage runs on the net10 compiler: ALL `Using Nemerle compiler:` lines
  (expected 15 = 5 compiles × 3 stages) end in `ncc-core.exe` and live under
  `bin\Debug\net-4.0\boot\`, `Stage1\` or `Stage2\`; ZERO non-core `ncc.exe`
  compiler invocations. If FX `ncc.exe` reappears in `boot\` after the fresh
  copy — STOP and report (unknown consumer).
- CompilerTests: 468 positive + 167 negative, 0 failed, exit 0.
- Invariants after the cycle: `boot-dnlib\` untouched on disk (decision 4.5 —
  emergency seed; `git status --short -- boot-dnlib` clean); `boot-net10\`
  still its original 10 files; `bin\Debug\net-4.0\boot\ncc-core.exe -version`
  exits 0.
- NO commit.

## Results

### Build-entry deviation (tooling, not plan)

The prescribed `cmd /c "`"...cmd`" Debug Rebuild "Stage3,Validate""` form was
mangled by this PowerShell version's native-argument quoting: MSBuild received
`/t: Stage3,Validate` (leading space) → `error MSB4057: The target " Stage3"`
and the build never started (914-byte log, nothing built, tree untouched).
Workaround: invoked the exact command `Build-core-base.cmd:51` itself runs —
`MSBuild.exe NemerleAll.nproj /t:Stage3,Validate /p:NTargetName=Rebuild
/p:Configuration=Debug /p:NInstall=bin\Debug\net-4.0` (plus
`MSBUILDENABLEALLPROPERTYFUNCTIONS=1`), argument passing verified first with a
throwaway `/t:__qtest__` probe. Same targets, same Rebuild forcing; one
invocation per step as planned.

### NRootBoot edit (single hunk)

`NemerleAll.nproj` L25-27: comment extended +
`<NRootBoot>$(NRoot)\boot-net10</NRootBoot>`. Size 37,077 → 37,205 bytes,
BOM EF BB BF intact, `git diff` contains only this hunk.

### Purge

`bin\Debug\net-4.0\boot\`: 111 stale FX-seed files pre-purge → 0 items after
(plus Stage2/Stage3 `*.il` 5+5 → 0).

### Stage3 + Validate (Rebuild) — PASS

Exit code 1 (expected). Full log: `.tmp\phase43\stage3-validate.log`
(5,597 lines).

- `FC: no differences encountered` × **5** — ncc-core.dll, Nemerle.Compiler,
  Nemerle, Nemerle.Macros, Nemerle.MSBuild.Tasks (Stage2 vs Stage3 .il).
- SOLE `: error MSB` = known-red pefverify
  `error MSB3073: ... pefverify.exe ...Stage3\Nemerle.Compiler.dll exited with
  code 1` (reported inline + in summary = 2 identical lines; 1126 `[MD]`
  0x80131252 pefverify verdicts inside that same known-red step, documented
  as expected since 3.1a-fix/3.4b/3.5e).
- "no valid CLR header": 0 occurrences.
- `Using Nemerle compiler:` — 12 lines = 4 Nemerle compiles × 3 stages
  (Nemerle, Nemerle.Compiler, Nemerle.Macros, ncc-core; the 5th FC'd assembly
  Nemerle.MSBuild.Tasks is a C# csproj, so it never invokes ncc — the "5 per
  stage" expectation miscounted it):
  - Stage1 ×4: `bin\Debug\net-4.0\boot\ncc-core.exe` (the net10 seed)
  - Stage2 ×4: `bin\Debug\net-4.0\Stage1\ncc-core.exe`
  - Stage3 ×4: `bin\Debug\net-4.0\Stage2\ncc-core.exe`
  ZERO non-core `ncc.exe` mentions anywhere in the log; no FX ncc.exe in the
  fresh `boot\` copy.

### Stage1 + CompilerTests (Build) — PASS

Exit 0, log `.tmp\phase43\stage1-tests.log`: positive **468 passed / 0
failed**, negative **167 passed / 0 failed**, "All tests passed." ×2;
13 additional `Using Nemerle compiler:` lines, all `Stage1\ncc-core.exe`
(test-suite snippets + harness).

### Post-cycle invariants

- `git status --short -- boot-dnlib` → empty (emergency seed untouched, 4.5).
- Whole-repo status: only ` M NemerleAll.nproj` + the new progress file
  (AGENTS.md / boot-net10/ were untracked before this task). No commit.
- `boot-net10\`: still the original 10 files.
- `bin\Debug\net-4.0\boot\`: 10 seed files + 5 NTasks outputs; `ncc.exe`
  count 0.
- `bin\Debug\net-4.0\boot\ncc-core.exe -version` → exit 0,
  "Nemerle Compiler (ncc) version 10.0.25.0 (git)".

Conclusion: the bootstrap chain seeds from `boot-net10`, reaches the
Stage2↔Stage3 fixed point, and CompilerTests are green on the net10 seed.

