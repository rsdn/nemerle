# Progress 8.6 — remediation of review #3/#4 findings (docs-heavy, no builds)

Scope: R4-01 (major) + R3-01/02/03/04 + R4-02/R4-04 doc hardening + record the
accepted-as-is set. Reports:
`...-report-review3.md` (R3-01..10), `...-report-review4.md` (R4-01..04).

## Why no builds are run (justification, per task)

- Every edit is a Markdown doc (`doc/MASTER-CHERRY-PICKS.md`, `AGENTS.md`,
  checklist, this progress file) or a new data file (`boot-net10/SEED.manifest`).
- The only non-doc edit is `Nemerle.Location/Nemerle.Location.nproj` (three
  IDE F5 debug-launch properties → relative paths). Verified **outside the
  NemerleAll build graph** before editing:
  `git grep -n -E "Nemerle\.Location|Macros-dnlib|Nemerle\.Compiler-dnlib" -- NemerleAll.nproj Nemerle.MSBuild.targets global.json Directory.Build.props build.ps1 build.sh .github`
  → **empty** (no target, import, or CI reference). The nproj is reachable only
  from `Nemerle.Compiler-dnlib.sln` (IDE convenience). The changed properties
  (`StartProgram`/`WorkingDirectory`/`CmdArgs`) are consumed by the VS debugger,
  never by compilation. No compiler/test source, no build-graph input changes →
  FC ×10 + suite gates are structurally unaffected; re-running them would prove
  nothing about these files.
- `boot-net10/SEED.manifest` is a checksum sidecar; `NPrepareBoot` copies the
  seed via `*.exe/*.dll/*.config/*.json` globs, so a `.manifest` file is never
  picked up by the build (verified against the glob list in NemerleAll.nproj /
  AGENTS.md seed description).

## Verified facts (git probes)

- **master pre-split layout (R4-01)**, `git ls-tree master -- <path>`:
  - `ncc/external/InternalTypes.n` — exists; `git grep -l "class InternalTypeClass" master -- ncc/` → `ncc/external/InternalTypes.n` (also contains `InitSystemTypes`, whose other hit is `ncc/passes.n`).
  - `ncc/external/LibraryReference.n` — exists; `git grep -n "class LibraryReference" master -- ncc/` → includes `ncc/external/LibraryReference.n`.
  - `ncc/CompilationOptions.n` — exists.
  - `ncc/hierarchy/MacroClassGen.n` — exists (this exact path on master);
    FNV anchor `Util.tmpname ($"operator$(x.GetHashCode())")` at line 749 —
    confirms `171f1f227` is the one mechanically cherry-pickable candidate
    (rename-detected, per review #4 merge-tree simulation).
- **Retarget-side source paths exist at their commits**:
  `ncc/backend/hierarchy/LibraryReference.n` @ f35161ae9,
  `ncc/backend/hierarchy/InternalTypeClass.n` @ e9741864a/5a7dadbf1,
  `ncc/frontend/CompilationOptions.n` @ 5a7dadbf1,
  `ncc/frontend/hierarchy/MacroClassGen.n` @ 171f1f227.
- **Range count drift (R3-01)**: `git log --oneline 17d3cf47c..HEAD` = **64**
  commits at `c7ae04a26` (doc said 57). Wording fixed to range-at-writing +
  regeneration command.
- **Macros-dnlib audit (R3-02)**: `git grep -n "Macros-dnlib" -- '*.nproj' '*.sln'`
  → referenced **only** by `Nemerle.Compiler-dnlib.sln:12`, i.e. only by the
  one solution that also carries `Nemerle.Location\Nemerle.Location.nproj`
  (line 8). That sln's four project paths all exist, but the canonical build
  is `NemerleAll.nproj`, which builds `Nemerle.Macros.nproj`
  (NemerleAll.nproj:70) and never the `-dnlib` variants. **Disposition:
  record-and-leave** — the sln may still serve as an IDE-browsing convenience
  and there is no positive evidence it is dead enough to delete; the goal is
  honest state, not extra deletion. Its own `C:\RSDN` debug-launch lines
  (Nemerle.Macros-dnlib.nproj:29-31) are explicitly recorded here as accepted
  machine-specific debug leftovers (the disposition R3-02 itself offers).

## Fixes

1. **R4-01** — `doc/MASTER-CHERRY-PICKS.md`: new "Port mechanics" section
   (master's pre-split layout, modify/delete → silent no-op trap, mapping
   table with the verified probes above); per-candidate port procedure
   rewritten from mechanical cherry-pick to manual port into the monolithic
   files; FX bootstrap+suite gate (R1-03) kept; `171f1f227` kept as the sole
   mechanically cherry-pickable item.
2. **R3-02** — `Nemerle.Location/Nemerle.Location.nproj:30-32`: `C:\RSDN\...`
   → `$(MSBuildThisFileDirectory)..\...` (XML well-formedness probed; grep
   proof the absolute paths are gone). Macros-dnlib: record-and-leave (above).
3. **R4-02** — `boot-net10/SEED.manifest` (sha256 + size + filename, one per
   line, `Get-FileHash`-generated) + AGENTS.md "Re-seeding the bootstrap
   chain" recipe (≤20 lines) incl. Validate-blindness note and the 4.6 /
   `2ff4e2fb4` precedent.
4. **R3-03** — AGENTS.md: `FC ×5 assemblies` → ×10 norm (5 assemblies × 2
   directions) in both the command table and the fixed-point sentence; fixed
   the Stage1/Stage2 conflation flagged in R4-02 while touching that sentence.
5. **R3-01** — sha-count wording: range-at-writing + `git log --oneline
   17d3cf47c..HEAD` regeneration command.
6. **R4-04** — AGENTS.md Gotchas: seed runtimeconfig pins
   `Microsoft.NETCore.App 10.0.0` with default Minor rollForward; 11.x-only
   machines need `DOTNET_ROLL_FORWARD=Major` (stock policy, not a defect).
7. **R3-04** — checklist: deleted the duplicate `[~] 8.3d` line, kept `[✅]`.

## Accepted as-is (recorded, not implemented)

- **R3-05** `NStageSelect` never reset: reachable only via repeated standalone
  `_NugetPack` in one MSBuild process; composite invocations never set it; the
  `packaging stage '…'` Message makes any flip visible. Single invocation per
  process is the shipped contract.
- **R3-06** `Sdk.targets` import conditioned on `Nemerle.Sdk.props` existence:
  the props-present-targets-missing partial drop is pathological; the guard
  covers the realistic stale-seed case.
- **R3-07** build.sh refusal-message order (msbuild resolution fires before
  the deliberate OS refusal on a bare Linux box): both paths exit 2 without
  executing anything; message merge is cosmetic.
- **R3-08** `da2b5cdee` commit-message phrasing: history is immutable, no
  rewrite for a wording nit; the clarification lives in report-review3.md and
  the permanent `ManagerClass.n` comment is correct.
- **R3-09** unix `fc`/diff single-direction in `NemerleAll.nproj` Validate:
  Validate is documented Windows-only and `build.sh` refuses non-Windows
  builds, so the asymmetry is unreachable in practice.
- **R4-03** 167-default-ref drift probe (compile probe `.n`, assert zero
  `cannot load default framework reference` warnings): recommendation recorded
  for a future test atom; not implemented here (this atom is docs + one
  out-of-graph nproj, test code is out of its declared scope).

## Status

All items done; acceptance probes recorded:

- A. `git diff --stat`: `AGENTS.md` (+33/−…), `Nemerle.Location/Nemerle.Location.nproj` (6),
  `doc/MASTER-CHERRY-PICKS.md` (87), checklist (1 deletion); new files
  `boot-net10/SEED.manifest` + this progress doc. Nothing else.
- B. `[xml](Get-Content …)` → OK; `git grep -i "c:\\rsdn"` on the nproj → no
  matches (exit 1).
- C. `SEED.manifest` (10 lines) recomputed with `Get-FileHash` — all 10
  sha256+size pairs match.
- D. Mapping targets re-probed after the edit: `git ls-tree master` for all
  four master files + class greps + FNV anchor at
  `master:ncc/hierarchy/MacroClassGen.n:749`; additionally the four
  retarget-side paths confirmed absent on master (`ncc/frontend/`/`ncc/backend/`
  do not exist there).
- E. No builds run (justification above). No commits (per task).
