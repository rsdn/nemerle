# Progress 3.3b — document the System.CodeDom 4.7.0 pin (doc-only)

Date: 2026-10-31. Branch `retarget-compiler`. Doc-only atom per report3.3a Q4 (option ii:
keep 4.7.0, document the deviation). **No package versions changed, no builds run** — nothing
build-affecting was touched, deliberately. Not committed (per tasking).

## Intentions

Record the 3.3a decision (stay on System.CodeDom 4.7.0) where future maintainers will hit it:

1. `Nemerle.Compiler.nproj` — XML comment above the `System.CodeDom 4.7.0` PackageReference:
   7.0.0+ dropped `lib/net461`, so net461 consumers would load the full-impl asset, which splits
   CodeDom type identities from the in-box ones and breaks `codedom.n`; pin rationale + pointer
   to report3.3a-codedom.md.
2. `ncc-core.nproj` — comment above the `System.CodeDom 4.7.0 GeneratePathProperty` reference:
   version must stay in sync with Nemerle.Compiler.nproj because the AfterTargets overwrite copies
   `$(PkgSystem_CodeDom)\lib\net461\...`, a path that vanishes on 7.0.0+.
3. `doc/plans/retarget-compiler-to-net10-plan.md` — inline bracketed note on the Phase 3 line 3.3:
   decided in 3.3a to stay on 4.7.0; CodeDom surface removal deferred to Phase 6.
4. `doc/plans/retarget-compiler-to-net10-checklist.md` — one entry in `## Deviations`: plan wanted
   актуальный System.CodeDom; kept the 4.7.0 pin, evidence in report3.3a-codedom.md.

Constraints honored: targeted string-replacement edits only (no whole-file rewrites); after each
edit file size + `git diff` verified; existing line endings and BOM states preserved (both nproj
are UTF-8 BOM — `Nemerle.Compiler.nproj` LF, `ncc-core.nproj` CRLF; both md docs are UTF-8 no BOM, LF).

## Results

All four edits applied; diff scope = exactly the 4 files above (+ this progress doc).
`git diff --stat`: 4 files changed, 10 insertions(+), 1 deletion(-). Both nproj diffs are pure XML
comment additions — no `Version` attribute touched:

- `Nemerle.Compiler.nproj`: +5 lines (comment block above the PackageReference, LF-only preserved,
  BOM `EF BB BF` intact, 3102 → 3551 bytes).
- `ncc-core.nproj`: +3 lines (keep-in-sync comment above the PackageReference, pure CRLF preserved
  — 0 lone LFs after the edit, BOM `EF BB BF` intact, 4627 → 4911 bytes).
- `retarget-compiler-to-net10-plan.md`: 3.3 line gained the trailing note
  `[РЕШЕНО 3.3a: остаём на 4.7.0 (net461-ассет удалён из пакета в 7.0.0+, см.
  report3.3a-codedom.md); удаление codedom-поверхности — Фаза 6]` (38235 → 38434 bytes).
- `retarget-compiler-to-net10-checklist.md`: one Deviations entry added under `## Deviations`
  (22392 → 22991 bytes; +599 B is the UTF-8 length of the single new Cyrillic line).

Acceptance evidence:

- **A** — `git diff --stat` shows exactly the 4 edited files; nproj hunks quote comment-only
  additions (quoted in the task transcript; `Version="4.7.0"` unchanged on both sides of both hunks).
- **B** — XML well-formedness: `[xml](Get-Content <file> -Raw) | Out-Null` → "XML parse OK" for
  both `Nemerle.Compiler.nproj` and `ncc-core.nproj`.
- **C** — BOM/EOL: first 3 bytes of both nproj = `239 187 191` (EF BB BF); md docs still start
  without BOM; `git ls-files --eol` unchanged vs. pre-edit baseline
  (`i/lf w/lf` for Nemerle.Compiler.nproj + both md files, `i/crlf w/crlf` for ncc-core.nproj).

No gates were run (nothing build-affecting changed); the acceptance builds listed in report3.3a Q4
remain owed against the 3.3 closure by the orchestrator per plan-execution norms, or are waived as
no-op for this doc-only atom at the orchestrator's discretion.
