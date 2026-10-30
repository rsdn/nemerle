# Progress 3.1d-validate-glob

## Intentions

Scope: ONLY the Validate target (~lines 315-355) of `NemerleAll.nproj`, per report
`retarget-compiler-to-net10-plan-report3.1c-3.1d.md` Q3/Q4. `ncc-core.nproj` stays
untouched (pre-existing uncommitted green state).

1. Exclude native Core apphosts from the `ncc*.exe` globs used for ildasm + pefverify:
   `Exclude="$(NPrevBin)\ncc-*.exe"` / `Exclude="$(NCurBin)\ncc-*.exe"` (absolute patterns;
   relative Excludes would resolve against the project dir and not match).
2. Add the managed Core payload `ncc-core.dll` to the IL comparison only: new `Il2`/`Il3`
   items = `@(Asm2)`/`@(Asm3)` + `$(NPrevBin)\ncc-core.dll` / `$(NCurBin)\ncc-core.dll`;
   switch the two ildasm Execs to iterate `%(Il2.FullPath)` / `%(Il3.FullPath)`.
   pefverify keeps iterating Asm3 → file set stays EXACTLY the pre-flip 5
   (4 Nemerle*.dll + ncc.exe); known-red pefverify on Nemerle.Compiler.dll preserved
   until Phase 5.1.
3. Pre-clean stale `*.il` before ildasm generation: `<Delete Files="$(NPrevBin)\*.il;$(NCurBin)\*.il" />`
   at target top (aborted run left stale `ncc-core.exe.il` etc. in Stage dirs → false fc diffs).
4. Leave `fc` (globs `*.il`, so `ncc-core.il` joins automatically), pefverify and trailing
   cleanup untouched. Nothing else in the file.

Validation (report Q4):
- Pre-clean `bin\Debug\net-4.0\Stage1\*.il` and `Stage2\*.il`.
- `cmd /c "Build-core-base.cmd Debug Build "Stage3,Validate""` → expect exit 1 with exactly
  one MSB3073 = pefverify on `Stage3\Nemerle.Compiler.dll`; zero "no valid CLR header";
  `FC: no differences encountered` × 6 (baseline 5 + `ncc-core.il`).
- Log must show ildasm invoked on `ncc-core.dll` and never on `ncc-core.exe`.
- Contingency: if ildasm 4.6.1 rejects `ncc-core.dll`, drop the two Il `ncc-core.dll` lines
  with a comment; requirements (i)(iii)(iv) still stand.
- Do NOT re-run CompilerTests. Do NOT commit. Preserve CRLF + UTF-8 BOM of NemerleAll.nproj.

## Results

DONE. Edits applied verbatim per report Q3; build run per Q4 step 1; not committed.

### Edit (`NemerleAll.nproj`, Validate target only)
- `<Delete Files="$(NPrevBin)\*.il;$(NCurBin)\*.il" />` inserted at target top.
- `Asm2`/`Asm3` `ncc*.exe` globs gained `Exclude="$(NPrevBin)\ncc-*.exe"` /
  `Exclude="$(NCurBin)\ncc-*.exe"` (absolute patterns); explanatory comment added.
- New `Il2`/`Il3` items = `@(Asm2)`/`@(Asm3)` + `ncc-core.dll` per dir; the two ildasm
  Execs now iterate `%(Il2.FullPath)` / `%(Il3.FullPath)`.
- `fc`, pefverify (`%(Asm3.FullPath)` → still exactly the pre-flip 5 files) and trailing
  cleanup untouched. ildasm accepted `ncc-core.dll` — no contingency needed.
- Stale `*.il` pre-cleaned before the run (`del` of Stage{1,2,3} → 0 remaining).

### Acceptance
- **A**: `Build "Stage3,Validate"` → exit 1, `Build FAILED`. Log
  `C:\RSDN\phase31d-logs\stage3-validate.log`: `FC: no differences encountered` × **6**
  (pairs incl. `ncc-core.dll.il`); sole MSBuild error code is
  `NemerleAll.nproj(357,5): error MSB3073: The command "...\peverify.exe" "...\Stage3\Nemerle.Compiler.dll" exited with code 1.`
  (summary "1126 Error(s)" = the known 1125 pefverify `[MD]` diagnostic lines + this 1
  MSB3073, same signature as the pre-flip known-red; only MSB code in the log is MSB3073).
  `findstr /C:"no valid CLR header"` → exit 1 (zero matches). pefverify was invoked only
  on `Nemerle.Compiler.dll` (fail-fast on first Asm3 item; set unchanged, no `ncc-core.dll`).
- **B**: ildasm invoked exactly 6× per stage dir; e.g.
  `"...\NETFX 4.6.1 Tools\ildasm.exe" "C:\RSDN\nemerle\bin\Debug\net-4.0\Stage3\ncc-core.dll" /out="C:\RSDN\nemerle\bin\Debug\net-4.0\Stage3\ncc-core.dll.il" /nobar`.
  `findstr /C:"ncc-core.exe" stage3-validate.log` returns only two `Copying file from
  "...\obj\...\apphost.exe" to "...\Stage{2,3}\ncc-core.exe"` build lines — zero ildasm lines.
- **C**: `git status` — modified `NemerleAll.nproj` + this progress doc, on top of
  pre-existing `ncc-core.nproj` (untouched) and checklist bookkeeping. BOM intact:
  file starts `ef bb bf 3c 50 72 6f 6a...` (`<Project>`); first diff hunk `@@ -315,11 +315,18 @@`;
  `git ls-files --eol` → `i/crlf w/crlf`.
- CompilerTests not re-run (per instructions, green at 468+167/0 in 3.1c).

## Re-application

The validated edit was **lost**: `NemerleAll.nproj` had been truncated to **0 bytes**
(whole-file rewrite accident in a later session) and was restored from HEAD → pre-edit
state (36,128 B, clean `git diff`). Re-applied from this document + report Q3 using
**targeted string replacements only** (no full-file round-trips), then re-validated.

### Re-applied edits (`NemerleAll.nproj`, Validate target only; size after each step)
- Edit 1 = `Delete *.il` at target top + ItemGroup per Q3 (Excludes, Il2/Il3) —
  36,128 → 36,593 B; hunk `@@ -315,11 +315,18 @@`.
- Edit 2 = ildasm Execs `%(Asm2/3.FullPath)` → `%(Il2/3.FullPath)` —
  36,593 → 36,589 B; hunk `@@ -329,8 +336,8 @@`. Total `git diff --stat`: +11/−4, line 1
  untouched; BOM `ef bb bf` + CRLF intact (`git ls-files --eol` → `i/crlf w/crlf`).
  pefverify still iterates `%(Asm3.FullPath)`; `fc`/cleanup untouched;
  `ncc-core.nproj` and all other files not modified by this re-apply.

### Re-apply acceptance (single build)
`Build-core-base.cmd Debug Build "Stage3,Validate"` → exit 1, `Build FAILED`,
log `C:\RSDN\phase31d-logs\stage3-validate2.log`:
- `FC: no differences encountered` × **6** (pairs incl. `ncc-core.dll.il`).
- Sole MSBuild error: `NemerleAll.nproj(357,5): error MSB3073: ... peverify.exe
  "...\Stage3\Nemerle.Compiler.dll" exited with code 1.` (summary 1126 Error(s) =
  1125 pefverify `[MD]` lines + this 1 MSB3073; only MSB code in the log; pefverify
  invoked only on `Nemerle.Compiler.dll`, no `ncc-core.dll` item added).
- `findstr /C:"no valid CLR header"` → exit 1 (zero matches).
- ildasm invoked 6× per stage dir; includes `ncc-core.dll` (log lines 569/575), zero
  ildasm lines for `ncc-core.exe`. 12 stale `*.il` (Stage2/3) were pre-existing; the new
  target-top `Delete` + `/out` regeneration kept `fc` at exactly the 6 expected pairs.
- `git status --short`: modified = `NemerleAll.nproj` + `ncc-core.nproj` + checklist
  (pre-existing); final size 36,589 B (≥ 36,128). Not committed.
