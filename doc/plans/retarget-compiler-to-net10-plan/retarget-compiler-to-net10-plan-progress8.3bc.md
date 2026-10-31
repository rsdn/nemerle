# Progress 8.3b + 8.3c — stale comments/docs + Validate robustness

Status: **DONE** (2026-10-31; gates A-D green, see §Evidence; checklist atoms 8.3b, 8.3c;
findings R1-05, R1-07, R1-08, R2-05, R2-08).

## Scope

8.3b (docs/comments sweep, R1-07 + R1-08 + R2-05 + R2-08):
1. `.github/workflows/build.yml` tests-job: reword "8 known failures until
   6.4 / TODO(6.4)" — 6.4 resolved all 8, allowlist is comments-only now;
   keep the `NKnownFailures` env line (empty-by-default insurance).
2. `NemerleAll.nproj` boot comment (:25-26) + `NPrepareBoot` sidecar comments
   (:241-250) still describe `boot-dnlib` as on-disk — deleted in 00cee342b
   (7.4); reword to git history.
3. `build.ps1`/`build.sh` (and the matching `NemerleAll.nproj:180` comment)
   forward-reference "Phase 6.3 retires the ildasm discovery" — 6.3 shipped
   without retiring it; reword to accurate state.
4. R2-05: AGENTS.md `boot-net10` parenthetical lists 6 of the 10 seed files —
   make exhaustive (`git ls-files boot-net10`: ncc-core.exe/.dll,
   ncc-core.runtimeconfig.json/.deps.json, Nemerle.dll, Nemerle.Compiler.dll,
   Nemerle.Macros.dll, Nemerle.MSBuild.Tasks.dll, dnlib.dll,
   System.CodeDom.dll).
5. R2-08: AGENTS.md note that a stale `bin\<Config>\net-4.0` drop can predate
   compiler fixes (e.g. zero-`-r` failing) — rebuild/reinstall, don't debug.
6. R1-08: `misc/packages/wix/Sources/CompilerAndTools.wxs` (exact path via
   `git grep ncc32`) still declares deleted `ncc32.exe`/`ncc64.exe` payload —
   remove the dead File entries/components/refs minimally (target guarded off
   since 5.5; no WiX restructuring).

8.3c (Validate, R1-05):
7. Comment at the Validate `fc` list: `Nemerle.MSBuild.Tasks.dll` compares as
   a byte-identical copy of the boot copy in both stage dirs (NTasks builds
   it once into `$(NBoot)`, stages only `Copy` it) — real self-compiled IL
   coverage is the other 4; comparison kept as a harmless tripwire.
8. fc direction: review2 proved `fc "prev\*.il" "cur\*.il"` iterates only the
   first wildcard (cur-only files ignored, exit 0; prev-only fails, exit 2).
   Add the reverse-direction `fc` Exec so Stage3-only unexpected output is
   caught; CI gate threshold updated for the doubled "FC: no differences"
   line count.

## Evidence

### A. Green gate — `./build.ps1 -Targets Stage3,Validate -Rebuild` (log `.tmp\phase83bc\validate.log`)

Exit 0 (`EXITCODE=0` appended as last log line; console showed
`Build succeeded. Nemerle installed to 'C:\RSDN\nemerle\bin\Debug\net-4.0'`).
Zero `: error` / `MSB3073` lines. **FC lines ×10** (5 assemblies × 2
directions), lines 95-122, all `FC: no differences encountered`:

```
Comparing files ...\STAGE2\ncc-core.dll.il and ...\STAGE3\ncc-core.dll.IL            (fwd)
Comparing files ...\STAGE2\Nemerle.Compiler.dll.il and ...                            (fwd)
Comparing files ...\STAGE2\Nemerle.dll.il and ...                                     (fwd)
Comparing files ...\STAGE2\Nemerle.Macros.dll.il and ...                              (fwd)
Comparing files ...\STAGE2\Nemerle.MSBuild.Tasks.dll.il and ...                       (fwd)
Comparing files ...\STAGE3\ncc-core.dll.il and ...\STAGE2\ncc-core.dll.IL             (rev)
Comparing files ...\STAGE3\Nemerle.Compiler.dll.il and ...                            (rev)
Comparing files ...\STAGE3\Nemerle.dll.il and ...                                     (rev)
Comparing files ...\STAGE3\Nemerle.Macros.dll.il and ...                              (rev)
Comparing files ...\STAGE3\Nemerle.MSBuild.Tasks.dll.il and ...                       (rev)
```

Note: first launch failed at *evaluation* with MSB4045 (`XML comment cannot
contain '--'` in the new comments — `--` is illegal inside XML); fixed by
rewording before the green run. Lesson worth keeping: no `--` sequences in
MSBuild/WiX comments.

### B. fc-direction proof (scratch, `.tmp\phase83bc\fc-proof.txt`)

`fcproof\prev` = a.il,b.il; `fcproof\cur` = a.il,b.il,surprise.il:
- forward  `fc "prev\*.il" "cur\*.il"` → surprise.il silently skipped,
  2× "FC: no differences encountered", exit **0** (the old blind spot);
- reverse  `fc "cur\*.il" "prev\*.il"` → `FC: cannot open ...surprise.IL`,
  exit **2** → MSB3073 → the new second Exec reddens the Validate target.
Real stage dirs were not touched.

### C. No suite re-run + build.yml re-grep

`Stage1,CompilerTests` not re-run: the diff touches only comments/docs
(build.yml, AGENTS.md, build.ps1/sh, nproj comments), one guarded-off WiX
file (target refuses to run without `-p:BuildingVsIntegration=true`, 5.5),
and the Validate fc Exec — no compiler, library, macro or test-harness
source changed, so the 461+7nr/167/0 norm is untouched.
CI gate re-grep: comment ">= 10 \"FC: no differences\" lines expected
(5 assemblies x 2 directions)" (:45), `if ($fc -lt 10)` (:62), error string
">= 10 expected" (:63) — matches the observed 10; no "8 known failures"/
"TODO(6.4)" strings remain; `$env:NKnownFailures = 'ci-known-failures-64.txt'`
kept (:130).

### D. `git diff --stat` scope

build.yml, AGENTS.md, NemerleAll.nproj, build.ps1, build.sh,
misc/packages/wix/Sources/CompilerAndTools.wxs + this progress doc.
(The working tree also carries the 8.3a executor's uncommitted Results
appends to progress8.3a.md — not part of this atom, left untouched.)
BOM preserved (nproj UTF-8-BOM), EOLs preserved (nproj CRLF, others LF);
both edited XML files parse (`[xml]` load OK).

## Status

DONE (no commit, per task instruction).
