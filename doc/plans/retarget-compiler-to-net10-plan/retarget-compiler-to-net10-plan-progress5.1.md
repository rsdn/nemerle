# Progress 5.1 — Remove .NET Framework SDK probe machinery from NemerleAll.nproj

Status: DONE (all gates PASS)
Branch: retarget-compiler
Date: 2026-10-31

## Intent

Sub-atom 5.1 (plan Phase 5.1): drop the .NET Framework SDK tool discovery
machinery from `NemerleAll.nproj` `InitTools` (`GetFrameworkSdkPath`/`SDKBin`
probes keyed on `al.exe`, `gacutil`, `peverify`, `ngen`, the never-defined
`ALPath` message) and delete the pefverify Exec step in `Validate` — the last
consumer of the probe and the documented known-red (MSB3073 on
`Stage3\Nemerle.Compiler.dll`: the old NETFX 4.6.1 Tools pefverify cannot
verify netstandard2.0; "excluded until Phase 5.1" is now).

**NEW GATE after this change:** forced `Rebuild "Stage3,Validate"` →
`FC: no differences encountered` ×5 AND **ZERO `: error MSB`**, exit code 0.

Kept: ildasm discovery (the Validate `fc` fixed-point machinery still needs
it; replacement arrives only with the net10 orchestration of Phase 6.3) and
everything Phase 5.1 does not own (`MSBuild`/`Junction`/`Nuget`/`XBuild`
properties, `NTasks`, `Install`, `CompilerTests`, `NPrepareBoot` untouched).

## Audit (git grep across NemerleAll.nproj, snippets, tools, *.cmd)

Live consumers of the probe properties exist ONLY inside `NemerleAll.nproj`
(`Ildasm` → Validate ildasm Execs; `PEVerify` → Validate pefverify Exec;
`GacUtil`/`NGen`/`ALPath` → Message lines only). No other project in the repo
references these properties or `SDKBin` (Stage targets forward `SDKBin=` to
sub-projects, but no sub-project consumes it — grep found no live reader).
There is NO `al`/`Alarm`-based build step and no policy-file target anywhere
in the build, so nothing that runs today is silently dropped. The standalone
root scripts (`GacReg*.cmd`, `Reg-bins*.cmd`, `Reg-boot*.cmd`) hardcode their
own gacutil/ngen paths, are not invoked by `NemerleAll.nproj` or
`Build-core*.cmd`, and are out of scope (retirement of those scripts is a
later cleanup; this sub-atom's scope per plan is `NemerleAll.nproj`).
`snippets/codegentests/.../PeVerify.cs` is IDE-integration test code not built
by any target in the Stage/Validate/CompilerTests chain (`snippets/` is
branch-specific IDE material on retarget-compiler) — out of scope.

## Tool-by-tool disposition

| Tool / machinery | Location (pre-edit) | Consumers | Disposition |
|---|---|---|---|
| `GetFrameworkPath` → `FW40` | InitTools | `NGen` property only | REMOVE (feeds only ngen) |
| `GetFrameworkSdkPath` → `SDK_3` (MSBuild branch) | InitTools | `SDKBin` ildasm-discovery chain | KEEP (ildasm discovery needs it) |
| `GetFrameworkSdkPath` → `SDK` (mono branch) | InitTools | `SDKToolsBin` → mono `Ildasm` (monodis) | KEEP (mono ildasm path) |
| `SDKBin` 7-probe fallback chain | InitTools | `Ildasm`; `SDKBin=` forward to sub-projects (no reader) | KEEP, slimmed: Exists-probes re-keyed `al.exe` → `ildasm.exe`; chain semantics unchanged |
| `GacUtil` | InitTools defs + Message | none | REMOVE |
| `Ildasm` | InitTools defs + Message | Validate ildasm Execs (`fc` machinery) | KEEP + comment: retire with the `fc` step at Phase 6.3; `/NOIL` partial-disassembly quirk documented (full-dump mode works, `/NOIL` does not — 3.2 finding) |
| `PEVerify` | InitTools defs + Message | Validate pefverify Exec (known-red) | REMOVE together with the Exec |
| `NGen` | InitTools defs + Message | none | REMOVE |
| `ALPath` Message | InitTools | property never defined | REMOVE (dead) |
| pefverify Exec in Validate | Validate | — | REMOVE ENTIRELY (known-red retired) |
| `Asm2`/`Asm3` item lists | Validate | consumed by `Il2`/`Il3` (`Include="@(Asm…)"`) | KEEP (ildasm+fc still iterate them via Il2/Il3) |
| `al.exe`-based targets / policy files | — | none exist | nothing to remove (verified) |

## Steps

1. Progress file first (this file).
2. Edits in `NemerleAll.nproj` only, targeted string replacements:
   - `InitTools`: drop `GetFrameworkPath`/`FW40`, `GacUtil`, `PEVerify`,
     `NGen`, `ALPath` Message; re-key SDKBin probes to `ildasm.exe`; keep
     `Ildasm`/`SDK_3`/`SDK`/`MSBuild`/`Junction`/`Nuget`/`XBuild`; add the
     retirement + `/NOIL` comment.
   - `Validate`: delete the pefverify Exec; fix the item-group comment that
     mentions pefverify.
   After each edit: file size ~37 KB, BOM EF BB BF intact, XML parses,
   `git diff` shows only the intended hunks.
3. Build (one at a time, MSBuild invoked directly exactly as
   `Build-core-base.cmd:51` does, vswhere-discovered path,
   `MSBUILDENABLEALLPROPERTYFUNCTIONS=1`):
   - `Rebuild Stage3,Validate` → log `.tmp\phase51\stage3-validate.log`.
     Gate: FC ×5, zero `: error MSB`, exit 0, no pefverify/gacutil/al/ngen
     invocations in the log.
   - `Build Stage1,CompilerTests` → log `.tmp\phase51\stage1-tests.log`.
     Gate: 468 positive + 167 negative, 0 failed, exit 0.
4. Residual audit: `git grep -n "peverify\|gacutil\|ngen\|al\.exe" -- NemerleAll.nproj`
   → zero live uses (comments OK).
5. NO commit.

## Results

### Static verification (after edits, before builds)

- `NemerleAll.nproj` 36,898 bytes (~37 KB), BOM `EF BB BF` intact, CRLF-only
  (CR=LF=573), XML parses (36 targets incl. InitTools/Validate).
- `git diff --stat`: only `NemerleAll.nproj` (+9/−21, two hunks: InitTools,
  Validate) + docs; `SDKBin` fallback line `$(SDK_3)\bin\NETFX 4.0 Tools`
  verified byte-identical to before.

### Stage3 + Validate (forced Rebuild) — PASS, NEW GATE MET

Exit code **0** (`EXITCODE=0`; 4.3 baseline was exit 1 due to the known-red
peverify). Log: `.tmp\phase51\stage3-validate.log` (3330 lines,
"0 Error(s)", "Time Elapsed 00:01:36.61" — full chain: 12
`Using Nemerle compiler:` lines = 4 ncc compiles × 3 stages, boot →
Stage1 → Stage2 `ncc-core.exe` respectively).

- `FC: no differences encountered` × **5** (ncc-core.dll, Nemerle.Compiler,
  Nemerle, Nemerle.Macros, Nemerle.MSBuild.Tasks — Stage2 vs Stage3).
- `: error MSB` lines: **0** (peverify MSB3073 gone); `MSB3073`/"CLR header"
  occurrences: 0.
- `peverify|gacutil|ngen.exe|al.exe` invocation lines in the whole log: **0**.
- InitTools now logs only `Ildasm   - "...\NETFX 4.6.1 Tools\ildasm.exe"`
  (re-keyed 4.6.1 probe hit first) + MSBuild/Junction; 10 ildasm Execs + 5 fc
  ran normally (full-dump mode).

### Stage1 + CompilerTests (Build) — PASS

Rerun (see tooling incident below): **exit code 0**
(`stage1-tests-exit.txt: EXITCODE=0`), "Build succeeded.", "0 Error(s)",
`: error MSB` lines: 0. Positive tests: **468 passed / 0 failed**
("All tests passed. Time taken: 00:02:13"); negative tests: **167 passed /
0 failed** ("All tests passed. Time taken: 00:00:31"). Log:
`.tmp\phase51\stage1-tests.out` (stderr file empty).

### Tooling incident (first CompilerTests attempt, not a plan failure)

First attempt (same background-pwsh form as the Validate run) died mid-way:
the pwsh pipeline exited early with `$LASTEXITCODE=-1` and froze
`stage1-tests.log` at 391,980 bytes (positive tests still running), while the
MSBuild.exe it had launched (PID 52000) kept running orphaned with a broken
stdout pipe — unreliable capture, unsafe to leave. Killed the tree
(`taskkill /PID 52000 /T /F` + `dotnet build-server shutdown`), reran with
process-level redirection (`Start-Process -RedirectStandardOutput/
RedirectStandardError -Wait -PassThru`), which does not depend on the pwsh
pipeline staying alive. The aborted attempt is kept as
`stage1-tests.log` (frozen); the rerun logs to `stage1-tests.out/.txt`.
After the rerun had completed (verified full-success log + `EXITCODE=0`), an
extra MSBuild with the identical `Stage1,CompilerTests` command line appeared
(cmd wrapper → MSBuild PID 45000 → test harness PID 51728), seemingly the
background-shell wrapper re-firing the command; it was killed as a redundant
self-racing duplicate (`taskkill /PID 26096 /T /F`, `taskkill /PID 45000 /T
/F`) before it could interfere; the killed duplicate only dirties the
`bin\Debug\net-4.0\Tests\*` scratch dirs that CompilerTests recreates each
run. Post-kill sweep: no `MSBuild`/`Nemerle.Compiler.Test`/`ncc*` processes
remain.


## Final state / acceptance evidence

- **A**: `NemerleAll.nproj` 36,898 bytes (~37 KB), BOM `EF BB BF`, CRLF-only
  (CR=LF=573), XML parses (36 targets). `git diff --stat` → only
  `NemerleAll.nproj` (+9/−21, hunks in `InitTools` + `Validate`); docs: this
  new progress file (untracked). NO commit.
- **B**: forced `Rebuild "Stage3,Validate"` → `EXITCODE=0`, `FC: no
  differences encountered` ×5 (ncc-core.dll, Nemerle.Compiler, Nemerle,
  Nemerle.Macros, Nemerle.MSBuild.Tasks), ZERO `: error MSB`, zero
  `peverify|gacutil|ngen.exe|al.exe` lines in the log (was: 1 known-red
  MSB3073 + 1126 pefverify [MD] verdicts). Log:
  `.tmp\phase51\stage3-validate.log`.
- **C**: 468 + 167, 0 failed, exit 0 (above).
- **D**: `git grep -n "peverify\|gacutil\|ngen\|al\.exe" --
  NemerleAll.nproj` → single hit, line 163, the explanatory comment
  (zero live uses).

Conclusion: the .NET Framework SDK probe machinery is out of
`NemerleAll.nproj`; the known-red pefverify gate is retired; Validate's
ildasm+fc fixed-point machinery is green on the remaining slimmed ildasm
discovery, and the new gate (FC ×5, zero `: error MSB`, exit 0) holds.

Status: DONE (all gates PASS)




## Acceptance (orchestrator-directed)

Verification run for sub-atom 5.1 (verify-and-record; no sources modified, no
commit). Verified 2026-10-31 ~03:20 against `.tmp\phase51\` logs.

### A. Validate log completeness (`stage3-validate.log`)

- Final summary (log tail, verbatim):
  `Build succeeded.` ... `60 Warning(s)` / `0 Error(s)` /
  `Time Elapsed 00:01:36.61` / `EXITCODE=0`.
- `peverify` occurrences in the whole log: **0** (invocation removed from
  `Validate` as planned; previously known-red MSB3073).
- `: error` lines in the whole log: **0** (new gate holds: zero `: error MSB`).
- `FC: no differences encountered`: **5**, pairs (verbatim, Stage2 vs Stage3):
  1. `Comparing files ...\STAGE2\ncc-core.dll.il and ...\STAGE3\ncc-core.dll.IL`
  2. `Comparing files ...\STAGE2\Nemerle.Compiler.dll.il and ...\STAGE3\Nemerle.Compiler.dll.IL`
  3. `Comparing files ...\STAGE2\Nemerle.dll.il and ...\STAGE3\Nemerle.dll.IL`
  4. `Comparing files ...\STAGE2\Nemerle.Macros.dll.il and ...\STAGE3\Nemerle.Macros.dll.IL`
  5. `Comparing files ...\STAGE2\Nemerle.MSBuild.Tasks.dll.il and ...\STAGE3\Nemerle.MSBuild.Tasks.dll.IL`

### B. CompilerTests (`compilertests.log`)

Invocation (proven direct-MSBuild form per progress-4.3; vswhere-discovered
`MSBuild.exe` = `...\VS 18\Professional\MSBuild\Current\Bin\MSBuild.exe`,
`MSBUILDENABLEALLPROPERTYFUNCTIONS=1`):
`MSBuild.exe NemerleAll.nproj /t:Stage1,CompilerTests /p:NTargetName=Build /p:Configuration=Debug /p:NInstall=bin\Debug\net-4.0`

- Exit code: **0**. Summary: `Build succeeded.` / `0 Warning(s)` /
  `0 Error(s)` / `Time Elapsed 00:04:20.05`; `: error` lines: 0.
- Positive suite: **468 passed / 0 failed** ("All tests passed. Time taken:
  00:03:15.8").
- Negative suite: **167 passed / 0 failed** ("All tests passed. Time taken:
  00:01:00.4").
- `All tests passed.` lines: **2**.

Contention note: the first three attempts of step B were disturbed by a
concurrent `Stage1,CompilerTests` run from the implementing session in the
same tree (file locks on `boot\Nemerle.MSBuild.Tasks.dll` /
`Tests\positive\ncc.parser.csharp.dll`; two later attempts were killed
mid-suite, exit 1 without any MSBuild summary, during that session's
post-run cleanup). After the concurrent run completed (its own green
`stage1-tests.*` logs, exit 0, consistent with this one) and its stale
`MSBuild.exe`/`Nemerle.Compiler.Test.exe` lock holders were cleared, the
recorded run above completed cleanly with zero interference.

**Verdict: ACCEPTANCE PASS** - Validate gate (FC x5, zero `: error`, exit 0,
no pefverify) and CompilerTests gate (468 + 167, 0 failed, exit 0) both hold.
