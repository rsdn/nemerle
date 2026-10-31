# Progress 6.2b — Core default references (compiler-side)

Implements decision **Q1** of
`retarget-compiler-to-net10-plan-report6.2a-default-refs.md`: when `ncc` runs
on a .NET (Core) host and stdlib loading is enabled, the implicit reference
set must become the active shared-framework directory (minus `System.Private.*`
and native shims), giving the zero-`-r` compile UX that FX `ncc` has
(`mscorlib`+`System`), while the FX branch stays byte-identical.

## Intentions

1. Single code change: `ncc/frontend/ManagerClass.n`, `LoadExternalLibraries`
   stdlib branch (after the four `load(typeof(...))` calls, ~:561-570):
   - Gate with the same host probe idiom as
     `CompilationOptions.IsRunningOnNetCore`:
     `typeof(object).Assembly.FullName.StartsWith("System.Private.CoreLib")`
     (on FX this is `mscorlib` → branch inert, master-safe).
   - Enumerate `IO.Path.GetDirectoryName(typeof(object).Assembly.Location)`
     (the ACTIVE `Microsoft.NETCore.App\<ver>` dir, correct under roll-forward)
     for `*.dll`, sorted with `StringComparer.Ordinal`.
   - Skip by name: `System.Private.*` prefix (already ingested via `typeof`,
     avoids impl+facade double-ingest), native shims per report: exact
     `{coreclr, hostpolicy, clrjit, clretwrc, clrgc, clrgcexp, mscorrc, msquic,
     mscordbi}`, prefixes `{mscordaccore, Microsoft.DiaSymReader.Native}` and
     suffix `.Native`. Everything else (`mscorlib`, `netstandard`, `System*`,
     `Microsoft.CSharp/VisualBasic*/Win32.*`, `WindowsBase`) loads with
     `isUserReference = false`.
   - Each load in its own swallow-all try/catch (the surrounding `tryEx`
     rethrows outside IntelliSense mode, so it cannot be reused): the dir
     contains native dlls; `BadImageFormatException` and any other load error
     must never be fatal for default refs.
   - `Options.References` loop (:580-581, `isUserReference = true`) stays
     AFTER, unchanged; `-no-stdlib` branch and `LoadNemerleMacros` untouched.
   - Dedup with explicit `-r` repeating a default path: pre-existing FX
     behavior (`LibraryReferenceManager.AddLibrary`), unchanged per report
     risk (3).
2. Acceptance (report Q4 adapted; class B 14 and class F 3 test failures are
   6.2c/6.2d scope and are expected to REMAIN):
   - A. `./build.ps1 -Targets Stage3,Validate -Rebuild` → fixed point ×5,
     zero `: error`, exit 0. All stage compiles pass `-no-stdlib`, so the new
     branch is inert during Stage1-4 (runtime behavior, invisible to stage IL);
     FC expected from first rebuild. Log `.tmp\phase62b\validate.log`.
   - B. Zero-`-r` standalone compiles with fresh Stage3 `ncc-core.exe`:
     `hello.n` (Console), a Regex-using and a Linq-using snippet → exit 0
     each; negative control `-no-stdlib` still fails fast.
   - C. `./build.ps1 -Targets Stage1,CompilerTests`: positives expected to
     jump 67 → ~380-450 (class A resolved; class B 14 red until 6.2c, class F
     3 until 6.2d); negatives mostly green. Quote totals + failure clusters.
     Log `.tmp\phase62b\tests.log`.
   - D. `git diff --stat`: only `ncc/frontend/ManagerClass.n` (+ this doc).
   - If C's positives do NOT rise materially → stop and report exact new
     error signatures (would falsify the report).
3. Constraints: do not commit; one build at a time; targeted edits only,
  preserve CRLF/no-BOM of `ManagerClass.n`; scratch only under
  `.tmp\phase62b\`. Cross-branch: this is shared-surface and a no-op on FX via
  the host gate — cherry-pick to master later (out of scope here).

## Results

### A. Green gate — `./build.ps1 -Targets Stage3,Validate -Rebuild` → PASS

Log `.tmp\phase62b\validate.log`, last line `EXIT=0`, zero `: error` lines
(zero `error MSB`/`error <code>` at all), and 5 × `FC: no differences
encountered` (IL fixed point Stage2↔Stage3 for `ncc-core`,
`Nemerle.Compiler`, `Nemerle`, `Nemerle.Macros`, `Nemerle.MSBuild.Tasks`),
`Build succeeded`. As predicted in 6.2a Q4, the new branch is inert during
the stages (all compile `-no-stdlib`), so the fixed point held from the first
rebuild.

### B. Zero-`-r` standalone compiles (fresh Stage3 `ncc-core.exe`) — PASS

Run from `.tmp\phase62b` with
`$ncc = 'C:\RSDN\nemerle\bin\Debug\net-4.0\Stage3\ncc-core.exe'`:

| Command | Exit |
|---|---|
| `& $ncc hello.n` (Nemerle.IO.printf → System.Console) | `EXIT(hello)=0` |
| `& $ncc regex-use.n` (`System.Text.RegularExpressions.Regex`) | `EXIT(regex)=0` |
| `& $ncc linq-use.n` (`System.Linq` `Where/Select/Sum` on array) | `EXIT(linq)=0` |
| `& $ncc -no-stdlib hello.n` (negative control) | `EXIT(no-stdlib)=-2146232797` — ICE `internal type System.Void not found` at `InitSystemTypes`, i.e. fails fast, branch inert ✓ |

### C. Suite run — `./build.ps1 -Targets Stage1,CompilerTests` (`tests.log`)

* Positive: **`100 tests passed, 368 tests failed`** (baseline 67/401).
  Breakdown of the 368:
  * **361 × `run......failed` → `Test finished with exit code -532462766`**
    (0xE0434352) — class B mscoree-shim run-launch, 6.2c scope; **all 361
    carry exactly this one signature**.
  * 7 × compile-stage, FX-only content (0 default-refs related):
    `form.n` (`Failed to resolve reference 'System.Windows.Forms'`),
    `access-checks.n` (`System.Web.*` unbound), `security.n`
    (`PermissionSet is not an attribute class`), `Issue-git-0507.n`
    (`FileIOPermission(SecurityAction.Demand)` attribute not found) — these 4
    are the class-F family; plus `codedom.n` (System.CodeDom types absent on
    Core), `resource.n` (FX-only Resource-macro typing), `serialize.n`
    (formatter-serialization obsolete-warning drift) — previously masked
    inside class A.
  * Class A is **fully eliminated**: zero `unbound name 'Console'` /
    `referenced namespace 'System...' does not exist` errors in the log;
    `compile...failed` count = 0. 461 of 468 tests now compile (baseline 81).
* Negative (manual run of the `NemerleAll.nproj:396` command line — the
  positive Exec fails MSB3073 and aborts MSBuild before the negative Exec,
  same procedure as 6.1): **`166 tests passed, 1 test failed`**
  (baseline 154/13). The single failure is `external-extensions.n:
  run......failed / Test finished with exit code -532462766` — class B.

**Count reconciliation (the one correction to the plan's arithmetic):**
6.1's "class B = 14" counted only the tests that could reach `run` while the
default-ref bug was active; the true B population is every `BEGIN-OUTPUT`
test, 361 of which were masked inside class A. So "positives → ~380-450"
materialized as: +380 tests moved from compile-fail to compile-clean, of
which 33 became outright passes (compile-only tests) and 361 are held at
`run` purely by the 6.2c shim-launch defect. After 6.2c the positives should
be ~461 passed / 7 failed (the 7 FX-only content tests above, of which the
3 planned `// NO-CORE` skips in 6.2d cover form/access-checks/security; the
other 4 need the same per-test disposition — 6.2a Q3's "465+3 norm"
underestimated this remainder by 4). The report's Q1 mechanism is confirmed,
not falsified: the failure clusters are exactly "B's 0xE0434352 shim +
F/FX-only content", as the gate anticipated.

### D. Diff scope — PASS

`git diff --stat`: `ncc/frontend/ManagerClass.n | 27 +++++++++++`
(1 file changed, 27 insertions); the only other repository change is the new
progress doc. No commits made.

### Implemented change (final form)

`ncc/frontend/ManagerClass.n`, stdlib branch of `LoadExternalLibraries`,
after the four `load(typeof(...))` calls: host gate
`typeof(object).Assembly.FullName.StartsWith("System.Private.CoreLib")`
(same idiom as `CompilationOptions.IsRunningOnNetCore`; false on FX ⇒ branch
inert, byte-identical FX path), then
`IO.Directory.GetFiles(dirname, "*.dll")` sorted with
`StringComparer.Ordinal`, skipping `System.Private.*` prefix, native names
(exact `coreclr/hostpolicy/clrjit/clretwrc/clrgc/clrgcexp/mscorrc/msquic/
mscordbi`, prefixes `mscordaccore`/`Microsoft.DiaSymReader.Native`, suffix
`.Native`), each `AddLibrary(dll, isUserReference=false)` in its own swallow
try/catch (BadImageFormat etc. never fatal). `Options.References` loop and
`-no-stdlib` branch untouched. CRLF/no-BOM preserved (850→877 CRLF lines).

