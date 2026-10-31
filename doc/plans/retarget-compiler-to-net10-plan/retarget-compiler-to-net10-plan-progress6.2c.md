# Progress 6.2c — BEGIN-OUTPUT run-launch on Core (class B)

Implements decision **Q2, halves 1+2** of
`retarget-compiler-to-net10-plan-report6.2a-default-refs.md` (validated
end-to-end by the report author): the 361 positive + 1 negative BEGIN-OUTPUT
tests that now compile cleanly (6.2b) still die at `run` with exit
`-532462766` (0xE0434352) — the harness `Process.Start`s the bare managed
`<test>.exe`, and Windows runs it on the FX mscoree shim, which cannot load
`System.Private.CoreLib`.

## Intentions

1. **Exec lines (`NemerleAll.nproj:394,396`, CompilerTests target only):**
   append the harness CLI switches `-r dotnet -rp exec` exactly per report Q2.
   - Switch spellings verified in `Main.n` against the Getopt option table:
     `-r`/`-runtime` — help text `"use this .Net runtime engine (default : none) "`;
     `-rp`/`-runtime-params` — help text `"parameters passed to the .Net runtime (default : none)"`.
     Both are `CliOption.String`, and `Getopt.Parse` (`lib/getopt.n:268-270`,
     `need_following_arg` at :207-218) consumes the NEXT token as the value
     when the option token has no `:`/`@` — so `-r dotnet` → runtime
     `"dotnet"`, `-rp exec` → runtime args `["exec"]`.
   - `Main.n:155-159` then selects `RuntimeProcessStartInfoFactory("dotnet",
     ["exec"])` → `ProcessStartInfo{ FileName = "dotnet"; Arguments = "exec
     <test>.exe" }` (`RuntimeProcessStartInfoFactory.n:23-35`), i.e. the run
     becomes `dotnet exec <test>.exe` — hostfxr, not the mscoree shim.
   - **Value choice (documented per task):** literal `dotnet` (NOT an absolute
     path) and literal `exec`. `exec` is hostfxr's *subcommand*, never a path;
     `dotnet` is resolved by `CreateProcess` from PATH (`UseShellExecute =
     false`, Windows search order includes PATH). The report validated exactly
     `-r dotnet -rp exec` end-to-end ("the mechanism exists … hostfxr accepts
     the `.exe` name, verified"), and the build host provably has `dotnet` on
     PATH (build.ps1 itself shells to dotnet/vswhere). Keeping the literal
     strings preserves the report's validated configuration; an absolute path
     would hard-code a machine-specific location into the nproj.
   - Only the two CompilerTests Exec lines change; other Exec invocations of
     the same exe family keep FX-legacy direct launch (no `-r`).
2. **Harness (`snippets/Nemerle.Test/Nemerle.Compiler.Test/NccTest.n`):** in
   `compile()`, the `-t:exe` branch (the report's :192-197, the
   `description.HasExpectedOutput` path), write
   `Path.ChangeExtension(exePath, ".runtimeconfig.json")` next to the produced
   exe when the host is Core. Without it `dotnet exec` dies with hostpolicy
   fatal `-2147450749` (verified in the report). JSON shape exactly as the
   report's validated content:
   `{"runtimeOptions":{"framework":{"name":"Microsoft.NETCore.App","version":"<major>.0.0"}}}`
   — version derived from the harness process itself
   (`Environment.Version.Major`, the same TFM the apphost runs on → "10.0.0"
   on .NET 10; default Minor roll-forward lets any installed 10.x satisfy it).
   Core gate = the established probe idiom
   `typeof(object).Assembly.FullName.StartsWith("System.Private.CoreLib")`
   (ManagerClass 6.2b / `CompilationOptions.IsRunningOnNetCore`); on FX the
   writer is inert and the FX path is unchanged.
3. **Scope guards:** do NOT touch `ManagerClass.n`; no `NO-CORE` markers /
   `NccTestDescription.n` changes (6.2d). Do not commit. One build at a time;
   scratch only under `.tmp\phase62c\`; targeted edits preserving
   UTF-8-BOM/CRLF (`NccTest.n`) and the nproj's existing encoding/EOL.
4. **Acceptance:**
   - A. `./build.ps1 -Targets Stage1,CompilerTests` → log
     `.tmp\phase62c\tests.log`. Positives expected ~461 passed / 7 failed, the
     7 exactly the FX-only-content family {form.n, access-checks.n,
     security.n, Issue-git-0507.n, codedom.n, resource.n, serialize.n};
     ZERO 0xE0434352 clusters left. The 7 failures abort MSBuild before the
     negative Exec (same as 6.2b), so negatives run by the manual
     `NemerleAll.nproj:396` command line → `.tmp\phase62c\tests-negative.log`,
     expected 167/0 incl. `external-extensions.n`.
   - B. Any run-test failures for NEW reasons: classify with signatures, do not
     chase beyond the two halves.
   - C. FC sanity (cheap): harness/nproj changes are outside the FC set, so
     instead of full Rebuild: `./build.ps1 -Targets Stage1` exit 0 AND
     `bin\Debug\net-4.0\Stage1\ncc-core.exe -version` exit 0. Full
     Rebuild+Validate fixed-point proof happens in the final acceptance atom.
   - D. `git diff --stat`: NemerleAll.nproj + harness sources + this doc only.

## Results

### A. Suite run — `./build.ps1 -Targets Stage1,CompilerTests` (`tests.log`)

* Positive: **`453 tests passed, 15 tests failed`** (6.2b baseline 100/368).
  The 361-test class-B cluster is GONE: zero 0xE0434352 except ONE surviving
  line, and representative former-class-B tests now pass, e.g.
  `testsuite\positive\tyvars.n: run......passed` (log:1434; the smoke run had
  already shown it exit 0).
* The 15 failures:
  * The 7 expected FX-only content tests, verbatim (all compile-stage,
    identical to the 6.2b classification):
    `form.n: ...failed` (:1225, `Failed to resolve reference
    'System.Windows.Forms'`), `access-checks.n: ...failed` (:1022,
    `System.Web.*` unbound), `security.n: ...failed` (:1399, CAS attributes),
    `Issue-git-0507.n: ...failed` (:1001, `the custom attribute
    'FileIOPermission(SecurityAction.Demand)' could not be found or is
    invalid`), `codedom.n: ...failed` (:1158, `unbound name
    'CodeTypeDeclaration'` + System.CodeDom resolve warnings), `resource.n:
    ...failed` (:1372), `serialize.n: ...failed` (:1402).
  * **8 run-stage failures, ALL NEW and classified — see B.** None is a
    two-halves plumbing gap: 7 × Core runtime-behavior drift, 1 × test-fixture
    output collision exposed by Core entry binding (the single surviving
    0xE0434352).
* The positive Exec's non-zero exit aborted MSBuild before the negative Exec
  (same as 6.1/6.2b), so negatives were run standalone with the exact
  `NemerleAll.nproj:396` command line (`-r dotnet -rp exec`, from
  `bin\Debug\net-4.0\Tests\negative`) → `.tmp\phase62c\tests-negative.log`:
  **167 passed / 0 failed**, `All tests passed.` (exit 0), incl.
  `testsuite\negative\external-extensions.n: run...........passed` — the one
  class-B negative from 6.2b.

### B. Classification of the 8 unexpected run failures (report-and-stop per spec B)

| Test | Cluster | Evidence |
|---|---|---|
| `Issue-git-0297.n` | fixture collision (see below) | `Test finished with exit code -532462766` — the ONLY surviving 0xE0434352 |
| `assert.n` | Core `ArgumentException.Message` format | `...(49,9,49,16). (Parameter '_o')` vs expected `...` + `Parameter name: _o` |
| `notnullorempty.n` | same | `(Parameter 's')` vs `Parameter name: s` |
| `Issue-git-0053.n` | Core `Expression.ToString` | `Lambda(body='(x - Convert(2, Int64))')` vs `'...(Convert(2))'` |
| `Issue-git-0274.n` | Core double `ToString` (IEEE shortest, .NET Core 3.0+) | `Cot(42) ==> 0.4364167060752729` vs `0.436416706075273` |
| `basic-value-types.n` | same (float) | `5.4000005447844055OK` vs `5.40000054478441OK` |
| `linq-2-ExprTree.n` | same (double) | `10.799999999999999` vs `10.8` |
| `overloading.n` | same (double) | `61.199999999999996` vs `61.2` |

The 7 non-0297 rows are category (iii) NEW Core runtime-behavior differences
(expected-output text vs .NET Core 3.0+ number formatting / exception message /
expression-tree ToString) — **6.4 checklist**, no plumbing relation; the launch
itself works (exit 0, outputs produced, only text differs).

**`Issue-git-0297.n` (the surviving 0xE0434352) — diagnosed, NOT a two-halves
gap.** A/B reproduction in the real `Tests\positive` dir with the fresh
harness (same `-r dotnet -rp exec`, harness-written runtimeconfig):

```text
WITHOUT sibling dll: dotnet exec .\Issue-git-0297.exe → EXIT=0, output "OK"
WITH sibling dll:    dotnet exec .\Issue-git-0297.exe → EXIT=-532462766
  Unhandled exception. System.MissingMethodException:
  Entry point not found in assembly 'Issue-git-0297, Version=0.0.0.0, ...'
```

Cause: `testsuite\positive` contains BOTH `Issue-git-0297.n` (BEGIN-OUTPUT →
`-t:exe Issue-git-0297.exe`) and `Issue-git-0297.cs` (compile-only →
`Issue-git-0297.dll`). Core binds the entry assembly BY SIMPLE NAME and the
app-dir probe prefers `.dll` over `.exe`, so the `.cs` test's library output
shadows the run exe's entry point; the FX mscoree shim loaded the exe by path
and was immune. A suite-wide scan finds exactly ONE such
`.n(BEGIN-OUTPUT)+.cs` same-basename pair (this one) and none in negatives.
Disposition: test-fixture defect (output-name collision), 6.4 checklist
(rename one fixture or namespace outputs by extension); the two halves are
provably correct with the collision removed, so no in-scope fix per spec B.

### C. FC sanity (cheap, per task) — PASS

`./build.ps1 -Targets Stage1` → `STAGE1_EXIT=0`, log ends `Build succeeded.`;
`bin\Debug\net-4.0\Stage1\ncc-core.exe -version` →
`Nemerle Compiler (ncc) version 10.0.37.0 (git)`, `NCCVER_EXIT=0`. Harness and
nproj changes are outside the FC set (no compiler-source change); the full
Rebuild + Validate fixed-point proof is scheduled in the final acceptance
atom.

### D. Diff scope — PASS

`git diff --stat` (excluding the new progress doc):
`NemerleAll.nproj | 11 +++++++--` and
`snippets/Nemerle.Test/Nemerle.Compiler.Test/NccTest.n | 27 ++++++++++++++++++-`
(2 files changed, 35 insertions(+), 3 deletions(-)). Encoding/EOL preserved
(both still UTF-8 BOM + CRLF). No commits made.

### Implemented change (final form)

1. `NemerleAll.nproj` CompilerTests Exec lines (+ explanatory comment): both
   the positive and negative harness invocations now carry `-r dotnet
   -rp exec`. Verified against `Main.n` Getopt table (`-r`/`-runtime`
   "use this .Net runtime engine", `-rp`/`-runtime-params` "parameters passed
   to the .Net runtime") and `lib/getopt.n:268-270` (String options consume
   the following token), producing
   `RuntimeProcessStartInfoFactory("dotnet", ["exec"])` →
   `dotnet exec <test>.exe` (hostfxr) instead of the mscoree shim. Chose the
   literal `dotnet`/`exec` exactly as the report validated: `exec` is the
   hostfxr subcommand (not a path); `dotnet` is resolved from PATH
   (`C:\Program Files\dotnet\dotnet.exe` confirmed, SDK 10.0.402) — no
   machine-specific absolute path baked into the nproj. Other Exec invocations
   of the exe family untouched.
2. `NccTest.n`: new `writeRuntimeConfigForCoreHost(exeFilePath)` called from
   the `HasExpectedOutput` (`-t:exe`) branch of `compile()`; when
   `typeof(object).Assembly.FullName.StartsWith("System.Private.CoreLib")`
   (the 6.2b/IsRunningOnNetCore probe idiom; false on FX ⇒ nothing written),
   writes `<test>.runtimeconfig.json` with exactly the report's validated
   shape, version derived from the harness process:
   `{"runtimeOptions":{"framework":{"name":"Microsoft.NETCore.App","version":"$(Environment.Version.Major).0.0"}}}`
   → observed on disk: `...version":"10.0.0"}`. `ManagerClass.n` untouched;
   no NO-CORE markers (6.2d).

