# Progress 6.2d — `// NO-CORE` test marker (FX-only-content tests not-run on Core)

Implements decision **Q3** of
`retarget-compiler-to-net10-plan-report6.2a-default-refs.md` as scoped by the
orchestrator (7 files, not the report's initial 3): the 7 compile-stage
positive failures that are FX-only *content* (WinForms / System.Web / CAS /
in-box CodeDom / FX resource-stream metadata / BinaryFormatter) can never pass
on a Core host and are not 6.4 output-drift work. Instead of leaving them red,
the harness learns a third content marker, `// NO-CORE`, mirroring the
existing `// NO-TEST` handling **exactly**: `NccTestDescription.Parse` sets a
flag, `NccTest.parseStep` reports `Result.Skipped` via
`_log.GetNotRunResult(...)` (the same "not a test" path; the listener counts
Skipped into `_notRun`, no exit-code effect).

## Intentions

1. **Harness parsing (`snippets/Nemerle.Test/Nemerle.Compiler.Test/NccTestDescription.n`):**
   new `NoCore : bool` property + match arm `.*NO-CORE.*` next to the existing
   `.*NO-TEST.*` / `.*NO-VERIFY.*` arms. NO-TEST's regex is NOT touched; the
   two patterns are not substrings of each other (`NO-CORE` ∌ `NO-TEST`), so
   master's FX-only harness never reacts to the marker.
2. **Host gate (`snippets/Nemerle.Test/Nemerle.Compiler.Test/NccTest.n`):**
   parseStep checks `description.NoCore && IsRunningOnCoreHost` where
   `IsRunningOnCoreHost` = the established probe idiom
   `typeof(object).Assembly.FullName.StartsWith("System.Private.CoreLib")`
   (6.2b/6.2c, `CompilationOptions.IsRunningOnNetCore`). On .NET Framework the
   probe is false ⇒ NO-CORE has ZERO effect and the test runs (master-safe).
   Reporting reuses `GetNotRunResult` with message "not a test on core" — no
   new reporting path.
3. **Marker lines** (one comment line at the top of each file, reason text
   without ':' so no `REFERENCE:`/`OPTIONS:`/`E:` pattern can match it; the
   literal substring NO-TEST appears nowhere):

   | file | recorded Core failure (6.2c tests.log) |
   |---|---|
   | `testsuite/positive/form.n` | :1226 `Failed to resolve reference 'System.Windows.Forms'.` |
   | `testsuite/positive/access-checks.n` | :1024 `unbound type name \`System.Web.UI.Page'` |
   | `testsuite/positive/security.n` | :1401 `\`System.Security.PermissionSet' is not an attribute class` |
   | `testsuite/positive/Issue-git-0507.n` | :1003 `the custom attribute \`FileIOPermission(SecurityAction.Demand)' could not be found or is invalid` |
   | `testsuite/positive/codedom.n` | :1166 `unbound name \`CodeTypeDeclaration'` (System.CodeDom resolve warnings :1159-1174) |
   | `testsuite/positive/resource.n` | :1386 `typing fails on ambiguity between overloads: StreamReader..ctor(stream : System.IO.Stream)` (see note) |
   | `testsuite/positive/serialize.n` | :1404 unexpected `warning N618: System.Runtime.Serialization.IObjectReference is obsolete: 'Formatter-based serialization is obsolete...'` |

   **resource.n note:** the task's shorthand says "resx Resource macro is
   FX-only (checklist 7.5)", but the test does not use the Resource macro; the
   recorded failure is StreamReader overload-resolution ambiguity against
   `Assembly.GetManifestResourceStream` — Core's StreamReader metadata (the
   optional-parameter `(Stream, Encoding, bool, int, bool)` ctor, absent in
   this shape on FX where the method returned `UnmanagedMemoryStream`) makes
   the 1-arg call ambiguous. Still deterministic, compile-stage, FX-only BCL
   surface — same family as 6.2b/6.2c classified; disposition unchanged.
   **serialize.n note:** compilation itself succeeds on Core; the test fails
   because Core marks the BinaryFormatter-era surface (IObjectReference)
   `[Obsolete]` and the harness rejects the unexpected warning — exactly the
   stated "BinaryFormatter/CAS era surface" reason.
4. **Not touched:** `Issue-git-0297.n`, the 7 output-drift tests (6.4),
   `NemerleAll.nproj`, `testsuite/test.n` (legacy harness keeps only NO-TEST).
5. **Acceptance:**
   - A. `./build.ps1 -Targets Stage1,CompilerTests` → `.tmp\phase62d\tests.log`:
     positives **453 passed, 8 failed** (0297 + the 7 drifts), **7 tests
     reported not-run** with "not a test on core". MSBuild aborts at the
     positives Exec (pre-existing since 6.2c), so negatives run standalone with
     the `NemerleAll.nproj:403` command line → expect 167/0.
   - B. FX-safety static proof: host-gated consumer code quoted; NO-TEST regex
     untouched.
   - C. `git diff --stat`: 2 harness files + 7 test files + this doc. No commit.

## Results

### A. Suite run — `./build.ps1 -Targets Stage1,CompilerTests` (`tests.log`)

* Positive: **`453 tests passed, 8 tests failed. Time taken: 00:03:42.6132466`**
  (tests.log:1787; harness on Core, `Runtime version: 10.0.13` :1229) —
  exactly the 6.2c baseline minus the 7 FX-only tests. The 8 failures are
  precisely `Issue-git-0297.n` + the 7 output-drift tests (6.4 scope):
  `Issue-git-0053, Issue-git-0274, Issue-git-0297, assert, basic-value-types,
  linq-2-ExprTree, notnullorempty, overloading` (all `run...failed`).
* The 7 NO-CORE tests, verbatim (the whole not-run set):

  ```text
  :1372  ...positive\Issue-git-0507.n: .......................................not a test on core
  :1391  ...positive\access-checks.n: ........................................not a test on core
  :1521  ...positive\codedom.n: ..............................................not a test on core
  :1572  ...positive\form.n: .................................................not a test on core
  :1718  ...positive\resource.n: .............................................not a test on core
  :1723  ...positive\security.n: .............................................not a test on core
  :1724  ...positive\serialize.n: ............................................not a test on core
  ```

  (counted: pos-passed=453, pos-failed=8, pos-notrun=7)
* As since 6.2c, the positive Exec's exit 1 aborted MSBuild before the
  negative Exec (`error MSB3073`, tests.log:1788), so negatives were run
  standalone with the exact `NemerleAll.nproj:403` command line from
  `bin\Debug\net-4.0\Tests\negative` (`-r dotnet -rp exec
  -output:. "-p:-nowarn:10003 -def:NET_4_0;DEBUG"`) →
  `.tmp\phase62d\tests-negative.log`: **167 passed / 0 failed**
  (`All tests passed. Time taken: 00:01:04.6019730` :191, exit 0; zero
  not-run lines — no NO-CORE markers in negatives).

### B. FX-safety static proof — PASS

* Host gate (`NccTest.parseStep`):
  `else if (description.NoCore && IsRunningOnCoreHost)` with
  `IsRunningOnCoreHost = typeof(object).Assembly.FullName.StartsWith("System.Private.CoreLib")`
  — false on .NET Framework ⇒ NO-CORE tests compile and run there unchanged;
  only the Core branch reaches `GetNotRunResult`.
* `.*NO-TEST.*` / `.*NO-VERIFY.*` lines untouched (diff shows them only as
  context; the only `+` lines in NccTestDescription.n are the `NoCore` property
  and the `.*NO-CORE.*` arm).
* Verified programmatically: none of the 7 marker lines matches any
  pre-existing parse pattern (no `E:`/`W:`/`H:`/`GE:`/`GW:`/`OK`/`REFERENCE:`/
  `MACRO:`/`OPTIONS:`/`PKG-CONFIG:`/NO-TEST/NO-VERIFY hit — marker reasons are
  colon-free by construction), and `.*NO-TEST.*` does not match any NO-CORE
  line (no substring containment either way). Master's harness therefore
  ignores the markers entirely; `testsuite/test.n` (legacy harness) untouched.

### C. Diff scope — PASS

`git diff --stat`: `NccTest.n` (+13), `NccTestDescription.n` (+2), the 7 test
files (+1 each; codedom.n shows `-`/`+` on its BOM-carrying first line, BOM
preserved) — plus the new progress doc (untracked). `doc/plans/retarget-
compiler-to-net10-checklist.md` was already modified in the working tree
before this atom (pre-existing, not touched here). Encodings/EOLs preserved
(LF stays LF, Issue-git-0507.n CRLF, codedom.n/NccTest*.n BOMs intact).
No commits made.

### Note (task-mandated reporting)

* **resource.n**: marked as instructed, but the recorded Core failure is NOT
  the Resource macro — quoted evidence and analysis in Intentions §3.
  **serialize.n**: fails via the unexpected-obsolete-warning path on the
  BinaryFormatter-era surface (`IObjectReference`), i.e. exactly the stated
  reason; compilation itself succeeds on Core.
