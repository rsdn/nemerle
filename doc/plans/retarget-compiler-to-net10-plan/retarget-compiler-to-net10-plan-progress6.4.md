# Progress 6.4 — Resolve the 8 residual compiler-suite failures on Core

Steady state before this atom (verified in 6.2c/6.2d):
`./build.ps1 -Targets Stage1,CompilerTests` on Core → positives **453 passed /
8 failed / 7 not-run**, negatives **167/0**. The 8 failures sit in the CI
allowlist `testsuite/ci-known-failures-64.txt` (harness option
`-known-failures`, 6.3). None of the 8 is a compiler bug: 1 is a test-fixture
output-name collision exposed by Core entry binding, 7 are literal BEGIN-OUTPUT
expectations vs Core BCL formatting (double `ToString`,
`ArgumentException.Message`, `Expression.ToString`). This atom resolves all 8;
afterwards the allowlist is comments-only and CI runs the suite with zero
allowlisted failures.

## Mechanism audit (what the harness can express)

* `NccTest.testOutputAssembly` (`NccTest.n:386-399`) compares expected vs
  actual output **line-by-line, literal** (`string.Equals(Trim, Trim,
  Ordinal)`). The `// E:`/`W:`/... expectations ARE regexes
  (`NccTestDescription` stores `Dictionary[int, Regex]`), but BEGIN-OUTPUT
  blocks are NOT — so "widen the regex" (task option c) does not exist for
  these 7 tests; a drifted line can only be replaced wholesale.
* `NccTestDescription.Parse` had no conditional-expectation syntax (task
  option a: none existed).
* `regexp match` (macros/text.n:160) wraps **every** arm pattern as
  `(?<id>^PATTERN$)` — full-line match. This is the linchpin of the chosen
  mechanism: a marker line `BEGIN-OUTPUT-CORE` cannot match master's
  `^BEGIN-OUTPUT` arm (full match would require the line to BE
  "BEGIN-OUTPUT"), and the marker contains no `//`, `REFERENCE:`,
  `MACRO:`, `OPTIONS:`, `PKG-CONFIG:`, `NO-TEST`, `NO-VERIFY` substring, so
  every other inner arm is inert on it too. Same for `END-OUTPUT-CORE`.
  Master's FX harness therefore ignores both markers and any lines placed
  between them (they sit outside every block it recognizes, so they fall to
  `_ => ()`).

## Intentions

1. **`Issue-git-0297.n` (fixture collision) — rename the C# fixture.**
   `testsuite/positive` contained `Issue-git-0297.n` (BEGIN-OUTPUT →
   `Issue-git-0297.exe`) and the unrelated `Issue-git-0297.cs` (compile-only →
   `Issue-git-0297.dll`); `dotnet exec <exe>` binds the entry assembly by
   SIMPLE NAME and the app-dir probe prefers `.dll` → MissingMethodException
   (6.2c repro). The two files never reference each other (grep: no reference
   to `Issue-git-0297.dll` anywhere), so `git mv
   testsuite/positive/Issue-git-0297.cs → Issue-git-0297-cs.cs` is the
   smallest fix: the `.n` keeps its identity (it is the actual issue
   repro/entry-point test), the C# variant keeps compiling standalone with
   byte-identical content, and on FX the shim loaded exes by path so the name
   never mattered. Suite-wide scan re-verified: exactly one
   `.n(BEGIN-OUTPUT)+.cs` same-basename pair, none in negatives.
2. **7 output-drift tests — host-aware expectations (option a), enabled by
   the full-match proof above.** Extend the test format with paired blocks:

   ```
   BEGIN-OUTPUT
   ...expected output on .NET Framework (unchanged, master's format)...
   END-OUTPUT
   BEGIN-OUTPUT-CORE
   ...expected output on .NET (Core), captured from the real Core run...
   END-OUTPUT-CORE
   ```

   Harness (`NccTestDescription.Parse` only): read `BEGIN-OUTPUT-CORE` blocks
   into a separate list; after parsing, **on a Core host only** (the
   established probe idiom `typeof(object).Assembly.FullName.StartsWith
   ("System.Private.CoreLib")`) and **only if** the file had at least one such
   block, replace `ExpectedOutput` with it. On FX the list is computed and
   discarded — the FX code path is byte-for-byte the old behavior.
   Convention (documented in the code): a `BEGIN-OUTPUT-CORE` block is always
   paired with a plain `BEGIN-OUTPUT` block, so FX (which ignores the Core
   block) still gets the full expectation; files without the new marker — all
   other 350+ run tests and every negative — parse exactly as today.
   Core block contents come from captured `dotnet exec <test>.exe` output of
   the 6.2d-built exes in `bin\Debug\net-4.0\Tests\positive` (scratch
   `.tmp\phase64\actual-<test>.txt`); full captured-vs-expected diffs in the
   Results table below — every non-drift line is identical on both hosts, so
   the Core blocks keep the tests' full assertion value instead of NO-CORE'ing
   them (option b rejected for these 7: they DO have Core value; only a handful
   of format lines drift).
   Per-test disposition:
   * `assert.n`, `notnullorempty.n` — Core appends `(Parameter 'x')` inline
     instead of a trailing `Parameter name: x` line
     (`ArgumentException.Message` format change).
   * `Issue-git-0053.n` — `Expression.ToString` prints `Convert(2, Int64)`.
   * `Issue-git-0274.n` — double shortest-round-trippable `ToString`
     (`0.4364167060752729` vs `0.436416706075273`); single line.
   * `basic-value-types.n` — same double family (`5.4000005447844055OK`);
     single line.
   * `overloading.n` — same double family (`61.199999999999996` ×2); 2 lines.
   * `linq-2-ExprTree.n` — same double family inside one concatenated line
     (`10.799999999999999` ×3, `14.399999999999999`).
3. **Allowlist file (acceptance C):** `testsuite/ci-known-failures-64.txt`
   becomes comments-only (the 8 ids deleted; header rewritten to record that
   6.4 resolved them). Mechanism kept default-off: the nproj still gates the
   harness switch on `Condition=" '$(NKnownFailures)' != '' "` and CI keeps
   passing a file that exists with zero entries (`LoadKnownFailures` skips
   `#` comments; `FindKnownFailure` short-circuits on an empty list) — no
   yaml/nproj churn, no risk of a dangling `-known-failures` path.
4. **Not touched:** `ManagerClass.n`, all `ncc/`, `lib/`, `macros/` sources,
   `NccTest.n` (comparison code needs no change), `NemerleAll.nproj`, CI yaml,
   negatives (zero changes; 167/0 re-quoted). `testsuite/test.n` (legacy
   harness) not touched — it ignores unknown lines; BEGIN-OUTPUT-CORE would
   only add noise if someone revived it by hand (documented below).
5. **Audit (no builds):** grep compiler + lib + macros + testsuite for
   (i) culture-sensitive casing/comparison that Core's ICU globalization could
   change at compile time, (ii) `CodePagesEncodingProvider` /
   `Encoding.GetEncoding(<non-standard>)` (not in-box on Core), (iii) dnlib
   emit paths assuming FX `ModuleBuilder`/globalization behavior. Report as a
   table; fix only demonstrated failures.
6. **Acceptance:**
   * A. `./build.ps1 -Targets Stage1,CompilerTests` with NO `NKnownFailures`
     env (local default) → positives 461 passed / 0 failed / 7 not-run
     (the 6.2d NO-CORE seven; 461+7=468), negatives run inside the same
     build (the positive Exec no longer aborts MSBuild) → 167/0.
   * B. FX-safety: `regexp match` full-match proof (above) + per-line inert
     scan of every line added to a shared test file against master's parse
     patterns; harness change host-gated and no-op without the marker.
   * C. Allowlist comments-only, nproj condition quoted.
   * D. `./build.ps1 -Targets Stage3,Validate -Rebuild` → FC ×5, zero
     `: error`, exit 0 (`\.tmp\phase64\validate.log`); `git diff --stat`
     shows no compiler-source files.

## Audit (named risk areas; grep-only, no builds)

| # | Area / site | Evidence | Assessment | Action |
|---|---|---|---|---|
| i-1 | Lexer numeric literals | `ncc/.../Lexer.n:1058,1062` `Double.Parse(..., NumberFormatInfo.InvariantInfo)`; no other float/double parse in ncc/lib/macros | Invariant, ICU-proof | none |
| i-2 | Case-insensitive name/metadata comparisons | `Completion.n:134,150` ToUpper**Invariant**; `CompilationOptions.n:327`, `Parser.n:154-156`, `NemerleCodeGenerator.n:1358` Invariant; `ManagerClass.n:541-556,589` Ordinal (incl. the 6.2b `Array.Sort(StringComparer.Ordinal)`) | Invariant/Ordinal, ICU-proof | none |
| i-3 | Culture-sensitive `ToLower()` in ncc | `LibraryReference.n:129-132` (IVT publickey(-token) attr strings: ASCII hex+keywords only), `LibraryReferenceManager.n:212,310` (file extensions), `ClassMember.n:298,317` + `TypeBuilder.n:467` (enum-name diagnostics, compared only in-process), `Lexer.n:1005` (digit/hex peek) | Inputs are ASCII-only domains where NLS (FX) and ICU (Core) agree per code point (no `I`/`ß`-class chars in hex/extensions); no compile-result drift observed in the 628-test suite on Core | none (watch item) |
| i-4 | `Completion.n:108` `CmpOptions = InvariantCultureIgnoreCase` | IDE completion matching only; Core ICU CompareInfo differs from FX NLS for exotic chars | IDE-path cosmetic, never compile-semantic | none |
| i-5 | lib/ + macros/ compile-time casing | `git grep "ToLower()\|ToUpper()" -- lib macros` → ZERO hits | Clean | none |
| ii | CodePages / non-standard encodings | `git grep "GetEncoding\|CodePagesEncodingProvider\|Encoding.Default" -- ncc lib macros snippets/Nemerle.Test tools/msbuild-task` → ZERO hits (source files are read via `File.OpenText`/`StreamReader` defaults, UTF-8+BOM on both hosts; no ResX/BinaryFormatter resource path in ncc) | CodePages-not-in-box cannot bite | none |
| iii | dnlib emit vs FX ModuleBuilder/globalization | `GetModuleBuilder()` returns `DN.ModuleDef` (`TypesManager.n:140`) — the dnlib branch has NO `System.Reflection.Emit` ModuleBuilder use (only an old comment in TypesManager.n:36 and ncc/README); no `CultureInfo`/`LCID`/`Locale` in emit paths; nondeterministic MVID/GUID already excluded from the Validate IL-diffs (`ncc/frontend/Makefile:251`) | No FX-flavoured emit/globalization assumptions | none |

No demonstrated failing behavior found → nothing fixed (per task: report only).

## Results

### A. Suite run — `./build.ps1 -Targets Stage1,CompilerTests`, no allowlist
(`.tmp\phase64\tests.log`; `NKnownFailures` env was empty, so MSBuild passed
no `-known-failures` at all — default-off path)

* Positives (`tests.log:1715`): **`All tests passed. Time taken:
  00:03:41.3374603`** — counted from the log: **461 passed / 0 failed /
  7 not-run** (461+7 = all 468). The 7 not-run are exactly the 6.2d NO-CORE
  set (`Issue-git-0507, access-checks, codedom, form, resource, security,
  serialize` → "not a test on core", log:1358-1652).
* The 8 formerly-failing tests, verbatim (all now run+pass on Core):

  ```text
  :1291  testsuite\positive\Issue-git-0053.n:     run..............passed
  :1320  testsuite\positive\Issue-git-0274.n:     run..............passed
  :1327  testsuite\positive\Issue-git-0297.n:     run..............passed
  :1388  testsuite\positive\assert.n:             run..............passed
  :1400  testsuite\positive\basic-value-types.n:  run..............passed
  :1572  testsuite\positive\linq-2-ExprTree.n:    run..............passed
  :1611  testsuite\positive\notnullorempty.n:     run..............passed
  :1620  testsuite\positive\overloading.n:        run..............passed
  ```

* Negatives ran INSIDE the same build this time (the positive Exec exits 0,
  so MSBuild no longer aborts before the negative Exec; `tests.log:1906`):
  **`All tests passed. Time taken: 00:01:05.4683573`** — counted: **167 passed
  / 0 failed / 0 not-run**. `Build succeeded.` (:1907), `BUILD_A_EXIT=0`.

### Disposition table (the 8)

| Test | Disposition | Mechanism | Before → After |
|---|---|---|---|
| `Issue-git-0297.n` | renamed fixture | `git mv testsuite/positive/Issue-git-0297.cs → Issue-git-0297-cs.cs` (content byte-identical, 0 diff lines): removes the same-basename `.dll` that Core's simple-name entry binding preferred over the test's `.exe`; the two files never referenced each other | failed (`MissingMethodException`, 0xE0434352) → passed |
| `assert.n` | host-aware expectations (a) | paired `BEGIN-OUTPUT-CORE` block (14 lines; Core `ArgumentException.Message` keeps `(Parameter '_o')` inline instead of the FX `Parameter name: _o` line) | failed (output drift) → passed |
| `notnullorempty.n` | host-aware expectations (a) | same, 2 merged message lines | failed → passed |
| `Issue-git-0053.n` | host-aware expectations (a) | same, `Convert(2, Int64)` line | failed → passed |
| `Issue-git-0274.n` | host-aware expectations (a) | same, `0.4364167060752729` (IEEE-shortest double) line | failed → passed |
| `basic-value-types.n` | host-aware expectations (a) | same, `5.4000005447844055OK` line | failed → passed |
| `overloading.n` | host-aware expectations (a) | same, `61.199999999999996` ×2 | failed → passed |
| `linq-2-ExprTree.n` | host-aware expectations (a) | same, `10.799999999999999`/`14.399999999999999` line | failed → passed |

"widened regex" (c) was ruled out by the mechanism audit: BEGIN-OUTPUT
comparison is literal (`string.Equals(Trim,Trim,Ordinal)` in
`NccTest.testOutputAssembly`), only `// E:`-style compiler-message
expectations are regexes. NO-CORE (b) rejected for these 7: they have real
Core value (all non-drift lines still asserted).

### B. FX-safety — PASS

1. **Harness change is host-gated and marker-gated**
   (`NccTestDescription.Parse`):

   ```nemerle
   | "^END-OUTPUT-CORE" => isReadingOutputCore = false
   ...
   if (isReadingOutputCore)
     coreExpectedOutput.Add(line);
   else if (isReadingInput) ...
   else if (isReadingOutput)
     description.ExpectedOutput.Add(line);
   ...
   | "^BEGIN-OUTPUT"      => isReadingOutput = true; description.HasExpectedOutput = true
   | "^BEGIN-OUTPUT-CORE" => isReadingOutputCore = true; description.HasExpectedOutput = true
   ...
   when (typeof(object).Assembly.FullName.StartsWith("System.Private.CoreLib") && coreExpectedOutput.Count > 0)
   {
     description.ExpectedOutput.Clear();
     description.ExpectedOutput.AddRange(coreExpectedOutput);
   }
   ```

   On .NET Framework the `when` probe is false ⇒ `ExpectedOutput` is exactly
   the plain BEGIN-OUTPUT lines (the old path); files without the new marker
   parse line-identically on both hosts.
2. **Markers are inert for master's FX parser by construction**: `regexp
   match` compiles every arm as `^ARM$` (full match; `macros/text.n:160
   pat.Append("(?<" + nsymb.Id + ">^" + newstr + "$)")`), so a line
   `BEGIN-OUTPUT-CORE` cannot match the `^BEGIN-OUTPUT` arm, and contains no
   `//`/`REFERENCE:`/`NO-TEST`/`NO-VERIFY` substring for the substring arms.
   The legacy `testsuite/test.n` parser uses the identical pattern set and is
   inert likewise.
3. **Empirical scan** (`.tmp\phase64\fx-inert-scan.ps1`): all **170 added
   lines** across the 7 shared test files matched **zero** pre-existing parse
   patterns under full-match semantics (`violations: 0`).
4. **Rename**: content-identical (`0` changed lines in `git diff --stat`);
   nothing in the repo references `Issue-git-0297.dll` (grep), and the FX
   shim loaded the test `.exe` by path, so the fixture name never mattered
   there.
5. Implementation notes: statement-level `if` without `else` is a parse error
   in this grammar (probe-verified), hence the `when` idiom (already the
   house style); the list is `def coreExpectedOutput : List[string] = List();`
   (the codebase's mutable-list idiom). One pre-existing LF-only line inside
   the edited hunk region of the mixed-EOL `NccTestDescription.n` became
   CRLF (visible as the `-`/`+` pair of the identical `^BEGIN-OUTPUT` arm);
   `git diff --ignore-cr-at-eol` shows the change as pure additions
   (+31/-1, the `-`/`+` being the `if (isReadingInput)` →
   `else if (isReadingInput)` restructure). All test-file diffs are pure
   additions with BOM/EOL preserved (verified byte-wise per file).

### C. Allowlist — comments-only, mechanism kept (recorded choice)

* `testsuite/ci-known-failures-64.txt` now contains ONLY the header comment
  (all 8 ids deleted; header records "6.4 RESOLVED ALL 8 ENTRIES").
* Chose "empty to comments-only" over delete-file: the CI yaml step still
  sets `$env:NKnownFailures = 'ci-known-failures-64.txt'` and the file exists
  with zero entries ⇒ `-known-failures` is passed but loads no ids
  (`LoadKnownFailures` skips `#` lines; `FindKnownFailure` short-circuits
  `if (_knownFailures.Count == 0) option.None()`), so CI behavior is
  "nothing allowlisted" with no yaml/nproj churn.
* Unset-property path (`NemerleAll.nproj:383`), the local/acceptance run:

  ```xml
  <NKnownFailuresArg Condition=" '$(NKnownFailures)' != '' ">-known-failures $(NKnownFailures)</NKnownFailuresArg>
  ```

  Empty/unset ⇒ `$(NKnownFailuresArg)` expands to nothing and the harness
  runs unfiltered (proven by run A, which had `NKnownFailures=[]`).

### D. Bootstrap gate — `./build.ps1 -Targets Stage3,Validate -Rebuild`

`.tmp\phase64\validate.log` (rebuild chain Stage1→Stage2→Stage3 visible at
:24-95): **FC ×5** "no differences encountered" (:97,100,103,106,109), ZERO
`: error` lines, `Build succeeded.` (:111), `VALIDATE_EXIT=0` (:112). The
fixed point is intact — no compiler source was touched (harness and test
fixtures are outside the FC set).

`git diff --stat HEAD` (final state): the 1 harness file
(`snippets/Nemerle.Test/Nemerle.Compiler.Test/NccTestDescription.n`),
`testsuite/ci-known-failures-64.txt`, the 7 test files (+170 pure-addition
lines), the rename `Issue-git-0297.cs → Issue-git-0297-cs.cs` (0 content
lines), plus this untracked progress doc. **`ncc/`, `lib/`, `macros/`:
zero files changed.** No commits made.
