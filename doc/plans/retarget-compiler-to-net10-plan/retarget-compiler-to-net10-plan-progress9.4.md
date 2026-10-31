# 9.4 — reverse-direction port: `f5fe58328` + `testsuite/positive/Bugs/lexer1.n` onto `retarget-compiler`

Executor progress report. Branch: `retarget-compiler` (main checkout,
`C:\RSDN\nemerle`). Orchestrator commits; this task does NOT commit.
Reference: `...-progress9.3.md` (reverse-findings section), `...-report9.1-convergence.md`
(master→retarget fix port table), `doc/MASTER-CHERRY-PICKS.md`.

## Status: COMPLETE

- [✅] Plan (this file)
- [✅] A-1 — `f5fe58328` core fix: `LoadParsersFrom` — moved `GetCustomAttributes`
      / `parserDefs` computation INSIDE the `try` in
      `ncc/backend/hierarchy/LibraryReferenceManager.n` (master file was
      `ncc/external/LibraryReferenceManager.n`; the 9.1 map says
      `ncc/external/` → `ncc/backend/hierarchy/`; content confirmed still at
      the pre-fix state — 9.1 table row `f5fe58328` says NO). The commit's two
      trailing-whitespace-only hunks (`mutable libraryReference;` neighbors)
      were NOT ported — cosmetic, no counterpart drift on this branch.
- [✅] A-2 — `f5fe58328` nproj hunk: `DevBuildQuickNccOnly` gains
      `Linq;Unsafe;_PegAndCSharp` before `Install` (all three targets exist on
      the retarget `NemerleAll.nproj`; ported into the retarget target block
      keeping master's column-aligned text).
- [✅] A-3 — `testsuite/positive/lexer1.n`: verbatim copy of master's blob
      (`git cat-file blob master:testsuite/positive/Bugs/lexer1.n` → blob
      `ae57b36dd`, LF endings, 169 bytes, NO trailing newline — the
      trailing-newline absence IS the test; `git hash-object` on the new file
      reproduces `ae57b36ddac4edb10afbfb6c8ef39c3c8269f94e`). **Path adapted to
      FLAT `testsuite/positive/`** (task pre-authorized: "flat
      testsuite/positive/ — adapt if master nests under Bugs/"): the harness
      collects with `FileSearcher.Search(..., SearchOption.TopDirectoryOnly)`
      and `NemerleAll.nproj` globs `testsuite\positive\*.n` on BOTH branches,
      so master's `Bugs/` subdir (where `c306248d2` "Remove test show stopper"
      parked the test in 2020) is never collected; a Bugs/ placement could not
      reach the required 462. The flat path is also the test's historical home
      (`6d8cb7807`..`c306248d2`). Markers `// W:` + `// OPTIONS:` are supported
      by `NccTestDescription.Parse`.
- [✅] A-4 — restore the pedantic EOF-newline check the test exercises (see
      «Why a compiler-side port is required»): `ManagerClass.CheckLastLineForLF`
      (private) + gate `Options.Warnings.IsEnabled(10002)` in `ManagerClass.Run`
      over `Options.Sources` (deviation from plan: helper is a private
      ManagerClass member, not `Message.*` — single consumer, no public API
      churn).
- [✅] GATE A1 — `./build.ps1 -Targets Stage3,Validate -Rebuild` → FC ×10
      "no differences", zero `: error`, exit 0 (log `.tmp\phase94\validate.log`).
- [✅] GATE A2 — `./build.ps1 -Targets Stage1,CompilerTests` → positives
      **462 passed** (461 + lexer1.n) + 7 not-run, negatives **167/0**;
      `lexer1.n: ...passed` (log `.tmp\phase94\tests.log`).
- [✅] PART B — docs (AGENTS.md cross-branch sync, MASTER-CHERRY-PICKS.md
      STATUS banner).

## Why a compiler-side port is required (A-4)

`lexer1.n` (`// W: no new line`, `// OPTIONS: -pedantic-lexer`) does NOT test
`f5fe58328` — the test is from `76c6da68d`/`7fb4ffcd4` (issue #565) and its
expected warning comes from master's `ncc/parsing/Source.n` `ReadFile`
(`checkLastLineForLF`: when warning 10002 is enabled, a compile-unit file not
ending with `\n` ⇒ `Message.Warning(Location(index,1,1), "no new line at the
end of the file")`; empty file ⇒ fatal `cannot open file …: unexpected end of
file`, LexerBase message). That check was LOST on the retarget line: master's
`Source.n` was "absorbed by Nemerle.Location/" (9.1 Appendix) and the
absorption dropped the LF check — `git grep "no new line"` is empty across
retarget `ncc/` + `Nemerle.Location/` (verified 2026-10-31), while the rest of
the 10002 machinery (`-pedantic-lexer` flag, tab/CR warnings) survived intact.
So the test cannot pass here (on EITHER host) until the check is restored;
9.1's "generic" disposition stands — it is not FX-specific, it is simply
missing on this branch.

Master fired the check where the compiler read a compile-unit file with a
`warnings`-aware source (`main.n:239`, `NemerleCodeCompiler.n:138,166` —
`FileSource(s, cOptions.Warnings)`). On retarget all three of those sites
become `FileUtils.GetSource(...)` (Nemerle.Location has no access to
`WarningOptions`/`Message`), and the Core harness compiles **hosted**
(`HostedNcc` bypasses `main.n` entirely: `options.Sources =
files.Map(FileUtils.GetSource(_))`), so a per-call-site port would miss the
harness path. All three flows converge on `ManagerClass.Run()`'s
`Options.Sources.ConvertAll(Parser.Parse)` — the compile-unit set master
checked — so the hook goes there (single place, identical file scope; macro
assembly sources, which master excluded via `checkLastLineForLF=false` in
`passes.n`, never enter `Options.Sources`).

Fidelity notes:
- warning text verbatim from master ("no new line at the end of the file"),
  reported via `Message.Warning(loc, m)` (code -1, ungated — the enablement
  gate is the `IsEnabled(10002)` check itself, exactly like master);
- location = offset 0 of the file ⇒ line 1, matching master's
  `Location(index,1,1)` (this is what the `// W:` on line 1 matches);
- empty-file case = `Message.FatalError(... "cannot open file `<path>':
  unexpected end of file")` — master's throw-then-catch produced exactly that
  composed text; guarded by `source.File.IsSourceFileAvailable` so fake
  (missing-file) snapshots keep the retarget status-quo path;
- last-CHAR of `source.Text` mirrors master's last-BYTE check ('\r\n'-ending
  files pass, '\r'-ending files warn — same outcomes);
- blast radius: only when 10002 is enabled (default-off list
  `array [10002, …]`; only lexer1.n passes `-pedantic-lexer` in the whole
  testsuite; NemerleAll.nproj/build.ps1 never pass `-pedantic-lexer` or
  `-dowarn:10002`), so bootstrap IL and the other 460+ tests are untouched.

## Gates execution notes

- Sequential builds only; logs tee'd to `.tmp\phase94\`.
- A1 first (Stage3,Validate -Rebuild), then A2 (Stage1,CompilerTests).

## Results

### Ported atom (PART A)

1. **`ncc/backend/hierarchy/LibraryReferenceManager.n`** (`LoadParsersFrom`):
   `def assemblyAttributes` / `def parserDefs` moved INSIDE the `try` — the
   `TypeLoadException` from `GetCustomAttributes` (external parser attribute
   types unloadable) is now caught and reported as
   `cannot load parsers from assembly ...` instead of crashing the compiler.
   Byte-for-byte the `f5fe58328` hunk adapted to the split-layout path.
2. **`NemerleAll.nproj`**: `DevBuildQuickNccOnly` now
   `Stage1; Linq;Unsafe;_PegAndCSharp; ... Install` (master post-image, the
   target block survived the retarget intact).
3. **`testsuite/positive/lexer1.n`** (NEW, flat path, blob-identical
   `ae57b36dd` to master's `Bugs/lexer1.n`):
   `// W: no new line` + `// OPTIONS: -pedantic-lexer` + assembly attribute
   body, no trailing newline.
4. **`ncc/frontend/ManagerClass.n`**: private `CheckLastLineForLF(source)`
   (warning text/location/empty-file fatal all mirror master's
   `FileSource.ReadFile`) + gate `when (Options.Warnings.IsEnabled(10002))
   foreach (source in Options.Sources) CheckLastLineForLF(source);` in `Run`
   before `Options.Sources.ConvertAll(Parser.Parse)`. This is the
   Source→SourceSnapshot-migration repair without which the ported test (and
   master parity of `-pedantic-lexer`) could not work on EITHER host here.

Deviations: (D9.4-1) test at FLAT `testsuite/positive/` instead of master's
`Bugs/` — see A-3 (TopDirectoryOnly collection on both branches; Bugs/ is a
graveyard dir; the task expected the test counted in the 462). (D9.4-2) first
Stage1 smoke failed with `expected ';'` at the hook — braceless-`when` body
needed `;` before the next sequence element; fixed, smoke green. (D9.4-3)
helper as private ManagerClass member instead of Message-module function.

### Behavior probes (Stage1 ncc-core)

| probe | result |
|---|---|
| `lexer1.n -pedantic-lexer` | `warning: no new line at the end of the file` @ `(1,1)`, exit 0 |
| other file w/o trailing `\n` + `-pedantic-lexer` | same warning (generic check) |
| same file WITHOUT `-pedantic-lexer` | no warning (default-off 10002 intact) |
| empty `.n` + `-pedantic-lexer` | `error: cannot open file `<path>': unexpected end of file`, exit 1 — master's composed text |

### GATE A1 — `./build.ps1 -Targets Stage3,Validate -Rebuild` (`.tmp\phase94\validate.log`)

EXIT=0; **FC ×10** (Stage2↔Stage3 × {ncc-core, Nemerle.Compiler, Nemerle,
Nemerle.Macros, Nemerle.MSBuild.Tasks}, both directions) — all
`FC: no differences encountered`; zero `: error` lines in the log.

### GATE A2 — `./build.ps1 -Targets Stage1,CompilerTests` (`.tmp\phase94\tests.log`)

EXIT=0, both sets `All tests passed.`:
```
C:\RSDN\nemerle\testsuite\positive\lexer1.n: ...........................................................passed
```
- Positives: **462 passed** (461 baseline + NEW lexer1.n) + **7 not-run**
  (`Issue-git-0507.n`, `access-checks.n`, `codedom.n`, `form.n`, `resource.n`,
  `security.n`, `serialize.n` — all `not a test on core`), 0 failed.
- Negatives: **167 passed**, 0 failed.
- (Intermediate A2 run with the test parked under `Bugs/` proved the
  TopDirectoryOnly collection — 461, no lexer1 line; re-run after the flat
  placement produced the numbers above.)

### PART B — docs

- `AGENTS.md` «Cross-branch sync»: cherry-pick/remote bullet kept; added the
  layout-alignment fact (`sync-master`, `56788fbeb..dbe955e91`, IL-identity
  proven, master-only fixes ported back in 9.4; identical paths from now on)
  and `doc/MASTER-CHERRY-PICKS.md` marked historical.
- `doc/MASTER-CHERRY-PICKS.md`: STATUS banner prepended (executed 2026-10-31
  via 9.2/9.3; retained as the what/why record; reverse list `f5fe58328` +
  `lexer1.n` noted as done by 9.4 PART A including the Source.n check repair).
