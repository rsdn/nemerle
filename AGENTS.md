# AGENTS.md — Nemerle

Nemerle: a statically-typed language for .NET with a C#-like syntax and a
meta-programming system. The compiler (`ncc`) and the standard library are
themselves written in Nemerle (`.n`), so the project is self-hosting: it must
be compiled by a previously built compiler.

## Two branches — do not mix them up

- **`master`** — the shipping version targeting **.NET Framework**. It is the
  only branch with the **Visual Studio 2017 IDE integration**, which is why the
  Framework target is kept alive. `VsIntegration/` lives here.
- **`retarget-compiler`** (current default checkout) — a port aiming at
  **.NET Core**. `VsIntegration/` is removed from the root; IDE code moved to
  `snippets/VS2010/`. The port's core is the **dnlib backend** (`*-dnlib.*`
  projects + `ExternalDependences/dnlib.dll`) replacing `System.Reflection.Emit`,
  plus the new `Nemerle.Location/` source-location abstraction.

### Cross-branch sync

The two lines must keep **compiling the same Nemerle source** (including their
own shared compiler source), so language-affecting changes must be ported to
both. This matters doubly because each branch bootstraps from its own seed —
drift breaks not just user programs but the shared bootstrap source too.

- **Keep in sync:** the language + semantics surface — `ncc/` frontend
  (lexer/parser), `hierarchy/`, `typing/`, macro mechanics, and the public APIs
  of `lib/` and `macros/`. Any language fix/feature belongs on both branches.
- **Intentionally divergent (do NOT unify):** the emit backend
  (`System.Reflection.Emit` on master vs `dnlib` on retarget) and the host
  runtime (.NET Framework vs .NET Core). Same observable semantics, different IL.
- Mechanism in practice: cherry-pick/merge shared frontend/typing fixes across
  branches (via the `github`/`kl` remotes); leave backend-specific work local.

Check which branch you are on before editing: a fix in `ncc/`/`lib/`/`macros/`
likely needs to land on both; IDE/integration work is branch-specific.

Remotes: `github` = upstream `rsdn/nemerle`; `kl` = internal TFS mirror.

## Build

The build is **driven by MSBuild + Visual Studio 2017**, even on the
retarget-compiler branch ("dotnet core" describes the produced compiler's goal,
not the build host). `Build-core-base.cmd` reads VS 2017 from the registry
(`HKLM\...\SxS\VS7` = `15.0`), calls `VsDevCmd.bat`, then runs `msbuild` on
`NemerleAll.nproj`. **Without VS 2017 installed the build scripts silently do
nothing** (they print "not found in Windows Registry").

Root wrapper scripts (all just call `Build-core-base.cmd <Config> <Target> "<Features>"`):

| Script | What it does |
|---|---|
| `Build-core.cmd`        | Stage1 + Peg/CSharp/CE/Async + Install (fastest usable compiler) |
| `Build-core-2-stage.cmd`| Stage2 (self-compile with Stage1 output) |
| `Build-core-with-tests.cmd` | Stage1 + CompilerTests + Install |
| `Build-core-with-ide-plug-in.cmd` | `DevBuildQuick` (adds NuGet pack + VS integration) |

Direct MSBuild equivalent:
`msbuild NemerleAll.nproj /t:Stage1 /p:NTargetName=Build /p:Configuration=Debug /p:NInstall=<dir>`

### Bootstrapping — the project lives on it (most important concept)

Nemerle's compiler and stdlib are written in Nemerle, so the compiler compiles
**itself**. You cannot build from a bare clone without a pre-built compiler: the
build is a **bootstrap chain** where each stage is produced by the previous one.
Internalize this before editing `ncc/`, `lib/`, or `macros/`:

- A checked-in **binary bootstrap compiler** seeds the chain (`NRootBoot`,
  NemerleAll.nproj); it is copied to `bin\<Config>\net-4.0\boot\` and used as
  `Nemerle=` for Stage1. **Never delete/regenerate these casually** — without a
  seed nothing builds. There are **two seeds, one per emit-backend generation**:
  - `boot-4.0/` — the .NET Framework line: emits via `System.Reflection.Emit`,
    runs on .NET Framework 4.0. Seed for **`master`**.
  - `boot-dnlib/` — the retarget line: emits via **dnlib** (not SRE), deployed
    as netstandard/.NET Core (`dnlib.dll`, `netstandard.dll`, `System.*` facades
    live in that folder). Seed for **`retarget-compiler`** (`NRootBoot` points here).
  A seed must be a *prior* binary that runs on the target host and emits the way
  its line expects — switching backend (SRE→dnlib) and/or runtime (FX→Core)
  invalidates the old seed, so a new known-good one is built once with the new
  backend and checked in. That is why `NRootBoot` differs per branch. (`boot/`
  is a legacy folder from an even older version.)
- Chain: `boot-dnlib → Stage1 → Stage2 → Stage3 → Stage4`, each stage compiled
  by the prior stage's `ncc` (see the `Nemerle=$(NPrevBin)` property per stage).
  Stage outputs: `bin\<Config>\net-4.0\Stage{1..4}`.
- **Why two stages (the core safety idea).** The current version is always built
  by a *previous* version, and that chain can silently break. So:
  - **Stage1** compiles the working tree with the *checked-in boot* `ncc.exe` —
    a known-good prior version that acts as the trusted anchor (worst case you
    rebuild from boot, the chain is never permanently poisoned).
  - **Stage2** recompiles the *same* sources with the Stage1-produced `ncc.exe`,
    i.e. the new compiler building itself.
  A single Stage1 build only proves the *old* compiler still digests the source;
  it says nothing about whether the *new* compiler is correct. Stage2 is what
  actually exercises the new compiler against its own full source. If Stage2
  succeeds and its IL matches Stage1 (`Validate` IL-diffs the last two stages),
  a **fixed point** is reached — the new compiler faithfully reproduces itself and
  is safe to promote as the next boot compiler.
- **Editing the compiler can break self-compilation**, not just the program it
  emits. A change that survives Stage1 may fail at Stage2 — e.g. a new language
  feature the source needs that the Stage1 output doesn't support yet. This is
  expected mid-bootstrap; resolve by keeping compiler + what it compiles in step.
- Day-to-day: build **Stage1** (`Build-core.cmd`). Before trusting/rolling a new
  boot, build **Stage2** (`Build-core-2-stage.cmd`) to confirm the bootstrap is
  intact; Stage3/Stage4 + `Validate` for a full fixed-point proof.
- `Install` copies the last built stage to `bin\<Config>\net-4.0` (dev) or
  `%ProgramFiles%\Nemerle` (default).

After `Build-core.cmd`, the working compiler is `bin\Debug\net-4.0\ncc.exe`.

Language sources by compiler pass: see `ncc/README` (parsing → hierarchy →
typing → generation). `ncc/frontend`, `ncc/backend`, `ncc/shared` are the
split; `lib/` is the stdlib, `macros/` the std macros.

## Tests

Compiler regression suite is data-driven, not NUnit. Test cases are `.n`/`.nnn`/`.cs`
files under `testsuite/positive/` and `testsuite/negative/`, executed by the
harness built from `testsuite/test.n` (`Nemerle.Compiler.Test.exe`).

- Build+run the whole suite: `Build-core-with-tests.cmd` (runs both positive and
  negative sets).
- **Single test**: run the harness from its copied working directory against one
  file, mirroring the `CompilerTests` target (NemerleAll.nproj ~line 376):
  ```
  cd bin\Debug\net-4.0\Tests\negative
  Nemerle.Compiler.Test.exe -output:. "-p:-nowarn:10003 -def:NET_4_0;DEBUG" "<abs\path\to\Test.n>"
  ```
- Expected diagnostics/output are encoded **in the test file as comments**:
  `// E: <regex>` (expected error), `// W: <regex>` (warning), `// OK`,
  `// NO-TEST`, and `BEGIN-OUTPUT`/`END-OUTPUT` blocks for runtime output.
  A file with no `BEGIN-OUTPUT` is only compiled to a DLL. See `testsuite/README`.

## Gotchas

- `.gitmodules` (vssdk/wix under `ExternalDependences/`) are only needed for the
  installer/IDE targets; the core compiler build does not require them.
- Projects are `.nproj` (Nemerle) in old-style MSBuild v4 format, not SDK-style.
  There is no `global.json` / `Directory.Build.props`. Do not assume
  `dotnet build` works on `NemerleAll.nproj` — it needs classic `msbuild`.
- Compiler is signed: build copies `misc/keys/*.snk` to `bin\<Config>\net-4.0\keys`
  (`NPrepareKeys`). Keep key handling intact when touching project files.
- `Nemerle.MSBuild.targets` (built from `tools/msbuild-task/`) is what lets
  downstream `.nproj` files import the compiler; changes there affect every build.
