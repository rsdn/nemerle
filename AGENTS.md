# AGENTS.md — Nemerle

Nemerle: a statically-typed language for .NET with a C#-like syntax and a
meta-programming system. The compiler (`ncc`) and the standard library are
themselves written in Nemerle (`.n`), so the project is self-hosting: it must
be compiled by a previously built compiler.

## Two branches — do not mix them up

- **`master`** — the shipping version targeting **.NET Framework**. It is the
  only branch with the **Visual Studio 2017 IDE integration**, which is why the
  Framework target is kept alive. `VsIntegration/` lives here.
- **`retarget-compiler`** (current default checkout) — a port to **.NET 10**,
  completed through Phase 6 of the net10 plan (`doc/plans/`). The **toolchain**
  (compiler host `ncc-core.exe`, test harness, packaging) runs on .NET 10; the
  **libraries stay `netstandard2.0`** (plan decisions D2/D6). `VsIntegration/`
  is removed from the root; IDE code moved to `snippets/VS2010/` (Windows-only,
  guarded off, unmigrated — the shipping IDE integration is `master`-only).
  The port's core is the **dnlib backend** (`*-dnlib.*` projects +
  `ExternalDependences/dnlib.dll`) replacing `System.Reflection.Emit`, plus the
  new `Nemerle.Location/` source-location abstraction. Bootstrap seed:
  `boot-net10/` (see Bootstrapping).

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
  runtime (.NET Framework on master vs .NET 10 on retarget). Same observable
  semantics, different IL.
- Mechanism in practice: cherry-pick/merge shared frontend/typing fixes across
  branches (via the `github`/`kl` remotes); leave backend-specific work local.

Check which branch you are on before editing: a fix in `ncc/`/`lib/`/`macros/`
likely needs to land on both; IDE/integration work is branch-specific.

Remotes: `github` = upstream `rsdn/nemerle`; `kl` = internal TFS mirror.

## Build

The build is **driven by `build.ps1`** (bash twin `build.sh`), the canonical
entry since the net10 retarget (Phase 5.2). It drives the classic-MSBuild
orchestrator `NemerleAll.nproj` with a modern toolchain and needs **no
Visual Studio 2017 and no registry probing**: MSBuild (>= 18, required by
`global.json` sdk 10.0.402) is located via `-MsBuildExe`, the `NEMERLE_MSBUILD`
environment variable, or vswhere (any recent Visual Studio with the MSBuild
component). A full build is still Windows-only (`build.sh` refuses off
Windows). `-UseDotNetMsBuild` exists, but classic MSBuild Full stays the
default: `dotnet msbuild` evaluates `NemerleAll.nproj` yet cannot run any
stage (MSB4803 on `GetFrameworkSdkPath` in `InitTools`).

Canonical commands (from the repo root; verified in
`doc/NET10-BUILD-BASELINE.md`, CI runs exactly these):

| Command | What it does |
|---|---|
| `./build.ps1` | Stage1 + Peg/CSharp/CE/Async + Install (fastest usable compiler) |
| `./build.ps1 -Targets Stage3,Validate -Rebuild` | standard green gate: Stage1→2→3 chain + IL fixed point (`FC` ×5 assemblies) |
| `./build.ps1 -Targets Stage1,CompilerTests` | compiler regression suite (one msbuild session is required, hence the combined list) |
| `./build.ps1 -Targets _NugetPack` | NuGet packaging via `dotnet pack` → `bin\<Config>\net-4.0\Nuget` |

`Build-core-base.cmd` and `Build-core{,-2-stage,-with-tests}.cmd` remain as
thin .cmd shims over `build.ps1` (contract
`Build-core-base.cmd <Config> <Target> "<Targets>"` — quote target lists
there, cmd.exe splits unquoted commas). The VS-IDE-plugin build entry and the
whole FX GAC/ngen/registry-registration script family were deleted in
Phase 7.1; VS IDE integration ships on `master` only.

Direct MSBuild equivalent: resolve MSBuild via
`vswhere -latest -products * -requires Microsoft.Component.MSBuild -property installationPath`,
then `MSBuild.exe NemerleAll.nproj /t:Stage1 /p:NTargetName=Build /p:Configuration=Debug /p:NInstall=<dir>`.

### Bootstrapping — the project lives on it (most important concept)

Nemerle's compiler and stdlib are written in Nemerle, so the compiler compiles
**itself**. You cannot build from a bare clone without a pre-built compiler: the
build is a **bootstrap chain** where each stage is produced by the previous one.
Internalize this before editing `ncc/`, `lib/`, or `macros/`:

- A checked-in **binary bootstrap compiler** seeds the chain (`NRootBoot`,
  NemerleAll.nproj); it is copied to `bin\<Config>\net-4.0\boot\` and used as
  `Nemerle=` for Stage1. **Never delete/regenerate these casually** — without a
  seed nothing builds. Seeds per line/generation:
  - `boot-4.0/` — the .NET Framework line: emits via `System.Reflection.Emit`,
    runs on .NET Framework 4.0. Seed for **`master`**.
  - `boot-net10/` — the retarget line's **current seed** (`NRootBoot` points
    here): **framework-dependent .NET 10** toolchain (`ncc-core.exe/.dll` +
    runtimeconfig/deps + `dnlib.dll` + `System.CodeDom.dll`), emits via
    **dnlib** (not SRE).
  - `boot-dnlib/` — the previous dnlib-era seed (netstandard deploy); kept only
    as an emergency fallback until the fixed point is reproduced from a clean
    clone (plan 7.4).
  A seed must be a *prior* binary that runs on the target host and emits the way
  its line expects — switching backend (SRE→dnlib) and/or runtime (FX→Core)
  invalidates the old seed, so a new known-good one is built once with the new
  backend and checked in. That is why `NRootBoot` differs per branch. (`boot/`
  is a legacy folder from an even older version.)
- Chain: `boot-net10 → Stage1 → Stage2 → Stage3 → Stage4`, each stage compiled
  by the prior stage's `ncc-core` (see the `Nemerle=$(NPrevBin)` property per
  stage). Stage outputs: `bin\<Config>\net-4.0\Stage{1..4}`.
- **Why two stages (the core safety idea).** The current version is always built
  by a *previous* version, and that chain can silently break. So:
  - **Stage1** compiles the working tree with the *checked-in boot*
    `ncc-core.exe` — a known-good prior version that acts as the trusted
    anchor (worst case you rebuild from boot, the chain is never permanently
    poisoned).
  - **Stage2** recompiles the *same* sources with the Stage1-produced
    `ncc-core.exe`, i.e. the new compiler building itself.
  A single Stage1 build only proves the *old* compiler still digests the source;
  it says nothing about whether the *new* compiler is correct. Stage2 is what
  actually exercises the new compiler against its own full source. If Stage2
  succeeds and its IL matches Stage1 (`Validate` ildasm-dumps and `fc`-diffs the
  last two stages across 5 assemblies — the old pefverify step was retired in
  Phase 5.1), a **fixed point** is reached — the new compiler faithfully
  reproduces itself and is safe to promote as the next boot compiler.
- **Editing the compiler can break self-compilation**, not just the program it
  emits. A change that survives Stage1 may fail at Stage2 — e.g. a new language
  feature the source needs that the Stage1 output doesn't support yet. This is
  expected mid-bootstrap; resolve by keeping compiler + what it compiles in step.
- Day-to-day: build **Stage1** (`./build.ps1`). Before trusting/rolling a new
  boot, build **Stage2** (`./build.ps1 -Targets Stage2`) to confirm the
  bootstrap is intact; Stage3/Stage4 + `Validate`
  (`./build.ps1 -Targets Stage3,Validate -Rebuild`) for a full fixed-point proof.
- `Install` copies the last built stage to `bin\<Config>\net-4.0` (dev) or
  `%ProgramFiles%\Nemerle` (default).

After `./build.ps1`, the working compiler is
`bin\Debug\net-4.0\ncc-core.exe` (a .NET 10 apphost; the old Framework
`ncc.exe` host is gone from the build graph since Phase 3.4b).

Language sources by compiler pass: see `ncc/README` (parsing → hierarchy →
typing → generation). `ncc/frontend`, `ncc/backend`, `ncc/shared` are the
split; `lib/` is the stdlib, `macros/` the std macros.

## Tests

Compiler regression suite is data-driven, not NUnit. Test cases are `.n`/`.nnn`/`.cs`
files under `testsuite/positive/` and `testsuite/negative/`, executed by the
harness `Nemerle.Compiler.Test` (net10 since Phase 6.1; sources under
`snippets/Nemerle.Test/Nemerle.Compiler.Test/`).

- Build+run the whole suite: `./build.ps1 -Targets Stage1,CompilerTests`
  (runs both positive and negative sets in one msbuild session).
- **Single test**: run the harness from its copied working directory against one
  file, mirroring the `CompilerTests` target (NemerleAll.nproj ~line 416):
  ```
  cd bin\Debug\net-4.0\Tests\negative
  Nemerle.Compiler.Test.exe -r dotnet -rp exec -output:. "-p:-nowarn:10003 -def:NET_4_0;DEBUG" "<abs\path\to\Test.n>"
  ```
  On .NET (Core) hosts `-r dotnet -rp exec` makes the harness launch test exes
  via `dotnet exec` + a generated `<test>.runtimeconfig.json` (6.2c); compiled
  tests get their default framework references from the compiler itself (6.2b),
  so no `-r` list is needed.
- Current Core norm: positives **461 passed + 7 not-run**, negatives **167
  passed**, 0 failed — the 7 not-run are FX-only content skipped via the
  `// NO-CORE` marker (6.2d). FX behavior is unchanged (master-safe).
- Optional `-known-failures <file>` allowlist (default-off; CI passes the
  currently empty `testsuite/ci-known-failures-64.txt`): listed ids are
  reported but do not fail the run, everything else still does.
- Expected diagnostics/output are encoded **in the test file as comments**:
  `// E: <regex>` (expected error), `// W: <regex>` (warning), `// OK`,
  `// NO-TEST`, `// NO-CORE` (not a test on Core hosts), and
  `BEGIN-OUTPUT`/`END-OUTPUT` blocks for runtime output, with paired
  `BEGIN-OUTPUT-CORE` blocks for Core-specific expected output (6.4).
  A file with no `BEGIN-OUTPUT` is only compiled to a DLL. See `testsuite/README`.

## Gotchas

- `.gitmodules` (vssdk/wix under `ExternalDependences/`) are only needed for the
  installer/IDE targets; the core compiler build does not require them.
- Projects are `.nproj` (Nemerle) in old-style MSBuild v4 format, not SDK-style
  (exception: `Nemerle.MSBuild.Tasks.csproj`). `global.json` (sdk 10.0.402) and
  root `Directory.Build.props` / `nuget.config` (plan 5.3) **do** exist now —
  they are evaluated by every project, keep them zero-delta. `dotnet build` /
  `dotnet msbuild` still cannot run `NemerleAll.nproj` (MSB4803 in `InitTools`);
  it needs classic `msbuild` >= 18, which `build.ps1` resolves for you.
- On this branch only the **toolchain** is net10 (`ncc-core`, harness);
  the **libraries stay `netstandard2.0`** (D2/D6) — do not "modernize" their TFMs.
- Compiler is signed: build copies `misc/keys/*.snk` to `bin\<Config>\net-4.0\keys`
  (`NPrepareKeys`). Keep key handling intact when touching project files.
- `Nemerle.MSBuild.targets` (built from `tools/msbuild-task/`) is what lets
  downstream `.nproj` files import the compiler; changes there affect every build.
