# Progress 10.1–10.3 — canonical names: `ncc.exe`/`ncc.dll` host, `boot/` seed dir

Branch `retarget-compiler`, plan checklist phase 10
(`doc/plans/retarget-compiler-to-net10-checklist.md`, items 10.1–10.3).
User decision: the Core compiler host is canonically `ncc.exe`/`ncc.dll`
(never `ncc-core.*`) and the seed directory is canonically `boot/`
(never `boot-net10/`). **Branch-local rename only** — `master` is untouched;
its `ncc.nproj` is a different, .NET Framework project, and no FX `ncc.nproj`
is re-added here (the net10 host project just gets its name back).

Rules honored: one build at a time (sequential); logs under
`.tmp\phase10\`; `git grep` only; EOL/BOM preserved (edit-in-place only);
no commits (orchestrator commits).

## Why the transitional alias is needed

The checked-in seed (`boot-net10/`) carries the OLD-named toolchain:
`ncc-core.exe` is a .NET apphost with the string `ncc-core.dll` embedded in
it — the apphost resolves that embedded managed-dll name *next to itself*.
An exe-only alias (`ncc.exe` = byte copy of `ncc-core.exe`) therefore still
works while `ncc-core.dll` sits beside it (renaming the dll would break the
embedded name; renaming the exe cannot break anything). So:

1. rename the build graph to `ncc.*`, bootstrap Stage1 once through the
   exe alias, let Stage1–3 produce correctly named `ncc.*` outputs;
2. reseed `boot/` from Stage2 (correctly named), delete the alias hack and
   the old-named seed files, rerun the full gates clean.

## Rename inventory (before → after, per file)

### Step 1 — source rename `ncc-core.*` → `ncc.*`

| File | Change |
|---|---|
| `ncc-core.nproj` | `git mv` → `ncc.nproj`; `<AssemblyName>` `ncc-core` → `ncc`; comments naming the Core host `ncc-core` → `ncc` (FX-era "net461 ncc.exe" historical phrasing kept where it names the deleted master-side binary) |
| `NemerleAll.nproj` | L25 seed comment `(ncc-core)` → `(ncc)`; L71–74 host-project comment rewritten + `<NCompilerProject Include="$(NRoot)\ncc.nproj" />`; L250–251 NPrepareBoot json-comment `ncc-core.*` → `ncc.*`; **Validate**: drop the now-vacuous `ncc*.exe` Asm includes (no managed exe matches; the apphost `ncc.exe` is native and must NOT reach ildasm — old `Exclude="ncc-*.exe"` guarded that, new name would have escaped it), `Il*` include `ncc-core.dll` → `ncc.dll`; L374 coverage comment `ncc-core` → `ncc`; `_NugetPack` freshness scan Stage{1..4}\ncc-core.exe → ncc.exe (4×), L625/632/665 comments/error text |
| `tools/msbuild-task/MSBuildTask.cs` | REMOVE the `ncc.exe`→`ncc-core.exe` autoremap (`CoreToolName` + its two use sites) — the name is canonical again, consumers need nothing; L170 comment |
| `ncc/frontend/CompilationOptions.n` | L34 comment: Core host `(ncc-core.exe)` → `(ncc.exe)`; L37 FX-host sentence disambiguated (comment-only, IL-identical) |
| `.github/workflows/build.yml` | smoke steps: `Stage3\ncc-core.exe` → `Stage3\ncc.exe` (paths ×2, name/comment/error-text mentions) |
| `Nemerle.Compiler.nuspec` | description + tool-closure comment; `tools\net10.0\` file list: `ncc-core.exe/.dll/.runtimeconfig.json/.deps.json` → `ncc.*`; L20 historical note → "FX ncc.exe left the build graph" |
| `Directory.Build.props` | L19 comment `net10.0 ncc-core` → `net10.0 ncc` (zero-delta file: comment-only) |
| `Nemerle.Location/Nemerle.Location.nproj` | `<StartProgram>` `Stage1\ncc-core.exe` → `Stage1\ncc.exe` |
| `snippets/Nemerle.Test/Nemerle.Compiler.Test/Nemerle.Compiler.Test.nproj` | L50 comment "mirrors ncc-core.nproj" → "mirrors ncc.nproj" |
| `NemerleAll.nproj` `NPrepareBoot` | **TEMPORARY** alias: after the seed copy, `ncc-core.exe` → `ncc.exe` (exe only) when the seed ships no `ncc.exe`; removed in Step 4 |

Checked and NOT changed: `build.ps1`/`build.sh` (no `ncc`/`boot` refs at all —
the orchestrator passes `Nemerle=`/`NRootBoot` itself);
`tools/msbuild-task/Nemerle.MSBuild.targets`, `Nemerle.Sdk.props/.targets`
(no tool-name references; `MSBuildTask.cs` resolves `ncc.exe` by lookup);
`tools/msbuild-task/ncc-mono{,.bat}` (mono-era wrappers naming `ncc.exe` —
already the canonical name, dead code path on this branch);
`Nemerle.nuspec` (libs only, no tool files);
harness (`Nemerle.Compiler.Test`) — compiles in-process via `HostedNcc`,
no external `ncc-core` process launch; `testsuite/` — no matches;
`ncc/shared/AssemblyInfo.n` — no host-name string; `doc/plans/**` —
historical, left alone.

### Step 5 — `boot-net10/` → `boot/`

| File | Change |
|---|---|
| `boot-net10/` | `git mv` → `boot/` (after reseed; old-named `ncc-core.*` seed files already deleted at reseed) |
| `NemerleAll.nproj` | `<NRootBoot>$(NRoot)\boot-net10` → `boot`; L25–28/L242–252 seed comments; standalone-guard comments ×4 |
| `Nemerle.nproj`, `Nemerle.Compiler.nproj`, `Nemerle.Macros.nproj`, `ncc.nproj` | standalone default `$(MSBuildProjectDirectory)\boot-net10` → `boot` (8.3a) + guard comments |
| `Nemerle.Compiler.nuspec` | comment "same set as the boot-net10 seed" → `boot/` seed |
| `AGENTS.md` | seed-list paragraph + all build-graph mentions → `ncc.*`/`boot/`, one-line historical note that the seed was staged as `boot-net10`/`ncc-core.*` during the retarget (phases 4–9) |
| `README`, `README.md` | `boot-net10/` → `boot/`, `ncc-core.exe` → `ncc.exe`, `ncc-core.nproj` → `ncc.nproj` |
| `doc/NET10-BUILD-BASELINE.md` | current-state paragraphs → new names; historical paragraphs (L91-era) stay |

### Step 4 — reseed + Step 5 gates (AGENTS.md «Re-seeding» recipe)

1. `Stage3,Validate -Rebuild` green on the alias + `Stage1,CompilerTests`
   green (preconditions to touch the seed at all).
2. Copy the 10 seed files from `bin\Debug\net-4.0\Stage2\` over `boot-net10/`
   (ncc.exe, ncc.dll, ncc.runtimeconfig.json, ncc.deps.json, dnlib.dll,
   System.CodeDom.dll, Nemerle.dll, Nemerle.Compiler.dll, Nemerle.Macros.dll,
   Nemerle.MSBuild.Tasks.dll); `git rm` the old `ncc-core.*` seed files so a
   stale old-named binary cannot mask anything.
3. Regenerate `SEED.manifest` (sha256 + size per file, `Get-FileHash`).
4. Remove the temporary alias from `NPrepareBoot`.
5. Clean `bin\Debug\net-4.0` + `obj\Debug\net-4.0` (old trees may predate the
   rename — AGENTS gotcha) and rerun the FULL cycle: `Stage3,Validate
   -Rebuild` (FC ×10) + `Stage1,CompilerTests` (461+7nr / 167).
6. `git mv boot-net10 boot` + reference/docs edits, then the final gate from
   the moved seed: `Stage3,Validate,_NugetPack -Rebuild` + zip listing +
   extracted `ncc.exe -version` + zero-r compile smoke.

## Bitness note (10.3)

On .NET (Core) a single framework-dependent apphost is sufficient: process
bitness follows the installed runtime (hostfxr picks the x64/x86/ARM64
runtime; the apphost itself is just a launcher stub), and dnlib is fully
managed — there are no FX-era hosting APIs (no `mscoree.dll` shimming, no
`CreateInstanceFrom`-in-proc hosting) that forced the old `ncc32.exe`/
`ncc64.exe` bitness launchers. The deleted `ncc32`/`ncc64` projects were
.NET Framework-era bitness selectors; keeping dual apphosts here would only
duplicate identical managed IL. The runtimeconfig's Minor roll-forward pin
(10.0.0) remains the only startup constraint (AGENTS gotcha, unchanged).

## Gate log

(filled in as the cycle runs — see `.tmp\phase10\*.log`)

### Step 1 — source rename (done)

`git mv ncc-core.nproj ncc.nproj` + all build-graph edits per the inventory
above (alias hack added to `NPrepareBoot`). All edited XML/YAML re-parsed
clean. `git grep ncc-core` outside docs now only matches: the temporary
`NPrepareBoot` alias + its comment, the historical NemerleAll comment, and
the old seed files (all three groups scheduled away by Steps 4–5).

### Step 2 — transitional bootstrap (GREEN)

`./build.ps1 -Targets Stage3,Validate -Rebuild` on a cleaned
`bin\Debug\net-4.0` + `obj\Debug\net-4.0`, seed aliased (`ncc.exe` = copy of
`ncc-core.exe`, dll NOT renamed so the apphost's embedded name still
resolves). Log: `.tmp\phase10\step2-stage3-validate.log`.

- Stage1 compiled by the aliased seed; every stage emits the new names:
  `ncc -> ...\Stage{1,2,3}\ncc.dll` (plus `ncc.exe` apphost 161,280 B,
  `ncc.runtimeconfig.json`, `ncc.deps.json` in Stage3).
- Validate: **10 × `FC: no differences encountered`** (5 assemblies × 2
  directions, `ncc.dll.il` included), no MSB errors,
  `Build succeeded.` — the new-named stages self-build at a fixed point.

### Step 3 — compiler suite on the new-named Stage1 (GREEN)

`./build.ps1 -Targets Stage1,CompilerTests`.
Log: `.tmp\phase10\step3-compiler-tests.log`.

- Positives: **462 passed + 7 “not a test on core” (skipped), 0 failed**
  (“All tests passed. Time taken: 00:03:40”; the 461→462 delta is
  `testsuite/positive/lexer1.n`, added by the 9.4 reverse-port — its gate
  commit message already records “positives 462 passed (461+new) + 7
  not-run”).
- Negatives: **167 passed, 0 failed** (“All tests passed. Time taken:
  00:01:04”).
- Harness ran on the new Stage1 (Runtime version 10.0.13, 64 bit).

### Step 4 — reseed + clean post-reseed cycle (done)

Reseed done per the AGENTS recipe (preconditions: Step 2 FC ×10 + Step 3
suite green): the 10 seed files copied from `bin\Debug\net-4.0\Stage2\`
(newly produced, correctly named `ncc.*`) over `boot-net10/`; the four old
`ncc-core.*` seed files `git rm`-ed (a stale old-named binary can no longer
mask anything); `SEED.manifest` regenerated (sha256 + size, CRLF, same
format; `dnlib.dll`/`System.CodeDom.dll` hashes unchanged — a good sign for
the regeneration procedure); the temporary `NPrepareBoot` exe alias hack
REMOVED. `bin\Debug\net-4.0` + `obj\Debug\net-4.0` cleaned, full cycle
restarted from the new-named seed.

- Post-reseed `Stage3,Validate -Rebuild` (clean tree, seeded by the new-named
  `boot-net10/` binaries, NO alias): every stage emits `ncc.*`;
  **10 × “FC: no differences encountered”**, zero errors, exit 0
  (`.tmp\phase10\step4-postreseed-stage3-validate.log`).
- Post-reseed `Stage1,CompilerTests`: positives **462 passed + 7 not-run,
  0 failed**; negatives **167 passed, 0 failed**; both sets “All tests
  passed.”, `Build succeeded.` (`.tmp\phase10\step4-postreseed-compiler-tests.log`).

### Step 5 — `boot-net10/` → `boot/` (done)

`git mv boot-net10 boot` hit a pre-existing tracked **legacy 2003-era
`boot/`** directory (`lock.txt` bootstrap-lock log, autotools `Makefile`
with no remaining configure driver, FX `ncc.exe`/`ncc32.exe`/`ncc64.exe`,
`true.exe` wine-probe stub — the folder AGENTS.md documented as “a legacy
folder from an even older version”). The rename supersedes it name-and-all
(AGENTS.md seed-list note updated accordingly), so its six tracked relics
were `git rm`-ed (content preserved in git history) and the seed now owns
`boot/` alone: the 10 reseeded files + regenerated `SEED.manifest`.
`NRootBoot`, the four 8.3a standalone defaults and the five guard comments
now say `boot/` (guard comments keep a no-literal lineage note);
`Nemerle.Compiler.nuspec` comment and the two `NemerleAll.nproj` glob
comments likewise. `git grep "ncc-core\|boot-net10"` outside `doc/` is
**empty**.

Final gate from the moved seed — `Stage3,Validate,_NugetPack -Rebuild` on a
cleaned tree, seeded by `boot/`: every stage emits `ncc.*`;
**10 × “FC: no differences encountered”**; `_NugetPack: packaging stage
'...\Stage3'`; `Successfully created package '...\Nuget\Nemerle.10.0.79.nupkg'`
and `...\Nemerle.Compiler.10.0.79.nupkg`; `Build succeeded.`, exit 0
(`.tmp\phase10\step5-final-stage3-validate-nugetpack.log`).

Docs pass: `AGENTS.md` (seed list → `boot/` + `ncc.*` with a one-line
historical note; reseed recipe; working-compiler path; gotchas),
`README`/`README.md`, and a new “Re-verified after Phase 10” paragraph in
`doc/NET10-BUILD-BASELINE.md` (historical baseline paragraphs untouched).

### Package + smoke (from the FINAL gate’s nupkg)

- `Nemerle.Compiler.10.0.79.nupkg` listing: `tools/net10.0/ncc.exe`,
  `ncc.dll`, `ncc.runtimeconfig.json`, `ncc.deps.json`, `dnlib.dll`,
  `System.CodeDom.dll`, `Nemerle.dll`, `Nemerle.Compiler.dll`,
  `Nemerle.Macros.dll` + `lib/netstandard2.0/{Nemerle.Compiler,Nemerle.Macros}.dll`
  — no `ncc-core.*` entries.
- Extracted toolset smoke: `ncc.exe -version` →
  “Nemerle Compiler (ncc) version 10.0.79.0”, exit **0**; zero-`-r` compile
  of a trivial `.n` → exit **0**, produced `out.exe`.

### Final state

- `git add -A` staged (no commits, per orchestrator rule): rename
  `ncc-core.nproj → ncc.nproj`; seed moves `boot-net10/{dnlib,
  System.CodeDom,ncc-core.deps.json,ncc-core.dll,
  ncc-core.runtimeconfig.json} → boot/...` (git-detected renames; the
  reseeded binaries show as content updates at their new paths); six legacy
  `boot/` relics deleted; source/build-graph/docs edits; progress doc added.
  `bin/`, `obj/` not in `git status` (ignored).
- `boot/SEED.manifest` re-verified byte-exact against the seed files
  (`ncc.*` names).
- `git grep "ncc-core\|boot-net10" -- ':!doc/plans' ':!*.md'` → **empty**.
- Cosmetic residue: the now-EMPTY `boot-net10/` working-tree directory is
  held by an unidentified transient OS handle (indexer/AV) and refuses
  removal; it contains no files, is untracked (git ignores empty dirs), and
  disappears on next checkout/reboot.






