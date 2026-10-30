# Progress — sub-point 2.3c (shared-`obj` restore collision fix)

Branch `retarget-compiler` @ 1cb57f94b (tree carries the uncommitted 2.3a/2.3b/2.4a
changes: `Nemerle.nproj`, `Nemerle.Compiler.nproj`, `Nemerle.Macros.nproj`,
`tools/msbuild-task/Nemerle.MSBuild.targets`, `tools/msbuild-task/Nemerle.Sdk.targets`,
plus pre-existing `doc/plans/retarget-compiler-to-net10-checklist.md`). No commits.
Context: handoff notes in
`retarget-compiler-to-net10-plan-progress2.3a-2.3b-2.4a.md`.

## Problem (established by prior subagent, verified)

`tools/msbuild-task/Nemerle.Sdk.props:9` redirects `MSBuildProjectExtensionsPath`
to `$(IntermediateOutputPath)$(AssemblyName)\` only when `IntermediateOutputPath`
is defined (the stage chain passes it). Under `dotnet build` the property is
empty at import time, so every root project falls back to the SDK default
`<projdir>\obj\` and shares one `obj\project.assets.json`. `dotnet build
Nemerle.Macros.nproj` (no package refs) implicitly restores and rewrites the
assets file that `Nemerle.Compiler.nproj` needs (dnlib PackageReference),
breaking the P2R Compiler rebuild with 1879 "unbound dnlib member" errors.
The same root `obj\` also carries `Nemerle.MSBuild.Tasks.csproj`'s restore
output, so CLI builds of the .nprojs additionally poison the classic NTasks
chain (CS0246 on the next Stage1).

## Change (single file)

`tools/msbuild-task/Nemerle.Sdk.props` — per-project `MSBuildProjectExtensionsPath`
in BOTH cases:

- line 9 (existing, verbatim): `Condition=" '$(IntermediateOutputPath)' != '' "`
  → `$(IntermediateOutputPath)$(AssemblyName)\` — stage chain behavior unchanged;
- NEW sibling assignment: `Condition=" '$(IntermediateOutputPath)' == '' "`
  → `$(MSBuildProjectDirectory)\obj\$(AssemblyName)\` — CLI builds get
  `obj\Nemerle\`, `obj\Nemerle.Compiler\`, `obj\Nemerle.Macros\` (and
  `obj\ncc-core\` etc.) instead of the shared root `obj\`.

Note on the exact expression: the task brief spelled the value
`$(MSBuildProjectDirectory)obj\$(AssemblyName)\`; MSBuild property expansion is
literal concatenation and `$(MSBuildProjectDirectory)` carries no trailing
separator, so that literal would yield `C:\RSDN\nemerleobj\Nemerle\`. Implemented
with the intended separator: `$(MSBuildProjectDirectory)\obj\$(AssemblyName)\`.

Both assignments sit in the existing `<PropertyGroup>` before the
`Import Sdk="Microsoft.NET.Sdk" Project="Sdk.props"` at the end of the file; the
SDK's own `MSBuildProjectExtensionsPath` default (`Microsoft.Common.props`,
conditioned on the property being empty) therefore never overwrites ours.

### Precondition check — `AssemblyName` defined before the import

Verified in all four in-scope projects (line of `<AssemblyName>` before the
`Nemerle.Sdk.props` import):

| project | AssemblyName | import |
|---|---|---|
| `Nemerle.nproj` | :11 `Nemerle` | :22 |
| `Nemerle.Compiler.nproj` | :11 `Nemerle.Compiler` | :21 |
| `Nemerle.Macros.nproj` | :11 `Nemerle.Macros` | :20 |
| `ncc-core.nproj` | :9 `ncc-core` (guarded) | :18 |

Also checked for collateral (not restructured, not required): `ncc.nproj`
(:9 before :18) likewise defines it early. `Nemerle.MSBuild.Tasks.csproj` does
NOT import `Nemerle.Sdk.props` (it is a plain SDK csproj; the grep hit at :42 is
a `<Copy>` source path), so it keeps the default root `obj\` — which is what
keeps the classic NTasks chain's assets there and out of the CLI builds' way.
Snippet projects under `snippets/` that import the props without an early
`AssemblyName` are built only via the stage chain (condition 1 fires) or, if the
new condition ever fires with an empty `AssemblyName`, the value normalizes to
`<projdir>\obj\` = the old default — harmless fallback, no behavior change.

### Deployment note

Projects consume the props from the boot-dir copy
`bin\Debug\net-4.0\boot\Nemerle.Sdk.props`, refreshed from `tools\msbuild-task\`
by the NTasks target. Classic Stage1 (acceptance A) is therefore run BEFORE the
`dotnet build` acceptance so the boot copy is current.

## Acceptance plan

- A. Classic regression: `Build-core-base.cmd Debug Build Stage1` → exit 0
  (also refreshes the boot copy of the props).
- B. From repo root, in order, each exit 0:
  `dotnet build Nemerle.nproj -v:m -o C:\RSDN\dotnetbuild-selftest\nemerle`,
  `dotnet build Nemerle.Compiler.nproj -v:m -o C:\RSDN\dotnetbuild-selftest\compiler`,
  `dotnet build Nemerle.Macros.nproj -v:m -o C:\RSDN\dotnetbuild-selftest\macros`;
  then the same three commands a SECOND time in the same order (idempotency).
- C. Classic Stage1 again → exit 0 and `bin\Debug\net-4.0\Stage1\` has
  Nemerle.dll, Nemerle.Compiler.dll, Nemerle.Macros.dll, ncc.exe from this run.
- D. `git diff --stat`: only `tools/msbuild-task/Nemerle.Sdk.props` (+ this doc).

## Evidence

### Property resolution (direct, after edit, both hosts)

```
> dotnet msbuild Nemerle.nproj          -t:Restore -getProperty:MSBuildProjectExtensionsPath
  C:\RSDN\nemerle\obj\Nemerle\
> dotnet msbuild Nemerle.Compiler.nproj -t:Restore -getProperty:MSBuildProjectExtensionsPath
  C:\RSDN\nemerle\obj\Nemerle.Compiler\
> dotnet msbuild Nemerle.Macros.nproj   -t:Restore -getProperty:MSBuildProjectExtensionsPath
  C:\RSDN\nemerle\obj\Nemerle.Macros\
> MSBuild.exe Nemerle.Compiler.nproj -t:Restore -getProperty:MSBuildProjectExtensionsPath
        -p:IntermediateOutputPath=bin\Debug\net-4.0\Stage1\ -p:Configuration=Debug
  C:\RSDN\nemerle\bin\Debug\net-4.0\Stage1\Nemerle.Compiler\        (condition 1, unchanged)
```

### A. Classic regression after the edit — PASS

```
> cmd /c "Build-core-base.cmd Debug Build Stage1"
  Build succeeded.  20 Warning(s)  0 Error(s)   EXIT=0
  boot copy refreshed: (Get-FileHash tools\msbuild-task\Nemerle.Sdk.props)
                      == (Get-FileHash bin\Debug\net-4.0\boot\Nemerle.Sdk.props) → True
```

### B. `dotnet build` x3, run TWICE in order — PASS (was BLOCKED before the fix)

Output dir `C:\RSDN\dotnetbuild-selftest\` deleted first (fresh evidence). Each
invocation from repo root:

| # | command | exit |
|---|---|---|
| r1 | `dotnet build Nemerle.nproj -v:m -o …\nemerle` | 0 |
| r1 | `dotnet build Nemerle.Compiler.nproj -v:m -o …\compiler` | 0 |
| r1 | `dotnet build Nemerle.Macros.nproj -v:m -o …\macros` | 0 (1879 dnlib errors GONE; only the 14+ pre-existing source warnings, `Build succeeded`) |
| r2 | same three, same order | 0 / 0 / 0 (idempotent — restores do not clobber each other) |

Isolation verified afterwards:

```
obj\Nemerle\project.assets.json           exists (restore of Nemerle graph)
obj\Nemerle.Compiler\project.assets.json  contains 'dnlib': True
obj\Nemerle.Macros\project.assets.json    exists (no package refs)
obj\project.assets.json                   UNTOUCHED — LastWriteTime still
  30.10.2026 20:23:16 (pre-B), tasks graph: 'Microsoft.Build': True, 'dnlib': False
```

Residual note: CLI P2R sub-builds still drop plain build intermediates
(per-project-named caches/dlls) into the shared `obj\Debug\` — they cannot
collide on assets files anymore (extensions paths are per-project) and none
land in the classic chain's paths; harmless.

### C. Classic Stage1 after B — PASS (with one transient, known environment hiccup)

First attempt failed: `error MSB3027/MSB3021: Could not copy
boot-dnlib\netstandard.dll → bin\Debug\net-4.0\boot\netstandard.dll … locked by
".NET Host (38520)"` — a lingering dotnet-CLI persistent MSBuild node holding
the boot dll; the exact transient already recorded by the 2.3a/2.3b subagent
("resolved with `dotnet build-server shutdown`"). Not a build-logic failure:
`dotnet build-server shutdown` + immediate retry succeeded.

The retry and one follow-up run were fully incremental (1.3 s, outputs in
`bin\…\Stage1\` are timestamp-preserving copies of the real compiles in
`obj\Debug\net-4.0\Stage1\`, which the stage chain keeps under its own
`IntermediateOutputPath` — condition 1 worked as designed, note the Stage1
restore messages referencing `obj\Debug\net-4.0\Stage1\<AssemblyName>\
project.assets.json`). To force a REAL recompile for the timestamp requirement,
`obj\Debug\net-4.0\Stage1\{Nemerle,Nemerle.Compiler,Nemerle.Macros}.dll/.pdb,
ncc.exe/.pdb` (+ bin copies) were deleted and Stage1 re-run:

```
> cmd /c "Build-core-base.cmd Debug Build Stage1"
  Build succeeded.  20 Warning(s)  0 Error(s)   Time Elapsed 00:00:38.74  EXIT=0
  bin\Debug\net-4.0\Stage1\:  Nemerle.dll         30.10.2026 20:32:06  302080 B
                              Nemerle.Compiler.dll 30.10.2026 20:32:23 1690112 B
                              Nemerle.Macros.dll  30.10.2026 20:32:36  600064 B
                              ncc.exe             30.10.2026 20:32:38    9216 B
                              (Get-Date at check: 20:32:40 — all from this run;
                               sizes identical to pre-B builds)
```

NTasks compiled against the intact root-`obj` tasks assets (no CS0246), and the
tasks `CoreCompile`/assets reported up-to-date throughout — the classic chain
consumed none of the CLI restores.

### D. Diff scope — PASS

```
> git diff --stat
  Nemerle.Compiler.nproj                            |  2 ++   (2.3a, pre-existing)
  Nemerle.Macros.nproj                              |  2 ++   (2.3a, pre-existing)
  Nemerle.nproj                                     |  2 ++   (2.3a, pre-existing)
  doc/plans/retarget-compiler-to-net10-checklist.md |  4 ++--  (pre-existing)
  tools/msbuild-task/Nemerle.MSBuild.targets        | 18 ++++…(2.3b, pre-existing)
  tools/msbuild-task/Nemerle.Sdk.props              |  2 ++   ← THIS TASK, the only new change
  tools/msbuild-task/Nemerle.Sdk.targets            | 13 ----- (2.4a, pre-existing)
```

Plus this untracked progress doc. `git ls-files --eol` on the props: index
`crlf` == worktree `crlf` (preserved). No commits made.

## Result

**2.3c DONE.** The shared-root-`obj` implicit-restore collision is fixed with a
2-line addition to `tools/msbuild-task/Nemerle.Sdk.props`; CLI `dotnet build`
of the three root .nprojs now works repeatedly in any order while the classic
stage chain (which always passes `IntermediateOutputPath`) resolves byte-
identical extensions paths to before.

Follow-ups observed (not done here):
- the boot-dll file lock by persistent dotnet-CLI nodes (fixed procedurally by
  `dotnet build-server shutdown`) will recur whenever CLI and classic builds
  interleave; consider documenting it in the plan or adding a pre-B shutdown;
- `Nemerle.MSBuild.Tasks.csproj` still keeps its restore in the root `obj\`
  (by design for now — CLI builds of the .nprojs no longer touch it); the
  `EnsurePackagesRestored`-doesn't-fire-under-NTasks item from the 2.3a/2.3b
  notes remains open.
