# Progress — sub-points 2.3a, 2.3b, 2.4a

Branch `retarget-compiler` @ 1cb57f94b (tree clean at start except pre-existing
`doc/plans/retarget-compiler-to-net10-checklist.md` modification and untracked
`AGENTS.md` / plan docs). Per report
`retarget-compiler-to-net10-plan-report2.3-2.4.md` (Q3.1, Q3.2, Q4). No commits.

## Scope (exactly three change groups)

1. **2.3a** — guarded `$(Nemerle)` fallback default in `Nemerle.nproj`,
   `Nemerle.Compiler.nproj`, `Nemerle.Macros.nproj` (report Q3.1). Keep the
   existing `boot-dnlib` default; add a second conditional `<Nemerle>` that
   fires only when (a) `$(Nemerle)` still equals the first default's literal
   (so builds passing `Nemerle=` explicitly — the stage chain always passes
   `Nemerle=$(NPrevBin)` — are never touched), (b) `$(MSBuildProjectDirectory)\boot-dnlib\Nemerle.Sdk.props`
   does not exist (seed dir carries ncc.exe + dlls only), and (c)
   `$(MSBuildThisFileDirectory)bin\Debug\net-4.0\boot\Nemerle.Sdk.props`
   exists (populated by any NemerleAll `NTasks` run).
2. **2.3b** — `tools/msbuild-task/Nemerle.MSBuild.targets`: gate the
   ResolveMacroAssemblyReferences machinery (property group :207-211 and the
   target :212-255 that feed `$(TargetFrameworkDirectory)`/TFR inputs) behind
   `'$(MSBuildRuntimeType)' != 'Core'`; under the Core host wire a companion
   target that emits `<Error Text="MacroReference is not supported when
   building with the .NET (Core) MSBuild host"/>` when `@(MacroReference)` is
   non-empty. Full-host evaluation sees identical effective values (added
   condition conjuncts are true on the Full host; the Core-only wiring never
   evaluates there). Nothing else touched — `:38` import and `CoreCompile`
   stay as-is.
3. **2.4a** — `tools/msbuild-task/Nemerle.Sdk.targets`: delete the dead
   `UsingTask MSBuild.Community.Tasks.TemplateFile` (:3) and the commented-out
   dead `AfterBuild` block referencing `TemplateFile`/`AL` (:4-15). Nothing else.

## Baseline evidence (taken BEFORE any edit)

`dotnet --version` → `10.0.402` (MSBuild 18.9.15, `MSBuildRuntimeType=Core`).

```
> dotnet msbuild Nemerle.nproj -t:__nope__ -v:m
Nemerle.nproj(20,3): error MSB4019: The imported project
"C:\RSDN\nemerle\boot-dnlib\Nemerle.Sdk.props" was not found. …
EXIT=1
```

Fallback dir state: `bin\Debug\net-4.0\boot\Nemerle.Sdk.props` exists (True),
`boot-dnlib\Nemerle.Sdk.props` absent (False), `boot-dnlib\ncc.exe` present.

## Changes made

### 2.3a — guarded `$(Nemerle)` fallback (3 root .nproj)

Added directly after the existing default in each file (same style), two lines:

```xml
    <!--Fallback to the boot dir populated by a NemerleAll run when the seed lacks the MSBuild files; never overrides an explicitly passed Nemerle=.-->
    <Nemerle Condition=" '$(Nemerle)' == '$(MSBuildProjectDirectory)\boot-dnlib' And !Exists('$(MSBuildProjectDirectory)\boot-dnlib\Nemerle.Sdk.props') And Exists('$(MSBuildThisFileDirectory)bin\Debug\net-4.0\boot\Nemerle.Sdk.props') ">$(MSBuildThisFileDirectory)bin\Debug\net-4.0\boot</Nemerle>
```

Rationale for the `'$(Nemerle)' == '<first default literal>'` guard: MSBuild
evaluates properties in order, so after the original line `$(Nemerle)` is
never empty; comparing against the default's own expansion is true exactly
when the default applied (an explicitly passed value — any stage's
`Nemerle=$(NPrevBin)`, which never equals this literal — short-circuits the
condition and sees byte-identical behavior).

### 2.3b — `'$(MSBuildRuntimeType)'` gate on the MacroReference TFR block

`tools/msbuild-task/Nemerle.MSBuild.targets` (+16/−2):

- `:207` wiring `<PropertyGroup>` → `Condition=" '$(MSBuildRuntimeType)' != 'Core' "`
  (under Core the macro target is never wired into `ResolveReferencesDependsOn`,
  so its `$(TargetFrameworkDirectory)`/installed-assembly-table machinery never runs).
- `:214` target condition `" '@(MacroReference)' != '' "` →
  `" '@(MacroReference)' != '' and '$(MSBuildRuntimeType)' != 'Core' "`
  (belt-and-braces; direct `-t:ResolveMacroAssemblyReferences` also skipped on Core).
- After `</Target>`: Core-only property group wiring
  `ErrorOnMacroReferenceUnderCoreHost` into `ResolveReferencesDependsOn`, plus:

```xml
  <Target
      Name="ErrorOnMacroReferenceUnderCoreHost"
      Condition=" '@(MacroReference)' != '' ">
    <Error Text="MacroReference is not supported when building with the .NET (Core) MSBuild host"/>
  </Target>
```

On the Full host every new condition is true/false-neutral, so effective
`ResolveReferencesDependsOn`/`ResolveAssemblyReferencesDependsOn` values and
target execution are identical to before (verified by acceptance C: Stage1
consumed the edited files — boot copies hash-match `tools\msbuild-task\` —
and built clean). Neither the `:38` import nor `CoreCompile` were touched.
Neither `NemerleAll.nproj` nor any `MacroReference`-consuming project
(`Nemerle.Location.nproj`, `ncc-dnlib.nproj`) was touched.

### 2.4a — delete dead TemplateFile UsingTask + commented block

`tools/msbuild-task/Nemerle.Sdk.targets` is now (−13 lines):

```xml
<Project>
  <Import Sdk="Microsoft.NET.Sdk" Project="Sdk.targets" />
</Project>
```

(The file had no trailing newline before; still has none. EOL check:
`git ls-files --eol` → index `crlf` == worktree `crlf`. The 3 .nproj and
`Nemerle.MSBuild.targets` are LF in index and worktree — unchanged.)

## Acceptance evidence

### A. Evaluation-only under .NET host, no extra properties — PASS

(after edits; run twice — after B and again after C/obj recovery, same result)

```
> dotnet msbuild Nemerle.nproj          -t:__nope__ -v:m
Nemerle.nproj : error MSB4057: The target "__nope__" does not exist in the project.
> dotnet msbuild Nemerle.Compiler.nproj -t:__nope__ -v:m   → MSB4057 only
> dotnet msbuild Nemerle.Macros.nproj   -t:__nope__ -v:m   → MSB4057 only
```

MSB4019 is gone; residual is exactly the allowed MSB4057.

### B. Real compile via .NET host — steps 1-2 PASS, step 3 BLOCKED (root cause OUT OF SCOPE)

```
> dotnet build Nemerle.nproj         -v:m -o C:\RSDN\dotnetbuild-selftest\nemerle
  Nemerle -> C:\RSDN\dotnetbuild-selftest\nemerle\Nemerle.dll   BUILD SUCCEEDED EXIT=0
  (Nemerle.dll 302080 B, freshly produced)
> dotnet build Nemerle.Compiler.nproj -v:m -o C:\RSDN\dotnetbuild-selftest\compiler
  Nemerle.Compiler -> ...\compiler\Nemerle.Compiler.dll          BUILD SUCCEEDED EXIT=0
  (14 pre-existing source warnings, 0 errors; also rebuilt Nemerle.dll into the dir)
> dotnet build Nemerle.Macros.nproj   -v:m -o C:\RSDN\dotnetbuild-selftest\macros
  FAILED EXIT=1 — 1879 errors, all in [Nemerle.Compiler.nproj] (the P2R rebuild):
  dnlib members unbound ("unbound type name DN.TypeSig", …)
```

**Root cause (verified, OUT OF SCOPE — STOP per instructions).** All projects
that live at the repo root — the 3 .nprojs AND `Nemerle.MSBuild.Tasks.csproj`
— share the single default `MSBuildProjectExtensionsPath` (`obj\`), because
`Nemerle.Sdk.props:9` redirects it to `$(IntermediateOutputPath)$(AssemblyName)\`
ONLY when `IntermediateOutputPath` is passed (the stage chain), and the tasks
csproj never redirects at all. The `dotnet build` CLI runs an implicit restore
that rewrites the per-directory `obj\project.assets.json` with the *entry*
project's graph. The Macros build therefore:
1. restore → `obj\project.assets.json` := Macros graph — verified afterwards:
   file contained only `NETStandard.Library`/`Microsoft.NETCore.Platforms`,
   `contains 'dnlib': False`;
2. rebuilds P2R `Nemerle.Compiler.nproj`, whose `ResolvePackageAssets` reads
   the SAME file → dnlib (its PackageReference) absent → ncc sees no dnlib →
   1879 dnlib errors.

Nothing in the three edited files influences restore, `MSBuildProjectExtensionsPath`,
PackageReference resolution or `@(ReferencePath)` for the 3 projects (their
`@(ReferencePath)` comes from the Sdk per report Q3); the defect is pre-existing
(dormant until a CLI build reaches step 3) and is exactly the report-Q5
obj-layout hazard, deferred to 2.3c triage. The proper fix lives in
`tools/msbuild-task/Nemerle.Sdk.props` (give CLI builds a per-project
`MSBuildProjectExtensionsPath`, e.g. `obj\$(MSBuildProjectName)\`) — outside
the three in-scope files, so NOT attempted here. Note the file itself is only
copied from `tools\msbuild-task\` to boot by NTasks, so the fix can land there
without touching the boot dir directly.

**Side effect observed and recovered:** the same shared `obj\` is where the
tasks csproj keeps its own restore output. My step-3 CLI restore overwrote
`obj\project.assets.json` (tasks graph → nproj graph), which then broke the
*classic* NTasks compile with CS0246 (`Microsoft.Build.*` refs lost) on the
next Stage1. Recovery performed: deleted the transient root `obj\` NuGet files
and regenerated them via the csproj's own `EnsurePackagesRestored` target
(`/t:EnsurePackagesRestored`, classic MSBuild; regenerates
`obj\project.assets.json` + `Nemerle.MSBuild.Tasks.csproj.nuget.*` with the
tasks graph). Post-recovery Stage1 then fully succeeded (see C). Separate
observation for 2.3c: the `EnsurePackagesRestored` BeforeTargets="Build"
injection did NOT fire when the csproj was built via the NTasks
`<MSBuild Targets="Build">` call (zero target mentions in a /v:diag log), so
the self-heal only works when the target is invoked explicitly — csproj-side
robustness item, also out of scope here. First Stage1 attempt also hit a
transient file lock on `bin\Debug\net-4.0\boot\netstandard.dll` held by a
lingering dotnet-CLI persistent MSBuild node (PID 26608); resolved with
`dotnet build-server shutdown`.

### C. Classic-host regression — PASS

```
> cmd /c "Build-core-base.cmd Debug Build Stage1"      (after obj recovery)
  … NTasks rebuilt Nemerle.MSBuild.Tasks.dll (references resolved from the
    tasks-graph assets), boot copies hash-match the edited tools files:
    Get-FileHash tools\msbuild-task\Nemerle.MSBuild.targets == boot copy: True
    Get-FileHash tools\msbuild-task\Nemerle.Sdk.targets   == boot copy: True
  … Stage1 built Nemerle, Nemerle.Compiler, Nemerle.Macros, ncc, ncc-core …
  Build succeeded.    0 Error(s)
EXIT=0
```

Proves Full-host behavior unchanged with the edited targets (the classic chain
consumed the NEW text via the NTasks copies).

### D. Diff scope — PASS

```
> git diff --stat
 Nemerle.Compiler.nproj                            |  2 ++
 Nemerle.Macros.nproj                              |  2 ++
 Nemerle.nproj                                     |  2 ++
 doc/plans/retarget-compiler-to-net10-checklist.md |  4 ++--   (PRE-EXISTING, not this task)
 tools/msbuild-task/Nemerle.MSBuild.targets        | 18 ++++++++++++++++--
 tools/msbuild-task/Nemerle.Sdk.targets            | 13 -------------
```

Plus this untracked progress doc. No commits made.

## Handoff notes for 2.3c / 2.4b

- 2.3c blocker is the shared-root-`obj` assets collision above; suggested fix
  site: `tools/msbuild-task/Nemerle.Sdk.props` (per-project
  `MSBuildProjectExtensionsPath` when `IntermediateOutputPath` is absent —
  must stay inert for the stage chain, which already isolates via `:9`), and
  likely the same for `Nemerle.MSBuild.Tasks.csproj`.
- After any CLI `dotnet build` of a root project, the classic chain needs the
  tasks-csproj assets in root `obj\` to be intact (see recovery above) until
  2.3c isolates them.
- The Core-host `ErrorOnMacroReferenceUnderCoreHost` stays latent until a
  Core-built project declares `MacroReference` (none do today).
