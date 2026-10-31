# Progress 5.3 - Directory.Build.props + nuget.config; .sln-as-orchestrator analysis

Status: DONE (gates A-D PASS)
Branch: retarget-compiler
Date: 2026-10-31

## Intent

Sub-atom 5.3 (plan Phase 5.3): "Ввести Directory.Build.props (общий
TargetFramework/LangVersion/Nullable/TreatWarningsAsErrors) и nuget.config.
Рассмотреть настоящий .sln/.slnx вместо NemerleAll.nproj как оркестратора."

Orchestrator scoping (binding): introduce the props file but populate it ONLY
with properties that provably do not change today's resolved values; never set
TargetFramework globally; never set TreatWarningsAsErrors=true; a nearly-empty
props file is an acceptable deliverable. Produce a written .sln analysis, no
.sln file.

## Pre-edit audit

* SDK-style audit: `git grep -l "Microsoft.NET.Sdk" -- "*.csproj"` lists
  exactly ONE project: `Nemerle.MSBuild.Tasks.csproj` (netstandard2.0, two
  PackageReferences: Microsoft.Build.Utilities.Core 17.0.0 and
  Microsoft.Build.Framework 17.0.0). All other csproj/fsproj live under
  `snippets/` or `tools/cs2n` and are outside the NemerleAll build graph.
* **Correction to the scoping premise**: the scoping note assumed .nproj files
  ignore Directory.Build.props ("old-style projects do NOT import it").
  That is false *for this repo*: every .nproj imports
  `$(Nemerle)\Nemerle.Sdk.props`, which ends with
  `<Import Sdk="Microsoft.NET.Sdk" Project="Sdk.props" />`; Sdk.props imports
  Microsoft.Common.props, which imports the nearest Directory.Build.props.
  Verified empirically (canary probe below): a root props file IS picked up by
  Nemerle.nproj, ncc-core.nproj and the whole stage chain, not just the csproj.
  Consequence: the blast radius of the props file is the ENTIRE bootstrap, so
  every property in it must be zero-delta.
* `NemerleAll.nproj` itself imports nothing (no Microsoft.Common.props /
  Sdk.props) - the orchestrator project is NOT affected by the props file.
* NuGet: no NuGet.config/nuget.config is tracked (`git ls-files "*uget*"`);
  restore sources today come from the USER-level `%APPDATA%\NuGet\NuGet.Config`
  (internal artifactory mirrors `r-nuget`, `nuget-monorepo`, a VS offline
  folder; nuget.org is not in the user list). `https://api.nuget.org/v3/index.json`
  answers 200 from this machine, and both required packages exist in the global
  cache, so a repo-scoped `<clear/>` + nuget.org config restores reproducibly.
* `$(Nuget)` (ExternalDependences\NuGet.exe, NemerleAll.nproj:176) is only
  used by installer/IDE-path targets, not by Stage*/Validate/CompilerTests.

## Resolved-value probes (before)

`dotnet msbuild <proj> -getProperty:...` (SDK 10.0.402; ncc-core probed with
`/p:Nemerle=bin\Debug\net-4.0\boot`):

| Project | LangVersion | Nullable | TreatWarningsAsErrors | TargetFramework |
|---|---|---|---|---|
| Nemerle.MSBuild.Tasks.csproj | 7.3 (TFM default) | (empty) | false | netstandard2.0 |
| Nemerle.nproj | (empty) | (empty) | false | netstandard2.0 |
| ncc-core.nproj | (empty) | (empty) | false | net10.0 |
| NemerleAll.nproj | (empty) | (empty) | (empty) | (n/a) |

## Property decisions

* TargetFramework - DROP. Values genuinely differ per project
  (netstandard2.0 libs / net10.0 ncc-core / harnesses); a global value would
  break the chain. Documented as a comment in the props file.
* LangVersion - DROP. Only one C# project exists (resolves 7.3, the
  netstandard2.0 default); the .nproj resolve empty (Nemerle language targets
  do not set it, ncc ignores it). Any single global literal either changes
  the .nproj resolved value ("" -> "7.3") or shadows the per-TFM default.
* Nullable - DROP. Resolves empty everywhere; SDK 10 passes
  `Nullable="$(Nullable)"` to the Csc task, and Csc treats empty exactly as
  `disable` (nullable disabled by default below C# 8). Setting the literal
  `disable` is semantically a no-op but is a literal "" -> "disable" delta in
  every project - not taken, since zero-delta is the bar.
* TreatWarningsAsErrors - KEEP as `false`. Probes show `false` (or empty ->
  task default false) everywhere today, so the literal is identity. Comment in
  the file records that flipping it to true is a post-6.x cleanup (the
  compiler regression suite has pre-existing warnings).

## nuget.config

`nuget.config` at repo root: `<packageSources><clear/><add key="nuget.org"
value="https://api.nuget.org/v3/index.json" protocolVersion="3"/></packageSources>`.
Repo-scoped (directory semantics), so user/machine feeds stop leaking into
builds of this repo; nuget.org covers both PackageReferences. Restore proof:
isolated-cache restore (`NUGET_PACKAGES` pointed at an empty dir) succeeds
using ONLY the repo config (log `.tmp\phase53\restore-isolated.log`), and the
full gates below exercise the in-build `Restore` targets.

## .sln/.slnx as orchestrator - analysis (no file created)

A .sln/.slnx cannot replace NemerleAll.nproj today, for structural reasons:
1. The stage chain (boot-net10 -> Stage1 -> Stage2 -> Stage3, each stage's
   ncc compiling the next) is data-driven in NemerleAll targets via
   `MSBuild Projects=@(NCompilerProject) Properties="Nemerle=$(NPrevBin)"`
   - the toolset is a *build output of the previous stage*. Solution-level
   project-to-project references cannot pass "use the compiler this very
   build produced in project X" as an MSBuild property; there is no stage
   ordering primitive in .sln other than ProjectDependencies, which cannot
   vary properties per stage.
2. Targets are parameterized through NTargetName (Build|Rebuild) and flow
   into every sub-project invocation; `slnx`/MSBuild solution filters offer
   configuration sets, not property-templated target fan-out.
3. Stage state (NCurBin/NPrevBin per invocation, Validate's IL-diff of the
   last two stages, Install copying the last built stage) is imperative
   orchestration logic; a solution file is a declarative project list.
4. What a .sln WOULD buy today: IDE/dev UX (F5 on ncc, solution folders for
   snippets) and NuGet/IDE restore discoverability. Neither touches CI gates.
5. Recommendation: DEFER. Revisit after 6.3 removes the last host-detection
   blockers (GetFrameworkSdkPath/ildasm discovery in InitTools); at that
   point stage ordering could be expressed as a real SDK-style project graph
   (`dotnet build` with #pragma references or P2P + GlobalReference of the
   per-stage ncc) and a .slnx becomes a thin view over that graph, not the
   orchestrator itself. Creating a .sln now would duplicate orchestration
   truth in two places and rot immediately.

## Steps

1. This progress file (first).
2. Canary probe proving Directory.Build.props reaches .nproj evaluation.
3. Create `Directory.Build.props` (TreatWarningsAsErrors=false only +
   documentation of the dropped properties) and `nuget.config`
   (<clear/> + nuget.org). CRLF, UTF-8 no BOM (matches tools/msbuild-task/
   Nemerle.Sdk.props i/crlf).
4. Post-create probes: -getProperty table identical to "before" for all four
   probe projects; isolated-cache restore proof.
5. Gate B: `./build.ps1 -Targets Stage3,Validate -Rebuild`
   (log `.tmp\phase53\validate.log`).
6. Gate C: `./build.ps1 -Targets Stage1,CompilerTests`
   (log `.tmp\phase53\tests.log`).
7. `git status` shows exactly the three new files.

## Canary probe (import-mechanism proof)

With a temporary `<NemerleP53Canary>yes</NemerleP53Canary>` in the props file,
`dotnet msbuild <proj> -getProperty:NemerleP53Canary` returned `yes` for
Nemerle.MSBuild.Tasks.csproj, Nemerle.nproj and ncc-core.nproj (all evaluate
the root props) and empty for NemerleAll.nproj (imports no common props).
The canary was removed before the gates; the final props file is the one
committed. Evidence: `.tmp\phase53\props-probes.log`.

## Resolved-value probes (after, canary removed)

Identical to the "before" table for all four projects (see
`.tmp\phase53\props-probes.log`): LangVersion 7.3/(empty), Nullable (empty),
TreatWarningsAsErrors false, per-project TargetFramework unchanged.

## Gate results

* Gate B: `./build.ps1 -Targets Stage3,Validate -Rebuild` ->
  `FC: no differences encountered` x5, zero `: error` lines, exit 0.
  Log: `.tmp\phase53\validate.log` (2026-10-31).
* Gate C: `./build.ps1 -Targets Stage1,CompilerTests` -> 468 positive +
  167 negative passed / 0 failed ("All tests passed."), zero `: error`
  lines, exit 0. Log: `.tmp\phase53\tests.log`.
* Restore proof: `dotnet restore Nemerle.MSBuild.Tasks.csproj` with
  `NUGET_PACKAGES` pointed at an empty directory and only the repo
  `nuget.config` in scope: success (30 packages downloaded from nuget.org),
  exit 0. Log: `.tmp\phase53\restore-isolated.log`.
