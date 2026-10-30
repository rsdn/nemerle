# Report — splitting plan sub-points 2.3 & 2.4 into atomic steps

Branch `retarget-compiler` @ 1cb57f94b. Evidence: read-only inspection + two
**evaluation-only** probes (`dotnet msbuild <p> -t:__nope__`; no compile target ran).
Paths relative to repo root; SDK 10.0.402 at `<sdk>`.

## Q1. Import graph a .nproj build executes (both hosts, same chain)

1. `Nemerle.nproj:2-19` props (`Nemerle` defaults to `boot-dnlib`, :3) → :20 imports
   `$(Nemerle)\Nemerle.Sdk.props`.
2. `tools/msbuild-task/Nemerle.Sdk.props`: `EnableDefaultItems=false`,
   `GenerateAssemblyInfo=false`, `AppendTargetFrameworkToOutputPath=false`,
   `LanguageTargets=Nemerle.MSBuild.targets` (:8),
   `MSBuildProjectExtensionsPath=$(IntermediateOutputPath)$(AssemblyName)\` only when
   `IntermediateOutputPath` set (:9); imports `Sdk.props` (:11) → `Microsoft.Common.props`.
3. `Nemerle.nproj:43` → `Nemerle.Sdk.targets:2` → Sdk `Sdk.targets`
   (`<sdk>\Sdks\Microsoft.NET.Sdk\Sdk\Sdk.targets`): `Microsoft.NET.Sdk.BeforeCommon.targets`,
   then `Import Project="$(LanguageTargets)"` — **Sdk.targets:25-38; the LanguageTargets
   hook exists in the .NET SDK too**, then `Microsoft.NET.Sdk.targets`
   (`ResolvePackageAssets`/`ProcessFrameworkReferences` feed `@(ReferencePath)`;
   `Microsoft.NET.GenerateAssemblyInfo.targets` imported at Sdk.targets:1376 — inert,
   `GenerateAssemblyInfo=false`).
4. `Nemerle.MSBuild.targets`: `UsingTask Ncc` (:3-5); `Import
   $(MSBuildBinPath)\Microsoft.Common.targets` (:38) — on **both** hosts a shim →
   `Microsoft.Common.CurrentVersion.targets` (full `Build`/`Clean`/`ResolveReferences`/
   `ResolveProjectReferences`/`CopyFilesToOutput`, `MSBuildAllProjects`, stock app.config
   handling — none of the 3 projects has App.config).

Nemerle **defines**: `ResolveMacroAssemblyReferences` + `ResolveReferencesDependsOn`
wiring (:207-255; fires only when `@(MacroReference)` non-empty),
`SplitProjectReferencesByType` (:117, MSBuild-3.5 relic, unreferenced by v4 Common),
`_SetTargetFrameworkMonikerAttribute` (:40), `_GenerateAssemblyInfo` (:49, gated
`GenerateAssemblyInfo==true` — inert). **Overrides**: `AssignProjectConfiguration` (:164),
`CreateManifestResourceNames` (:280, `CreateCSharpManifestResourceName` — ships in
`Microsoft.Build.Tasks.Core`, both hosts), `CoreCompile` (:322-381 → `Ncc` ToolTask; the
C# CoreCompile is never imported since LanguageTargets replaced CSharp.targets).
**Relies on**: `PrepareForBuild`, `ResolveReferences`/`ResolveProjectReferences`
(`MacroProjectReference` ItemDefinition :96-101 → `@(ReferencePath)` with
`OutputItemType=macro`, split in CoreCompile :342-343), `_ComputeNonExistentFileProperty`
(:320), `TargetsTriggeredByCompilation` (:380), `CopyFilesToOutput`, Clean/FileWrites.
`UseHostCompilerIfAvailable` (:22) is noise. `Nemerle.Sdk.targets:3` `UsingTask
TemplateFile` is the only live Community.Tasks reference on the .nproj path; its
consumers (`TemplateFile`, `AL`, :13-14) sit in a **commented-out** `AfterBuild` block (:4-15).

## Q2. What breaks under `dotnet msbuild` (probe evidence)

- **Probe A** `dotnet msbuild Nemerle.nproj -t:__nope__` →
  `Nemerle.nproj(20,3): error MSB4019: … boot-dnlib\Nemerle.Sdk.props was not found`.
  Seed `boot-dnlib\` (git ls-files) = ncc.exe + dlls only; props/targets/tasks-dll live
  in `bin\<Config>\net-4.0\boot\` (`NTasks`, `NemerleAll.nproj:201-224` +
  `Nemerle.MSBuild.Tasks.csproj:CopyNemerleTargetsToOutput`). **The only hard blocker.**
- **Probe B** + `-p:Nemerle=…\bin\Debug\net-4.0\boot` → evaluation **clean** (only
  MSB4057). Common.targets, Ncc UsingTask and Sdk wiring all load under SDK 10.
- Corrected context assumptions:
  - `Microsoft.Common.targets` **exists in the dotnet SDK** (verified
    `<sdk 10.0.402>\Microsoft.Common.targets`, shim to `…CurrentVersion.targets`). Not a break.
  - `UsingTask MSBuild.Community.Tasks.*` is **lazy** — no evaluation failure (Probe B);
    it fails only at task instantiation: the dll is image `v4.0.30319` referencing
    `Microsoft.Build.Utilities.v4.0` (reflection), unloadable in the Core host. No
    acceptance-path target instantiates it.
- `GetReferenceAssemblyPaths` is guarded by `'$(TargetFrameworkMoniker)' != ''`
  (SDK Common.CurrentVersion.targets:1259-1260) → **skipped** for netstandard2.0/
  netcoreapp2.1 on both hosts; only `ncc.nproj` (net461) would hit it and fail under
  Core — that project leaves the build set in Phase 3.4.
- `Nemerle.MSBuild.targets:218-251` (MacroReference RAR): TFR inputs empty on Core, but
  the target condition (:214) needs `@(MacroReference)` — absent from the 3 projects +
  ncc-core (`git grep`: only `Nemerle.Location.nproj:62`, `ncc-dnlib.nproj:52`, snippets).
  Latent only.
- `Ncc` ToolTask (netstandard2.0, both hosts — 2.1/2.2 report):
  `FindExecutable` roots `ncc.exe` under `CompilerPath=$(Nemerle)`
  (`tools/msbuild-task/MSBuildTask.cs:107-141`); the Framework-exe child runs fine from
  the Core host on Windows; `ToolPath=$(CscToolPath)` (:373) matters only in the
  not-found fallback. `GetFrameworkSdkPath`/`GetFrameworkPath` (`NemerleAll.nproj:154-162`)
  are classic-host orchestrator only (Phase 5.1).

## Q3. Minimal-diff design for 2.3 — keep ONE targets file, change almost nothing

The premise ("SDK reference resolution instead of GetReferenceAssemblyPaths") is
already true: `@(ReferencePath)` for the 3 projects comes from the Sdk on both hosts;
the TFR machinery feeds only the MacroReference target, never entered by them.
Falling back to stock SDK targets on Core (conditional `LanguageTargets`) is **not an
option** — .n sources need `CoreCompile`/`Ncc` in whichever host compiles them.

1. Close the MSB4019 gap **without touching the seed dir** (forbidden per checklist
   header): acceptance command
   `dotnet build <Nemerle|Nemerle.Compiler|Nemerle.Macros>.nproj -p:Nemerle=bin\Debug\net-4.0\boot`
   (populated by any NemerleAll run); optionally a guarded `Exists('…boot\Nemerle.Sdk.props')`
   fallback default in the root .nprojs. Classic chain unaffected — stages always pass
   `Nemerle=$(NPrevBin)` (`NemerleAll.nproj:252,259,270,281,292`).
2. Core-hygiene gate: wrap `Nemerle.MSBuild.targets:218-219` items and params :232,247,249-251
   in `'$(MSBuildRuntimeType)' != 'Core'` property groups; under Core either resolve
   `@(MacroReference)` via plain RAR (`Assemblies`+`SearchPaths`) or emit
   `<Error Condition="'$(MSBuildRuntimeType)'=='Core' and '@(MacroReference)'!=''"/>`.
   **Full-host text stays byte-for-byte** → Stage1-3 / Validate IL fixed point untouched.
   (`MacroProjectReference` needs no RAR — it flows via `ResolveProjectReferences` with
   `OutputItemType=macro`; its consumers `_PegAndCSharp`/`_Statechart` build classic-only.)
3. Keep the `:38` import as-is (shim exists on both hosts; MSBuildBinPath = host dir).

## Q4. MSBuild.Community.Tasks users → replacements

| Use | Where | Purpose | Native replacement | On 3-nproj path? |
|---|---|---|---|---|
| `UsingTask TemplateFile` | `Nemerle.Sdk.targets:3` | registers task; consumers :13-14 (`TemplateFile`, `AL` policy block) **commented out** | delete line (+optional dead-block removal) | **YES — only 2.4 item needed for 2.5** |
| `UsingTask FileUpdate` | `NemerleAll.nproj:2` | registration for Validate | — | no (orchestrator, classic host) |
| `FileUpdate` ×6 | `NemerleAll.nproj:333-342` | strip `//` lines from ildasm `.il` before `fc` (duplicated Win/Unix pairs :337-338/:341-342) | pure MSBuild `ReadLinesFromFile` → transform `$([System.Text.RegularExpressions.Regex]::Replace(…,'^//.*','// REPLACED'))` → `WriteLinesToFile`, or `Exec pwsh -c "(gc f) -replace '^//.*','// REPLACED' | sc f"` | no — Phase 5 (with 5.1 ildasm/fc/peverify rework) |
| `TemplateFile`+`AL` (commented) | `Nemerle.Sdk.targets:13-14` | Framework policy-assembly gen — dead since netcore retarget | delete; no replacement | no |
| `TemplateFile`,`GetGitTagRevision`,`NSetEnvironmentVariable` | `snippets/VS2010/*.csproj:49-51,825,862 / :23-25,1118,1148` | IDE codegen | latter two are **Nemerle's own tasks** (kept in `Nemerle.MSBuild.Tasks.csproj:25-27`, netstandard2.0, both hosts); TemplateFile → generated-source item | no — IDE scope |
| `al.exe` path probe | `NemerleAll.nproj:164-170` | InitTools SDKBin | drop with 5.1 | no |

Minimal subset for 2.5: **delete `Nemerle.Sdk.targets:3`**. Note its AssemblyFile path
`$(MSBuildProjectDirectory)\ExternalDependences\…` only resolves for root projects —
another reason to delete rather than redirect.

## Q5. Side effects to watch

- **obj layout**: `MSBuildProjectExtensionsPath` defaults to `obj\` under `dotnet build`
  (Sdk.props:9 override applies only when `IntermediateOutputPath` is passed, i.e. stages
  → `$(NCurObj)$(AssemblyName)\`); dotnet-CLI artifacts can't collide with stage dirs.
  Stage2/3 asset reuse copies `$(NCurObj)\**\project.assets.json` (`NemerleAll.nproj:253-259,269`) — untouched.
- **`Nemerle` property** flows as a global property through `ProjectReference`
  (Macros→Compiler→Nemerle) identically in both hosts.
- **`AppendTargetFrameworkToOutputPath=false`** (Sdk.props:5) keeps `bin\Debug\` flat on
  both hosts; intermediate TFM-append already tolerated by stage globs — verify once at
  2.5, don't pre-fix.
- **Restore/versions**: dotnet `build` implies restore. **PackageReference confirmed**:
  dnlib 3.3.0 + System.CodeDom 4.7.0 only in `Nemerle.Compiler.nproj:37-38`; the other
  libs + ncc-core have no package refs. Classic Stage1 calls `Targets=Restore` explicitly
  (:252). `ncc\packages.config` (dnlib 3.2.0/net461) is **dead weight** — SDK-style
  `-t:Restore` ignores packages.config and no `<Reference>` points into `packages\`;
  retire with ncc.nproj in 3.4.
- **ncc-core.nproj** (netcoreapp2.1, Exe): compiles, but its real apphost/net10 story is
  3.1 — keep it out of the 2.5 acceptance set (3 libraries only).
- Install (`NemerleAll.nproj:384-411`) and `_Integration` (:414-420) use **no**
  Community.Tasks (pure Copy / nested MSBuild); no TemplateFile/AL/GetGitTagRevision there.

## Suggested atomic split

- **2.3a** acceptance command / guarded `$(Nemerle)` default (Q3.1).
- **2.3b** `'$(MSBuildRuntimeType)'` gate on the MacroReference TFR block (Q3.2).
- **2.3c** run `dotnet build` on the 3 nprojs; triage residuals (none expected structural).
- **2.4a** delete `Nemerle.Sdk.targets:3` (+ optional dead block).
- **2.4b** *(defer to Phase 5)* NemerleAll Validate FileUpdate→native rewrite; snippets TemplateFile.
- **Regression** after 2.3b/2.4a: `Build-core-base.cmd Debug Build Stage1` (+ Stage2/3).
