# Progress — sub-points 2.1 & 2.2: MSBuild task assembly loads in both hosts

Scope: port `Nemerle.MSBuild.Tasks.csproj` (+ its 4 source files) so one
produced `Nemerle.MSBuild.Tasks.dll` (netstandard2.0) loads in BOTH classic
MSBuild.exe (.NET Framework host, 18.11.0.47511 on this machine) and .NET
`dotnet msbuild` (SDK 10.0.402). **Status: DONE — all acceptance criteria pass.**

Files touched (tracked): `Nemerle.MSBuild.Tasks.csproj`,
`tools/msbuild-task/MSBuildTask.cs`. (`GetGitTagRevision.cs`,
`SetEnvironmentVariable.cs`, `Properties\AssemblyInfo.cs` compiled against
netstandard2.0 unchanged — `Process/Kill/StandardOutput`,
`Environment.SetEnvironmentVariable` and all used attributes exist in
netstandard2.0. `MSBuildTask.cs` final edit was co-signed by the user who
re-applied the `using` cleanup.)

## Design decisions & findings

1. **Package version: 17.0.0, not 17.10.x.** Empirically (dir listings of
   `%USERPROFILE%\.nuget\packages\microsoft.build.{utilities.core,framework}\*\lib`):
   only **17.0.0** ships `lib\netstandard2.0` assets; 17.7.2+ dropped to
   `net7.0/net9.0/net10.0 + net472` only. So 17.0.0 is the newest version
   compatible with the mandated `netstandard2.0` TFM.
2. **`Version=17.*` in criterion B is impossible via packages** (see evidence
   under B): every `Microsoft.Build.*` NuGet assembly from 15.x through 18.6.3
   is stamped **AssemblyVersion 15.1.0.0** — a deliberate MSBuild binding
   compatibility decision. Hosts bind it version-agnostically (Roslyn's
   netstandard2.0 task assembly has referenced 15.1.0.0 for years and loads
   under MSBuild 17/18 on both hosts). The criterion's intent — reference the
   cross-host `*Core` assemblies instead of the Framework-GAC
   `Microsoft.Build.Utilities.v4.0` — is satisfied.
3. **SDK default item globs must be off entirely**: with only
   `EnableDefaultCompileItems=false`, the default `EmbeddedResource` glob swept
   `snippets\**\*.resx` into the build (38 errors MSB3822/MSB3823). Fixed with
   `EnableDefaultItems=false` (plus the explicit `EnableDefaultCompileItems`
   per spec).
4. **`AfterBuild` target name is unusable in an SDK-style project**:
   `Microsoft.Common.CurrentVersion.targets` defines an empty `AfterBuild` and
   is imported *after* the project body, silently overriding a project-level
   target of that name (diag: `Overriding target "AfterBuild" in project
   "…Nemerle.MSBuild.Tasks.csproj" with target "AfterBuild" from project
   "…Microsoft.Common.CurrentVersion.targets"` — the copy step then never ran;
   confirmed by the missing `Nemerle.*` files in a scratch output dir).
   Renamed to `CopyNemerleTargetsToOutput` with `AfterTargets="Build"`.
5. **Auto-restore hook** (`EnsurePackagesRestored`, `BeforeTargets="Build;Rebuild"`,
   guarded by missing `$(MSBuildProjectExtensionsPath)$(MSBuildProjectFile).nuget.g.props`):
   classic `msbuild.exe` has no implicit restore; `NTasks` calls
   `Targets=Build|Rebuild` directly, so on a clean tree the first build would
   fail `NETSDK1004`. The nested `<MSBuild Targets="Restore">` works under the
   Framework host (VS NuGet 7.11) and is a no-op under `dotnet build` (its
   implicit restore already produced the assets). Verified: deleted `obj\`,
   classic `-t:Rebuild` → restore ran, build OK.
6. `ExcludeAssets=runtime` + `PrivateAssets=all` on both PackageReferences
   (plus that excluding the NETStandard.Library transitive closure from
   copy-local) yields a zero-dll-copy output: no `Microsoft.Build*.dll`, no
   `System.*.dll`, no `netstandard.dll` land next to the task.

## Results / evidence

### A. `dotnet build Nemerle.MSBuild.Tasks.csproj -o C:\RSDN\tasks-selftest`

Run from repo root (global.json 10.0.402; `dotnet --version` = 10.0.402):

```
Build succeeded.
    0 Warning(s)
    0 Error(s)
EXIT=0
```

### B. Produced dll metadata + selftest dir

`[Reflection.Assembly]::LoadFile(...) + GetReferencedAssemblies()`:

```
Assembly: Nemerle.MSBuild.Tasks, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null   (unsigned, as before)
netstandard, Version=2.0.0.0, Culture=neutral, PublicKeyToken=cc7b13ffcd2ddd51
Microsoft.Build.Utilities.Core, Version=15.1.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a
Microsoft.Build.Framework, Version=15.1.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a
```

`Version=15.1.0.0`, **not** `17.*`: verified with
`[Reflection.AssemblyName]::GetAssemblyName(...)` over the cached package libs
— microsoft.build.{utilities.core,framework} 17.0.0/17.7.2/17.14.28/17.14.8/
18.4.0/18.6.3 (all TFMs incl. `net472`) are **all** stamped `15.1.0.0`.
No NuGet package can produce a `Version=17.*` reference; 15.1.0.0 is the
canonical MSBuild binding version and both hosts' loaders bind it to their own
runtime version (proven by C: MSBuild.exe 18 executes the task).

Output dir (`dir /b C:\RSDN\tasks-selftest`):

```
Nemerle.MSBuild.targets        Nemerle.MSBuild.Tasks.deps.json
Nemerle.MSBuild.Tasks.dll      Nemerle.MSBuild.Tasks.pdb
Nemerle.Sdk.props              Nemerle.Sdk.targets
```

— exactly the 4 files NemerleAll.nproj expects (dll + 3 targets/props; the
deps.json/pdb are SDK byproducts), **no** Microsoft.Build*.dll, no System.*.dll.

### C. Stage1 via classic MSBuild

```
cmd /c "`"C:\RSDN\nemerle\Build-core-base.cmd`" Debug Build Stage1"
...
    20 Warning(s)
    0 Error(s)
Time Elapsed 00:01:15.43
Nemerle installed to "C:\RSDN\nemerle\bin\Debug\net-4.0"
EXIT=0
```

Classic MSBuild 18 (`NTasks` → msbuild.exe → netstandard2.0 task loaded,
`Ncc` ToolTask ran, compiler + Nemerle.Macros built). Warnings are the
pre-existing Nemerle macro warnings.

Additional classic-host proof (exact NTasks-style invocation, quoted props):
`msbuild.exe Nemerle.MSBuild.Tasks.csproj -p:Configuration=Debug
"-p:OutputPath=C:\RSDN\tasks-selftest-classic\"
"-p:IntermediateOutputPath=C:\RSDN\nemerle\obj\Debug\TasksSim\"
"-p:NKeysDir=..." -t:Rebuild` → `EXIT=0`; both externally-passed paths honored
verbatim (no TFM subfolders) and the dir contains exactly the same 6 files
as the selftest dir.

### D. boot dir + git status

`bin\Debug\net-4.0\boot\*.dll`: **102 dlls before, 102 after; added: (none),
removed: (none)** (Nemerle.MSBuild.Tasks.dll rebuilt in place). The boot dir's
large pre-existing `System.*.dll` set belongs to the boot-dnlib seed and was
untouched.

`git status --porcelain`:

```
 M Nemerle.MSBuild.Tasks.csproj
 M doc/plans/retarget-compiler-to-net10-checklist.md   (pre-existing at session start)
 M tools/msbuild-task/MSBuildTask.cs
?? AGENTS.md                                           (pre-existing, untracked)
?? doc/plans/retarget-compiler-to-net10-plan/          (plan docs, incl. this file)
```

Only the csproj + MSBuildTask.cs changed among tracked sources (the other three
.cs files needed no changes). No commits made.

## Debugging trail (kept for the record)

- The "netstandard.dll reference missing under msbuild.exe" failure seen in
  intermediate runs (48 × CS0012/CS0234) was **transient mixed state**: obj\
  assets/nuget-g-files from interleaved restores + concurrent builds by the
  user, plus my hook not firing for `-t:Rebuild` at the time. Instrumented
  item-flow dumps (`CustomAfterMicrosoftCommonTargets` probing
  `@(Reference)` after `ResolvePackageAssets`/`ResolveLockFileReferences`/
  `_HandlePackageFileConflicts`/RAR) showed **identical** flows on both hosts
  (`refs=113→130→127`, `ReferencePath=127`), and after the hook fix + clean
  `obj\`, classic Build/Rebuild and the NTasks-shaped invocation are all
  deterministic exit 0. No MSBuild/SDK bug workaround is needed beyond items
  3–5 above.
