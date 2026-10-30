# Report — Phase 3 reconnaissance (3.1 net10 host/apphost, 3.4 drop ncc.nproj, 3.5 Core 2.1→10 breaks)

Branch `retarget-compiler` @ `a42c896d0`, tree `C:\RSDN\nemerle` (build paths show `F:\RSDN\nemerle`). Read-only: no
builds run, only this file created. Evidence = last completed classic Stage1 artifacts (`bin|obj\Debug\net-4.0\Stage1`)
+ SDK 10.0.402 targets on disk. `ildasm.exe` and `Select-String` are blocked by sandbox permissions, so IL reference
sets come from PE `#Strings` scans (absence claims are sound: an `AssemblyRef` name must live in `#Strings`).

## Q1 — Reference set passed to ncc for ncc-core today

* `ncc-core.nproj` has **no `<Reference>` items**: `OutputType=Exe`(:8), `NoStdLib=true`(:12), `GreedyReferences=false`(:14),
  `TargetFramework=netcoreapp2.1`(:16), sources = `ncc\shared\AssemblyInfo.n` + `ncc\main.n`, `ProjectReference` →
  `Nemerle.Compiler.nproj`, `Nemerle.nproj`. Contrast `ncc.nproj:31-34` (explicit `<Reference Include="mscorlib"/>`, `System`).
* `/r:` provenance: `Nemerle.MSBuild.targets:356-357` splits `@(ReferencePath)`, `:380` passes
  `References="@(_NonMacroReferencePath)"`, `MSBuildTask.cs:257-261` emits one `\n/ref:` per item. The targets file adds
  **no** framework refs itself (no default `System.Runtime`-style list) — only `/no-stdlib` (`:374`→`MSBuildTask.cs:199-200`)
  and `/greedy-references:-` (`:203-204`).
* Assets file used by stages is per-project: `Nemerle.Sdk.props:9-11` sets
  `MSBuildProjectExtensionsPath=$(IntermediateOutputPath)$(AssemblyName)\` →
  `obj\Debug\net-4.0\Stage1\ncc-core\project.assets.json` (target `netcoreapp2.1`, `:4`).
* Framework arrives as the **implicit package** `Microsoft.NETCore.App/2.1.0` (`autoReferenced`, assets `:22`, `:1231`);
  its `compile` node is the whole `ref/netcoreapp2.1/*.dll` set — **143 assemblies** on disk in
  `%USERPROFILE%\.nuget\packages\microsoft.netcore.app\2.1.0\ref\netcoreapp2.1` (incl. `mscorlib.dll`, `System.dll`,
  `netstandard.dll`, `System.Runtime.dll`, `System.Console.dll`, `System.Xml.dll`, `WindowsBase.dll`). SDK 10 has no
  netcoreapp2.1 targeting pack, so that package is the ref source; its imported
  `build/netcoreapp2.1/Microsoft.NETCore.App.props` (via `ncc-core.nproj.nuget.g.props`) adds only conflict properties, no refs.
* Plus transitive compile assets from `Nemerle.Compiler.nproj:39-40` (`dnlib/4.5.0`, `System.CodeDom/10.0.14`) and the
  project outputs `Nemerle.dll`, `Nemerle.Compiler.dll`.
* ⇒ `/r:` ≈ **143 netcoreapp2.1 refs + Nemerle.dll + Nemerle.Compiler.dll + dnlib.dll + System.CodeDom.dll**. ncc also adds
  its own directory to `Options.LibraryPaths` (`main.n:52-53`) for plugin/macro lookup.
* Side effect: because the framework came as an ordinary package its facades are copy-local — `bin\Debug\net-4.0\Stage1\`
  holds ~100 `System.*.dll` + `netstandard.dll`, while `obj\...\ncc-core.nproj.FileListAbsolute.txt` lists 14 real outputs.

## Q2 — ManagerClass defaults vs `/no-stdlib`

`ncc\frontend\ManagerClass.n:532` `if (Options.DoNotLoadStdlib)` … `else` (`:561-570`):

```
563:           forceInitNemerleTypes                    = true;
564:           LibrariesManager.AssemblyResolver.UseGAC = true;
565:           def load(type) { LibrariesManager.AddLibrary(type.Assembly.Location, isUserReference=false); }
566:           load(typeof(object));                    // mscorlib
567:           load(typeof(System.Uri));                // System
568:           load(typeof(System.Xml.XmlNode));        // System.Xml
569:           load(typeof(Nemerle.Collections.NList)); // Nemerle
```

* Flag `-no-stdlib` → `CompilationOptions.n:537-540`; emitted from `$(NoStdLib)` (`Nemerle.MSBuild.targets:374`,
  `MSBuildTask.cs:199-200`). **Every** stage project sets `NoStdLib=true` (`ncc-core.nproj:12`, `ncc.nproj:12`,
  `Nemerle.Compiler.nproj:13`, `Nemerle.Macros.nproj`, `Nemerle.Compiler.Test.nproj:15`, `Nemerle.Test.Framework.nproj:15`);
  the legacy make path passes it literally (`ncc\frontend\Makefile:41`).
* ⇒ **The stage build never runs `:561-570`.** With `-no-stdlib` only the explicit `/r:` set is used, reordered so the
  largest of `mscorlib`/`netstandard`/`System.Runtime` is first (`:534-555`); `forceInitNemerleTypes` comes from a
  reference named `Nemerle` (`:556-557`).
* If the defaults ran on Core: `Location` = `System.Private.CoreLib` / `System.Private.Uri` / `System.Private.Xml` in the
  shared framework — dnlib loads them fine, so the failure mode is *impl assemblies instead of the ref/facade set*
  (type-forward noise), not a crash. The real Core start-up crash is `main.n:52`.
* Macro loading is path-based, host-agnostic: `LoadNemerleMacros` (`:609-616`) → `LookupAssemblyForExecution` searches
  `_lib_path` (contains the compiler dir, `main.n:53`) and `assemblyLoadFrom(path)` (`:268-278`, `:148-155`) **before** the
  strong-name `Assembly.Load(name + strongPart)` fallback (`:282`). `Nemerle.Macros.dll` is absent from
  `ncc-core.deps.json`, so the path branch is what carries it.
* **⇒ 3.5-ManagerClass gates neither 3.4 nor 4.x**; it only matters for running ncc-core with no `/r:` (not needed for 3.6's `-version`).

## Q3 — Row by row: quote → minimal edit → gate

**[A]** = host-agnostic, identical behavior on the Framework host; **[C]** = needs `#if`/runtime gate or changes host-visible behavior.
Any `.n` edit changes emitted IL, so acceptance must read *Stage1≡Stage2≡Stage3 (fixed point) + tests 468/167*, not "equal to the pre-3.5 baseline".

1. **`ncc/main.n:52` [H]** `def compilerDirectory = IO.Path.GetDirectoryName(Uri(typeof(MainClass).Assembly.CodeBase).LocalPath);`
   — `CodeBase` throws `PlatformNotSupportedException` on Core ≥3.0, inside `main_with_catching`'s try (`:50`, catch-all `:68`)
   ⇒ ICE + `exit 2` on every run. **[A]**: `def loc = typeof(MainClass).Assembly.Location;` then
   `if (string.IsNullOrEmpty(loc)) AppContext.BaseDirectory else IO.Path.GetDirectoryName(loc)`. **Precedes 3.1b/3.6.**
2. **`LibraryReferenceManager.n:123-143` [dead]** `DirectoryOfCodebase` (mono `mono/gac` hack) — no callers in `ncc\`. Delete or leave; zero risk.
3. **`LibraryReferenceManager.n:157-165`, `:199` [H, error paths]** `getAssemblyLocation` = `System.Uri(assembly.CodeBase).LocalPath`
   (two overloads) and `Uri(type.Assembly.CodeBase).LocalPath` in the IMacro-identity error; callers are diagnostics
   (`:322`, `:349`) that would themselves throw PNSE on Core. **[A]**: one helper returning `Location` (empty-safe) used in all three. **Precedes 3.4.**
4. **`LibraryReferenceManager.n:145-155, 282` [H]** `SR.Assembly.Load(name)`/`LoadFrom(path)` + `Assembly.Load(name+strongPart)`
   fallback. On Core `Assembly.Load(string)` probes ALC.Default via deps.json, so non-dependency strong names never resolve;
   `LoadFrom(path)` works. **[A-ish]**: leave 145-155; at `:282` retry each `_lib_path` with `Assembly.LoadFrom(Path.Combine(lib, name+".dll"))`
   first — strictly adds successes, same failure text. Not needed for 3.4/4.x (Q2), needed before out-of-tree `/macros:`.
5. **`ManagerClass.n:565-569` [H→effectively L]** — leave untouched in Phase 3; a real Core default set must resolve the
   `Microsoft.NETCore.App` ref dir (Roslyn-style) rather than `Assembly.Location` — **[C]**, gates nothing in 3.1/3.4.
6. **`tools/msbuild-task/MSBuildTask.cs` [H]** — `Registry`/`CodeBase` already gone (`1cb57f94b`; now `:117-136`
   `Assembly.Location` + `AppContext.BaseDirectory`). **Remaining 3.4 blocker:** `ToolName` hard-codes `"ncc.exe"`
   (`:95-105`, `#if MONO` only) and `FindExecutable` prefers `$(CompilerPath)\ncc.exe` (`:107-136`); after 3.4 nothing
   produces `ncc.exe` (Core apphost = `ncc-core.exe`). Pick one: `<AssemblyName>ncc</AssemblyName>` for ncc-core, a
   `CompilerExecutable` property, or OS/extension-aware `ToolName`. **Must precede removing `ncc.nproj`.**
7. **`ncc/frontend/codedom/*` + `Nemerle.Compiler.nproj:40` [M→no-op]** — `System.CodeDom` already 10.0.14;
   `NemerleCodeCompiler.n:43 using System.Configuration;` is unused and compiles only because netstandard2.0 exposes
   `System.Configuration.*`. Deleting it is **[A]**/cosmetic; `CodeDomProvider` stays in the netstandard2.0 assembly (D2) ⇒ no Phase-3 break.
8. **`macros/Resource.n:68` [M]** `Assembly.Load("System.Windows.Forms, Version=4.0.0.0, …")` for `ResXResourceReader`;
   on Core `FileNotFoundException` → `catch { | e => Message.Error(e.Message); }` (`:146`) ⇒ **loud compile error, not a
   silent break** (plan wording too optimistic). Only resx-macro users hit it; real fix (`System.Resources.Extensions` /
   own parser) is a separate commit, before Phase 6 parity if the testsuite uses it.
9. **`lib/hashtable.n:103` + `[Serializable]` [M→latent]** `protected this (info : SerializationInfo, context : StreamingContext) { base(info, context); }`;
   `[System.Serializable]` sites: `hashtable.n:42`, `core.n:3,18,24,35`, `HashSetEx.n:12`, `list.n:48`, `option.n:39` — and it is the
   **only** serialization ctor in `lib\`. `SYSLIB0050/0051` come from the *reference assembly*; `lib\` compiles against
   netstandard2.0 where they are absent ⇒ clean today and in Phase 3. Only when `Nemerle.nproj` multi-targets net10.0:
   delete the ctor, keep attributes (**[A]**, IL-visible, never bundled with 3.1/3.4).
10. **`ncc/backend/hierarchy/LibraryReference.n:84` [L]** `SHA1Managed()` — not obsolete in the netstandard2.0 ref set
    (`SYSLIB0020` is a .NET 5+ ref-set attribute); `SHA1.Create()` is **[A]**.
    **Not in the plan table, same file:** `:72-76 def snKey() : SR.StrongNameKeyPair` used at `:112-113`
    (`snKey().PublicKey`) — its members throw PNSE on Core ⇒ the `InternalsVisibleTo` scan breaks on a net10 host.
    Fix by reading key bytes / `dnlib.DotNet.StrongNamePublicKey` (**[A]**; gate: before Phase 6). Signing itself is
    already host-agnostic: `TypesManager.n:277` uses `DN.StrongNameKey` (`:41`, `:517`), not `System.Reflection`.
11. **`ncc/frontend/Makefile:41,293` + `macros/macros.build:49-50` [M→out of graph]** `-r System.Data -r
    System.Windows.Forms`, `-r:mscorlib -r:System -r:System.Xml` (and profiler targets `:346-358`): NAnt/make paths, not
    the MSBuild stage graph ⇒ zero effect on acceptance; clean up in Phase 5/7.
12. **New, not in the plan table: PDB writer default [H for 3.6].** `TypesManager.n:29-34` `PdbType = Windows | Portable
    | Embedded` (default = first member ⇒ Windows MSF PDBs; confirmed in `progress2.5` Step 6) → `:415-426`
    `dnlib PdbFileKind.WindowsPDB`, which needs `Microsoft.DiaSymReader.Native`. A net10 `ncc-core` has no such native and
    `ncc-core.nproj:20-21` sets `DebugSymbols=true`/`DebugType=full` ⇒ it fails as soon as it compiles anything with debug
    info; `-version` will not catch it. Handle in 3.1b (`/pdb-type:Portable`, see `CompilationOptions.n:431`, or
    `DebugType=portable`).
* Symbols (for [C] calls): `NemerleAll.nproj:36-44` builds `NProjectConstants=NET_4_0(;DEBUG)` but passes it only to
  TestFramework/Linq/Unsafe/Tools and to the runner (`-def:…`, `:378`,`:380`); Stage1..4 `MSBuild` calls pass no
  `DefineConstants`. Per-project: `Nemerle.Compiler.nproj:27/35` (`DEBUG;TRACE`/`TRACE`); `ncc-core.nproj` none. ncc honors
  `/define:`→`#if` (`MSBuildTask.cs:210-215`; used at `FixedType.n:388`, `PrettyPrint.n:139`). SDK adds TFM symbols
  implicitly (`Microsoft.NET.Sdk.BeforeCommon.targets:188-207`, `AddImplicitDefineConstants:290-298`
  `AfterTargets="PrepareForBuild"`), and `CompileDependsOn` still contains `PrepareForBuild`
  (`Microsoft.Common.CurrentVersion.targets:3655-3668`) because `Nemerle.MSBuild.targets:334` overrides only
  `CoreCompileDependsOn` ⇒ after 3.1 `main.n` could use `#if NET10_0_OR_GREATER`; today no `.n` file branches on a runtime symbol.

## Q4 — runtimeconfig today and who produces the apphost

```
bin\Debug\net-4.0\Stage1\ncc-core.runtimeconfig.json
{ "runtimeOptions": { "tfm": "netcoreapp2.1",
    "framework": { "name": "Microsoft.NETCore.App", "version": "2.1.0" } } }      // + ncc-core.runtimeconfig.dev.json (probing paths)
```
* Generator is **the SDK, not the Nemerle targets**: `Nemerle.Sdk.targets` is just `Import Sdk="Microsoft.NET.Sdk"
  Project="Sdk.targets"`; `Microsoft.NET.Sdk.targets:37` enables `GenerateRuntimeConfigurationFiles` when
  `HasRuntimeOutput` (=`_IsExecutable`, `Microsoft.NET.Sdk.Common.targets:36-37`; `OutputType=Exe`), task at `:28`, paths at
  `:87-89`. Evidence it survives the `LanguageTargets` override: both JSONs in Stage1 + `obj\...\ncc-core.genruntimeconfig.cache`.
* Apphost: `Microsoft.NET.RuntimeIdentifierInference.targets:222-227` — `UseAppHost` defaults true only for **TFM ≥ 3.0**
  ⇒ netcoreapp2.1 gets none (matches E5). **Flipping to `net10.0` enables the apphost by itself**; explicit
  `<UseAppHost>true</UseAppHost>` is documentation, `RuntimeIdentifier` not needed (framework-dependent).
* Chain that must keep working under the override: `_CreateAppHost` is in `CompileDependsOn`
  (`Microsoft.NET.Sdk.targets:798-805`) and depends on `CoreCompile` (`:842-845`) — the Nemerle override replaces only the
  `CoreCompile` target (`:336-395`); conditions `ComputeNETCoreBuildOutputFiles` (true for any `.NETCoreApp`, `:42`) and
  `AppHostSourcePath` from `_GetAppHostPaths` (`:869-885`); the copy to output is `_ComputeNETCoreBuildOutputFiles`
  (`:999-1034`) injecting a `None` item `Link=$(AssemblyName)$(_NativeExecutableExtension)` + `PreserveNewest`, and
  `AssignTargetPaths`/`CopyFilesToOutputDirectory` come from `Microsoft.Common.CurrentVersion.targets` (imported at
  `Nemerle.MSBuild.targets:38`). ⇒ **no Nemerle-side code has to copy the apphost**; it lands as
  `bin\Debug\net-4.0\Stage1\ncc-core.exe` (flat: `Nemerle.Sdk.props:5`).
* Also automatic at 3.1: `tfm`/framework re-stamped to 10.0, and `GenerateRuntimeConfigDevFile` flips to false for ≥6.0
  (`:81`) ⇒ the `.dev.json` disappears (nothing consumes it).

## Q5 — ncc-core.dll vs ncc.exe reference sets (not identical — by design)

| Stage1 artifact | mscorlib | netstandard | System.Private.CoreLib | dnlib | System.CodeDom | Core facades (System.Runtime/Console/Threading/IO.FileSystem) |
|---|---|---|---|---|---|---|
| `ncc-core.dll` (netcoreapp2.1) | **absent** | **absent** | absent | absent | absent | **present** |
| `ncc.exe` (net461) | **present** | absent | absent | absent | absent | Console/IO.FileSystem absent |
| `Nemerle.Compiler.dll` (ns2.0) | absent | **present** | absent | **present** | **present** | System.Runtime present |
| `Nemerle.dll` (ns2.0) | absent | **present** | absent | absent | absent | System.Runtime present |

`ncc-core` compiles against the netcoreapp2.1 **facade** set (no `mscorlib`/`netstandard`, and no `System.Private.CoreLib`
ref — type refs point at facades, not the impl); `ncc.exe` refs `mscorlib`. After 3.1 it stays facade-based (net10 set).
`Nemerle*.dll` keep a pure `netstandard` reference set — that is what makes D2/D6 (loadable from Framework and net10 hosts) hold.

## Recommended atomic split (acceptance = classic Stage chain fixed point + tests 468/167)

* **3.1a** — `ncc-core.nproj` TFM `netcoreapp2.1`→`net10.0`, nothing else. Extra acceptance: `runtimeconfig.json` shows
  `net10.0`; `ncc-core.exe` appears (expected); the other four stage assemblies IL-unchanged.
* **3.1b** — make it *run*: row 1 (`main.n:52`) **+** row 12 (PDB type on Core host). Extra acceptance:
  `Stage1\ncc-core.exe -version` without `--roll-forward`, and it compiles one trivial `.n` to dll+pdb.
* **3.1c** — stop copying ~100 netcoreapp2.1 facades into Stage1 (`CopyLocalLockFileAssemblies=false` / narrowed refs). Output-only.
* **3.4a** — ToolTask executable name (row 6) while `ncc.nproj` still exists; Stage1/Stage2 must stay green.
* **3.4b** — delete `<NCompilerProject Include="$(NRoot)\ncc.nproj"/>` (`NemerleAll.nproj:61`); `Validate`'s `ncc*.exe`
  glob keeps matching the apphost.
* **3.5 ordering** — 3.5a `main.n:52` **before 3.1b**; 3.5b ToolTask tool-name **before 3.4b**; 3.5c
  `LibraryReferenceManager` `CodeBase` diagnostics (`:157-165`,`:199`) + `:282` lib-path retry **before 3.6/4.3**;
  3.5d `LibraryReference.n:72-76` `StrongNameKeyPair` + `SHA1Managed` + unused `using` before Phase 6; 3.5e
  `macros/Resource.n:68` resx reader as its own commit; 3.5f `hashtable.n:103`/`[Serializable]` and Makefile/NANT FX refs
  only when a library multi-targets net10.0 or the make path dies (Phase 5/7).
* Gates nothing: `ManagerClass.n:565-569`, codedom, hashtable, SHA1, Makefile/NAnt — must **not** be bundled into 3.1/3.4.
