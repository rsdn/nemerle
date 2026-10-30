# Report 3.1c-flatdir + 3.1d-validate-glob — decisions with evidence

Branch `retarget-compiler`, working tree = 3.1a-retry state (`ncc-core.nproj` uncommitted: net10.0 +
`ProduceReferenceAssembly=false` + dnlib AfterTargets overwrite). All paths relative to `C:\RSDN\nemerle`.

## Established facts (measured, not assumed)

- Shared flat stage dir: both hosts write package assets into one `bin\Debug\net-4.0\Stage{N}`:
  `obj\Debug\net-4.0\Stage1\ncc.nproj.FileListAbsolute.txt:4-5` (dnlib.dll, System.CodeDom.dll) and
  `obj\Debug\net-4.0\Stage1\ncc-core.nproj.FileListAbsolute.txt:6-7` (same two names).
  `Nemerle.Compiler.nproj.FileListAbsolute.txt` (netstandard2.0 lib) copies **neither** — writers are only ncc.nproj and ncc-core.nproj.
- Asset selection (`project.assets.json` under `obj\Debug\net-4.0\Stage1\`):
  ncc@net461 → `dnlib/lib/net45` (`ncc\project.assets.json:8,30`), `System.CodeDom/lib/net461` (`:30`);
  ncc-core@net10 → `dnlib/lib/net6.0` (`ncc-core\project.assets.json:8`), `System.CodeDom/lib/netstandard2.0` (`:26`).
- Package assets on disk (`%USERPROFILE%\.nuget\packages`): System.CodeDom net461 = **25,192 B** (type-forwarding facade),
  netstandard2.0 = **181,832 B** (full impl, the CodeGenerator-NRE build); dnlib ns2.0 = 1,178,624 B, net6.0 = 1,182,720 B.
  **Assembly identities are identical per package**: System.CodeDom `4.0.3.0, cc7b13ffcd2ddd51` in both libs; dnlib `4.5.0.0` in both
  (verified via `AssemblyName.GetAssemblyName`). This is what makes same-identity overwrites bind on both hosts.
- Current Stage1 dir: `System.CodeDom.dll` = 181,832 B (broken, ncc-core wins), `dnlib.dll` = 1,178,624 B (ns2.0, overwrite works).
- Build order: `NemerleAll.nproj:58-62` NCompilerProject = Nemerle → Nemerle.Compiler → Nemerle.Macros → ncc → **ncc-core**;
  stage targets invoke `MSBuild Projects="@(NCompilerProject)"` sequentially without `BuildInParallel` (`NemerleAll.nproj:257,264,276,287,298`),
  and after it only copy `NTasksFiles` = `Nemerle.MSBuild.Tasks.dll` + `.targets`/`.props` (`:203-217,265`). Nothing touches dnlib/CodeDom after ncc-core.
- CodeDom consumers: `ncc\frontend\codedom\*` compiles into **Nemerle.Compiler.dll** (`Nemerle.Compiler.nproj:56`), not ncc.
  `ncc\main.n` has zero CodeDom references (grep). ncc-core.nproj compiles only `main.n` + `AssemblyInfo.n` (`ncc-core.nproj:36-41`).
- Propagation into test dirs: `Nemerle.Compiler.Test.nproj:57-59` references `Nemerle.Compiler.dll` by HintPath into the stage dir;
  RAR CopyLocal carries the sibling `System.CodeDom.dll`/`dnlib.dll` from that dir into `NBin\TestFramework` → `Tests\positive`.
  Fixing the stage-dir flavor fixes the whole chain.
- E-baseline (pre-flip): IL-diff 5/5 identical; sole known-red = pefverify (FX 4.6.1 Tools vs netstandard2.0 Nemerle.Compiler.dll),
  checklist notes 0.4/1.5 (`doc\plans\retarget-compiler-to-net10-checklist.md:13,21,85`). FC ×5 items =
  Nemerle.dll, Nemerle.Compiler.dll, Nemerle.Macros.dll, Nemerle.MSBuild.Tasks.dll, ncc.exe (verified by `dir` of Stage2).
- Stale artifacts right now: `bin\Debug\net-4.0\Stage2\` contains leftover `ncc-core.exe.il` + 4 other `.il` from the aborted Validate
  (target died at `NemerleAll.nproj:332`, the trailing `Delete` at `:351-352` never ran). They must be cleaned before re-running,
  else `fc` (line 344) compares `Stage2\ncc-core.exe.il` against nothing → spurious red.

## Q1 (3.1c-flatdir, System.CodeDom) — mechanisms

(a) **AfterTargets overwrite (mirror of the dnlib hack)** — source: `$(PkgSystem_CodeDom)\lib\net461\System.CodeDom.dll`
    (identical file ncc.nproj itself copies; identity 4.0.3.0). ncc-core.nproj has no direct System.CodeDom PackageReference
    (it is transitive, `ncc-core\project.assets.json:18`), and `Pkg*` path properties are generated only for *declared*
    references with `GeneratePathProperty` (feature verified present in `NuGet.ProjectModel.dll`, SDK 10.0.402; already used as
    `$(Pkgdnlib)` at `ncc-core.nproj:48,51`). So: declare `<PackageReference Include="System.CodeDom" Version="4.7.0" GeneratePathProperty="true"/>`
    (same 4.7.0 already in the graph → restore graph unchanged) and add a second `Copy` to the existing AfterTargets target.
    ncc-core is the last project of the stage (`NemerleAll.nproj:61-62`) and its AfterTargets runs after its own
    `CopyFilesToOutputDirectory`, so it is the final writer — empirically proven by dnlib (Stage1 dnlib.dll = ns2.0 today). **Works.**

(b) `CopyLocalLockFileAssemblies=false` on ncc-core.nproj — property exists (`Sdks\Microsoft.NET.Sdk\targets\Microsoft.NET.Sdk.targets`, SDK 10.0.402).
    But it stops *all* lock-file copies: both dnlib.dll and System.CodeDom.dll disappear from the app dir. `ncc-core.deps.json:19-34`
    still lists both, and a framework-dependent host probes only the app dir (no probing paths; `runtimeconfig.dev.json` is absent at TFM≥6,
    progress3.1a-retry.md §B). ncc-core.exe needs dnlib on every invocation (Nemerle.Compiler's emit path loads it; the symmetric
    crash with ncc.exe is documented in progress3.1a-retry.md §"Extra required") → 3.6-gate C breaks. Reject.

(c) **Does ncc-core@net10 need the full CodeDom asset at runtime? No.** Microsoft.NETCore.App does not ship System.CodeDom, and the
    reference is only *used* if a CodeDom type executes; all CodeDom-touching code lives in Nemerle.Compiler.dll's `ncc\frontend\codedom`
    providers (`Nemerle.Compiler.nproj:56`), reachable only by a host invoking Nemerle-as-a-CodeDomProvider — never by `ncc-core.exe -version`
    or a normal compile (`main.n` has no CodeDom refs). Assembly resolution is lazy per-assembly. Swapping in the 25 KB facade keeps
    file name and identity (4.0.3.0), so deps.json probing/binding is byte-for-byte the same situation as today's dnlib overwrite; a
    TypeResolutionException could only occur on CodeDom code paths, which ncc-core does not execute. The day ncc-core must run
    codedom-class code (Phase 4) is the day it needs the *full* asset — impossible in the shared flat dir; see Q2 exit note.

(d) `ExcludeAssets="runtime"` / `PrivateAssets="all"` on the PackageReference. Precise semantics: these attributes are honored only on the
    **declaration** (`Nemerle.Compiler.nproj:40`) — there is no consumer-side per-transitive-package filter through a ProjectReference.
    `ExcludeAssets="runtime"`: compile asset stays for Nemerle.Compiler itself, but the runtime asset is removed from the runtime closure of
    *every* consumer — ncc.nproj stops copying the 25 KB facade too; .NET Framework has no in-box/GAC assembly named
    `System.CodeDom, 4.0.3.0` (that's why the package exists), so codedom.n would fail with FileNotFoundException instead of NRE.
    `PrivateAssets="all"`: package does not flow to consumers at all — same net461 breakage. Both fix ncc-core by breaking ncc. Reject.

(e) SDK-level knobs: verified by `findstr /s` over `C:\Program Files\dotnet\sdk\10.0.402\**` — **`TargetingPackOnly` and `SkipCopyLocal`
    do not exist** (zero matches; not inventing them). `CopyLocalLockFileAssemblies` exists but is all-or-nothing (case b). No per-assembly
    knob exists → project-file mechanism (a) is the SDK-sanctioned one.

**Recommendation: (a)**, extending the existing AfterTargets target so one mechanism normalizes both shared assemblies.

## Q2 (dnlib overwrite robustness + uniformity + exit)

Robust today: within a stage target, ncc-core.nproj is the last project (order above; no BuildInParallel) and the post-MSBuild copies
(`NTasksFiles`) carry no dnlib/CodeDom (`NemerleAll.nproj:203-217`). Within ncc-core's own Build, referenced projects rebuild first
(global OutputPath forces it) and write ns2.0/net461 flavors, then ncc-core's CopyLocal overwrites with net6.0/181 KB, then
`OverrideDnlibAssetWithNetStandard` (`ncc-core.nproj:50-54`) runs last — final state deterministic. Empirical: Stage1 dnlib.dll = ns2.0.
Uniformity: the same target can copy both files (one target, two `Copy` tasks, same SkipUnchangedFiles pattern); the 3.1c fix should fold
CodeDom into it rather than adding a second target.
Exit note (Phase 4): after 3.4b drops ncc.nproj and 4.3 reseeds with boot-net10, the two-flavor conflict disappears; the overrides should
then be deleted and the Core host relocated to its own subfolder (e.g. `Stage\N\net10\`), where it can carry the *native* flavors
(net6.0 dnlib, 181 KB full System.CodeDom) that a Core-run codedom path will actually need — the flat dir is a coexistence crutch, not the end state.

## Q3 (3.1d-validate-glob) — exact change in `NemerleAll.nproj` Validate (lines 317-353)

Current: ItemGroup `:318-323` (`ncc*.exe` globs `:320,322`), ildasm Execs `:332-333` over Asm2/Asm3, `fc *.il` `:344`,
pefverify `:350` over `%(Asm3.FullPath)`, cleanup `:351-352`. Failure evidence: `C:\RSDN\phase31a2-logs\E-stage3-validate-final.log:2601-2602`.

Replace the ItemGroup (318-323) with:

```xml
    <Delete Files="$(NPrevBin)\*.il;$(NCurBin)\*.il" />
    <ItemGroup>
      <!-- Managed assemblies only: exclude native Core apphosts (ncc-*.exe) from ildasm+peverify. -->
      <Asm2 Include="$(NPrevBin)\Nemerle*.dll" />
      <Asm2 Include="$(NPrevBin)\ncc*.exe" Exclude="$(NPrevBin)\ncc-*.exe" />
      <Asm3 Include="$(NCurBin)\Nemerle*.dll" />
      <Asm3 Include="$(NCurBin)\ncc*.exe" Exclude="$(NCurBin)\ncc-*.exe" />
      <!-- IL comparison additionally covers the managed Core payload: -->
      <Il2 Include="@(Asm2)" />
      <Il2 Include="$(NPrevBin)\ncc-core.dll" />
      <Il3 Include="@(Asm3)" />
      <Il3 Include="$(NCurBin)\ncc-core.dll" />
    </ItemGroup>
```

and switch the two ildasm Execs (332-333) to iterate `@(Il2)` / `@(Il3)`. Everything else unchanged: `fc :344` globs `*.il`, so
`ncc-core.il` joins the comparison automatically (ildasm emits MVID/timestamps only as `//` comments → normalized by FileUpdate
`:338-343`, same as ncc.exe today); pefverify `:350` keeps iterating Asm3 → set is exactly the pre-flip one (4 Nemerle*.dll + ncc.exe):
apphost excluded (i ✓), ncc-core.dll ildasm'd+fc'd (ii ✓), ncc.exe path untouched (iii ✓), pefverify known-red on Nemerle.Compiler.dll
preserved and no new pefverify item added (iv ✓ — a 4.6.1 pefverify on a net10 assembly is untested here; keep it out of scope until 5.1).
The leading `Delete` debrands the stale-`.il` class of false reds (Stage2 currently holds 5, incl. `ncc-core.exe.il`).
`Exclude` must be the absolute pattern shown (relative Excludes resolve against the project dir and would not match).
Contingency: if ildasm 4.6.1 refuses ncc-core.dll (untestable offline — ildasm blocked in recon sandbox), drop the two Il `ncc-core.dll`
lines with a comment; requirements (i)(iii)(iv) stand regardless.

## Q4 — E-gate re-derivation after both fixes

Pre-clean once: `del /q bin\Debug\net-4.0\Stage1\*.il bin\Debug\net-4.0\Stage2\*.il` (Stage3 has none).
1. `cmd /c ""C:\RSDN\nemerle\Build-core-base.cmd" Debug Build "Stage3,Validate""` (quotes mandatory — cmd eats commas, checklist note 2026-10-30).
   Expect exit 1 with **exactly one** MSB3073 = pefverify on `Nemerle.Compiler.dll`; zero "no valid CLR header";
   **"no differences encountered" × 6** (baseline 5 + `ncc-core.il`; FC ×5 is now stale by construction since Q3 adds the item).
2. `cmd /c ""C:\RSDN\nemerle\Build-core-base.cmd" Debug Build "Stage1,CompilerTests""` → exit 0, positive **468 passed / 0 failed**,
   negative **167 passed**, `All tests passed.` ×2 (E-stage1-compilertests-final.log regression gone with 25 KB facade).
3. Stage-dir flavor proof: `System.CodeDom.dll` = 25,192 B, `dnlib.dll` = 1,178,624 B; `ncc.exe -version` = 0,
   `ncc-core.exe -version` = 0, plus `ncc-core.exe /whatever <trivial.n>` compile exit 0 (proves the 25 KB swap did not break Core startup/compile).
4. Proof ncc-core.dll was not silently dropped from IL comparison: the Validate log must show per stage dir exactly 6 ildasm invocations
   including one for `ncc-core.dll`, zero for `ncc-core.exe`, and the `fc` section must list `ncc-core.il : no differences encountered`
   (6th pair); after the run, no `*.il` remain (cleanup `:351-352` now reachable).

## Atom specs (paste-ready)

**Atom 3.1c-flatdir** — file `ncc-core.nproj` only.
Edits: (1) add `<PackageReference Include="System.CodeDom" Version="4.7.0" GeneratePathProperty="true" />` next to the dnlib
PackageReference (line 48); (2) in the existing `OverrideDnlibAssetWithNetStandard` target (50-54, rename optional) add
`<Copy SourceFiles="$(PkgSystem_CodeDom)\lib\net461\System.CodeDom.dll" DestinationFiles="$(OutDir)System.CodeDom.dll" SkipUnchangedFiles="true" />`
plus a comment (same flat-dir rationale as the dnlib block, lines 44-47). Preserve BOM/CRLF.
Acceptance: rebuild Stage1 → `Stage1\System.CodeDom.dll` = 25,192 B and `dnlib.dll` = 1,178,624 B; `ncc.exe -version` and
`ncc-core.exe -version` exit 0; `Build "Stage1,CompilerTests"` exits 0 with 468+167 passed, `All tests passed.` ×2; `git diff` touches only ncc-core.nproj.

**Atom 3.1d-validate-glob** — file `NemerleAll.nproj` only, Validate target 317-353.
Edits: insert `<Delete Files="$(NPrevBin)\*.il;$(NCurBin)\*.il" />` at target top; apply the ItemGroup replacement and the ildasm
`%(Asm2/3.FullPath)` → `%(Il2/3.FullPath)` switch from Q3 verbatim; leave `fc` (344), pefverify (350) and cleanup (351-352) untouched.
Acceptance: pre-clean stale `*.il`; `Build "Stage3,Validate"` exits 1 with sole error pefverify-MSB3073 on Nemerle.Compiler.dll, no
"CLR header" error, `no differences encountered` × 6 incl. `ncc-core.il`; log shows no ildasm call for `ncc-core.exe`; if ildasm rejects
ncc-core.dll, remove its two Il lines (document) — Validate must still reach `fc` and end on the single known-red.
