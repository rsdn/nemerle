# Progress 3.1c-flatdir — System.CodeDom flat-dir overwrite in ncc-core.nproj

Branch `retarget-compiler`, working dir `C:\RSDN\nemerle`.
Decision source: `retarget-compiler-to-net10-plan-report3.1c-3.1d.md`, Q1 recommended
option (a) (AfterTargets overwrite, mirror of the existing dnlib hack) + Q2 uniformity
note (one target, two `Copy` tasks). Do NOT commit.

## Intentions

1. Touch only `ncc-core.nproj` (preserve UTF-8 BOM EF BB BF and CRLF; `git diff`
   must show only intended lines).
2. Add a direct `<PackageReference Include="System.CodeDom" Version="4.7.0" GeneratePathProperty="true" />`
   next to the dnlib PackageReference. Exactly as the report prescribes (Q1(a), Atom
   spec): no `PrivateAssets`/`ExcludeAssets` (Q1(d) rejects those — they would break
   the net461 ncc.exe side). Version 4.7.0 matches the pin already in
   `Nemerle.Compiler.nproj:40`, so the restore graph is unchanged; the declaration
   exists solely to generate `$(PkgSystem_CodeDom)` (Pkg* properties are generated
   only for *declared* references with `GeneratePathProperty`).
3. Extend the existing `AfterTargets="Build"` target (`OverrideDnlibAssetWithNetStandard`,
   ncc-core.nproj:50-54) with a second `Copy` overwriting `$(OutDir)System.CodeDom.dll`
   with `$(PkgSystem_CodeDom)\lib\net461\System.CodeDom.dll` (25,192 B type-forwarding
   facade, identity 4.0.3.0/cc7b13ffcd2ddd51 — identical to the 181,832 B
   netstandard2.0 build it overwrites, verified in the report via
   `AssemblyName.GetAssemblyName`). ncc-core is the last project of the stage and
   AfterTargets runs after its own `CopyFilesToOutputDirectory` → final writer in the
   shared flat stage dir (same mechanism already proven for dnlib).
   Keep the existing target name (rename is optional per the atom spec; smaller diff).
   Add a comment stating the same flat-dir rationale as the dnlib block.
4. Do NOT touch `NemerleAll.nproj` (Validate glob = atom 3.1d) or any other file.
   Leave the pre-existing uncommitted changes (checklist doc, other progress docs) as is.

## Planned acceptance steps

- A. `cmd /c "`"C:\RSDN\nemerle\Build-core-base.cmd`" Debug Build Stage1"` → exit 0.
- B. `Stage1\System.CodeDom.dll` = 25,192 B (== `%USERPROFILE%\.nuget\packages\system.codedom\4.7.0\lib\net461\System.CodeDom.dll`)
  and `Stage1\dnlib.dll` = 1,178,624 B (dnlib 4.5.0 netstandard2.0 asset).
- C. codedom.n via the existing harness in `bin\Debug\net-4.0\Tests\positive`; if the
  dir is stale (it currently holds the old 181,832 B System.CodeDom.dll copied by the
  previous CompilerTests run), fall back to one
  `cmd /c "`"C:\RSDN\nemerle\Build-core-base.cmd`" Debug Build "Stage1,CompilerTests""`
  and quote the codedom.n line + totals 468/167.
- D. `ncc-core.exe -version` from Stage1 exits 0 (banner quoted).
- E. `git diff --stat` vs HEAD: only `ncc-core.nproj` (+ this progress doc) beyond the
  pre-existing uncommitted state; BOM/first line untouched.

## Results

Done 2026-10-31. Edits applied to `ncc-core.nproj` only (BOM `EF BB BF` and CRLF
verified intact after edits; `git diff` shows only the intended lines).

1. Added `<PackageReference Include="System.CodeDom" Version="4.7.0" GeneratePathProperty="true" />`
   next to the dnlib one, with a comment (no PrivateAssets/ExcludeAssets, per report
   Q1(a)/(d)). `(Get-Item ...nuget.g.props)` confirms both `PkgSystem_CodeDom` and
   `Pkgdnlib` are generated in `obj\Debug\net-4.0\Stage1\ncc-core\ncc-core.nproj.nuget.g.props`.
2. Extended `OverrideDnlibAssetWithNetStandard` (name kept) with the second `Copy`
   (`$(PkgSystem_CodeDom)\lib\net461\System.CodeDom.dll` → `$(OutDir)System.CodeDom.dll`,
   `SkipUnchangedFiles="true"`).

Incident (transient, resolved): the first `Build Stage1` after the edit failed at the
new Copy with `MSB3030: Could not copy "\lib\net461\System.CodeDom.dll"` — the Build-pass
project evaluation saw the pre-restore (stale) `nuget.g.props` that only had `Pkgdnlib`.
Re-running the same command succeeded (restore output already fresh). Note: the failed
first run is NOT a project-file defect; second run's log shows correct final-writer order.

Acceptance evidence:

- A. `cmd /c "...Build-core-base.cmd" Debug Build Stage1` → `Build succeeded. 0 Warning(s) 0 Error(s)`,
  exit 0. Log shows ncc-core CopyLocal writing `lib/netstandard2.0\System.CodeDom.dll` then
  `OverrideDnlibAssetWithNetStandard` overwriting with `lib\net461\System.CodeDom.dll` (last writer).
- B. `Stage1\System.CodeDom.dll` = **25,192 B** == `%USERPROFILE%\.nuget\packages\system.codedom\4.7.0\lib\net461\System.CodeDom.dll`
  (25,192 B); `Stage1\dnlib.dll` = **1,178,624 B** == dnlib 4.5.0 netstandard2.0 asset (1,178,624 B).
- C. Existing-harness probe first failed as expected (harness dir stale: 181,832 B broken
  facade, `codedom.n: run...failed`, exit -1073741819) → ran
  `Build "Stage1,CompilerTests"` once, exit 0. Log: `testsuite\positive\codedom.n: run...passed`;
  counts: positive 468 passed / 0 failed, negative 167 passed / 0 failed;
  `All tests passed.` ×2. `Tests\positive\System.CodeDom.dll` now 25,192 B.
- D. `Stage1\ncc-core.exe -version` → exit 0, banner
  `Nemerle Compiler (ncc) version 10.0.11.0 (git)`.
- E. `git diff --stat` vs HEAD: `ncc-core.nproj` (+31/−1, containing the pre-existing 3.1a-retry
  edits plus mine) and the pre-existing `doc/plans/retarget-compiler-to-net10-checklist.md`;
  no other tracked file touched. Untracked additions: this progress doc only (others
  pre-existing). First line/BOM untouched.

Not committed, per instructions. Validate-glob left for atom 3.1d.
