# Progress — 3.1a retry: flip ncc-core.nproj to net10.0

Branch: retarget-compiler. Prior blocker (old boot seed ICEing on net10 ref sets)
is gone: seed `boot-dnlib` @ a6c0a4cec contains the InitSystemTypes net10-ref fix
and was proven to compile against the FULL net10 ref pack.

## Intentions

1. Single edit: `ncc-core.nproj:16` `<TargetFramework>netcoreapp2.1</TargetFramework>`
   → `<TargetFramework>net10.0</TargetFramework>`. Preserve UTF-8 BOM + CRLF;
   verify `git diff` touches ONLY that line.
2. A: Build Stage1 → expect exit 0.
   `cmd /c "`"C:\RSDN\nemerle\Build-core-base.cmd`" Debug Build Stage1"`
3. B: Inspect `bin\Debug\net-4.0\Stage1\ncc-core.runtimeconfig.json` — expect
   `tfm: net10.0`, framework version 10.x; `ncc-core.exe` apphost exists;
   old `ncc-core.runtimeconfig.dev.json` absent.
4. C (3.6 gate): run `bin\Debug\net-4.0\Stage1\ncc-core.exe -version` directly
   (no dotnet prefix, no --roll-forward) → exit 0 + version banner; also
   `dotnet ncc-core.dll -version` → exit 0.
5. D (informational for 3.1c): count `System.*.dll` in Stage1 output; check
   FileListAbsolute for stray netcoreapp2.1 package facades; state whether the
   flip changed the situation. Do NOT fix.
6. E (full parity): logs to `C:\RSDN\phase31a2-logs\`
   - `Stage3,Validate` → expect FC ×5 (`no differences encountered`), sole
     error = known-red pefverify MSB3073.
   - `Stage1,CompilerTests` → expect 468 positive + 167 negative passed,
     `All tests passed.` ×2.
7. F: `git diff --stat` = only `ncc-core.nproj` (+ this progress doc);
   no BOM damage (first line of ncc-core.nproj unchanged in diff).
8. Do NOT commit.

## Results

### A: Stage1 build

- First attempt after the bare TFM flip: **FAILED**, but compilation itself was
  fine — ncc compiled ncc-core.dll against the full net10 ref pack
  (`/ref:...\Microsoft.NETCore.App.Ref\10.0.13\ref\net10.0\*`), apphost was
  produced (`Copying ... obj\...\apphost.exe -> bin\...\Stage1\ncc-core.exe`).
  The failure was in `CopyFilesToOutputDirectory`:
  `Microsoft.Common.CurrentVersion.targets(4954,5): error : Expected file
  "obj\Debug\net-4.0\Stage1\refint\ncc-core.dll" does not exist.`
- Root cause (MSBuild evidence): `Microsoft.NET.TargetFrameworkInference.targets:281`
  (SDK 10.0.402) implicitly sets `ProduceReferenceAssembly=true` for
  .NETCoreApp >= 5.0 (netcoreapp2.1 did not hit this). The standard
  `CopyRefAssembly` task (`Microsoft.Common.CurrentVersion.targets:4955`,
  `IntermediateRefAssembly = obj\refint\$(TargetName)$(TargetExt)`, line 418)
  then expects a refint assembly that Nemerle's custom `CoreCompile`
  (Nemerle.MSBuild.targets) never emits — ncc has no `/refout`.
- Fix: explicit `<ProduceReferenceAssembly>false</ProduceReferenceAssembly>`
  added next to `<TargetFramework>net10.0</TargetFramework>` in ncc-core.nproj
  (same project, standard opt-out for custom languages; without it the flip
  cannot build). Retry Stage1: **exit 0**.

### Extra required: dnlib flavor (shared stage dir)
After the flip, `Stage1\ncc.exe` (the .NET Framework host the whole chain uses)
crashed on **any** invocation (`-version` too) with
`FileLoadException: System.Runtime, Version=6.0.0.0` (0x80131040) in
`SystemTypeClass..ctor`. Cause: ncc-core@net10 restore resolves dnlib to
`lib/net6.0/dnlib.dll` (System.CodeDom stayed `lib/netstandard2.0`) and copies it
into the shared `Stage1` dir; net461 `ncc.exe` loads it and cannot bind the
net6-only facade. Baseline (netcoreapp2.1) resolved dnlib to `lib/netstandard2.0`
(works on FX and Core). Fix: keep the PackageReference flow (so `deps.json` lists
dnlib for the Core host) and overwrite the copied file with the identical-identity
`lib/netstandard2.0` build via an `AfterTargets="Build"` copy. After: both
`ncc.exe` and `ncc-core.exe` `-version` work. Same class as 3.1c (ncc-core's
package/asset flow polluting the flat, shared stage output dir).

## Final results (clean rebuild, my change)

### A — Stage1 exit 0: PASS
`Build-core-base.cmd Debug Build Stage1` → `STAGE1_EXIT=0`
(log `C:\RSDN\phase31a2-logs\A-stage1-final.log`).

### B — runtimeconfig / apphost / dev.json: PASS
`Stage1\ncc-core.runtimeconfig.json`:
`{ "runtimeOptions": { "tfm": "net10.0", "framework": { "name":
"Microsoft.NETCore.App", "version": "10.0.0" }, ... } }`
`ncc-core.exe` present, 161280 bytes (apphost auto-generated at TFM>=3.0).
`ncc-core.runtimeconfig.dev.json` ABSENT (GenerateDevProperties off >= 6.0).

### C — direct run (3.6 gate): PASS
`Stage1\ncc-core.exe -version` (no dotnet prefix, no --roll-forward) → exit 0,
banner verbatim:
```
Nemerle Compiler (ncc) version 10.0.11.0 (git)
Copyright (c) University of Wroclaw 2003-2008, Nemerle Project Team 2008-2013
All rights reserved.
```
`dotnet Stage1\ncc-core.dll -version` → exit 0 (same banner).

### D — reference-set sanity (informational, NOT fixed): the flip DID change it
- `Stage1\System.*.dll` count = **95** (unchanged vs baseline; those facades come
  from `ncc.nproj`'s netstandard facade flow, tracked by `ncc.nproj.FileListAbsolute.txt`).
- BUT the flip changed the *flavor* of two shared files ncc-core now copies:
  - `Stage1\System.CodeDom.dll` = **181832 B** (full netstandard2.0 impl) vs
    baseline **25192 B** (type-forwarding facade). This is the codedom regression
    cause (see E).
  - `Stage1\dnlib.dll` = netstandard2.0 (via override) vs baseline net45.
- `ncc-core.nproj.FileListAbsolute.txt` lists both as ncc-core outputs. Confirms
  3.1c is the right home to stop ncc-core copying package libs into the flat dir.

### E — full parity: BLOCKED (two causes, both outside 3.1a scope)
`Stage3,Validate` (log `E-stage3-validate-final.log`), exit 1:
- Sole error: `ildasm.exe 'Stage2\ncc-core.exe' has no valid CLR header and cannot
  be disassembled` / `MSB3073` at `NemerleAll.nproj(332)`. FC ×5 **not reached**
  (Validate aborts before `fc`; `no differences encountered` count = 0).
- Cause: `Validate`'s `ncc*.exe` ildasm glob (`NemerleAll.nproj:320`) now matches
  the new **native apphost** `ncc-core.exe` (required by acceptance B). Baseline
  netcoreapp2.1 had no apphost, so the glob matched only `ncc.exe`. Fixing the glob
  is a change to `NemerleAll.nproj` — out of scope (F restricts diff to
  ncc-core.nproj; recon "3.4b" already deferred the glob to ncc.nproj removal).
- Note: the known-red baseline was "final pefverify MSB3073"; with the apphost
  present the run now dies earlier at the ildasm step instead.

`Stage1,CompilerTests` (log `E-stage1-compilertests-final.log`), exit 1:
- Positive: **467 passed, 1 failed** (expected 468). Failure = `codedom.n: run ...
  failed`, exit code -1073741819 (0xC0000005) = `NullReferenceException` in
  `System.CodeDom.Compiler.CodeGenerator.GenerateCommentStatements`.
- Negative: **167/167 passed** (standalone run `E-negative-standalone.log`, exit 0,
  "All tests passed."). In-target it is skipped only because the positive Exec runs
  first (`NemerleAll.nproj:383` before `:385`) and aborts on codedom.
- codedom attribution (proven, NOT dnlib):
  - Reproduced 467/1 clean; baseline (netcoreapp2.1) 468 clean.
  - Swapping dnlib net45<->netstandard2.0 in a *baseline* test dir: recompile + run
    PASS with both → dnlib is not the cause.
  - Infra-DLL hash diff of the test dir: only meaningful diff is
    `System.CodeDom.dll` — baseline **25192 B** (facade → framework CodeGenerator,
    works) vs mine **181832 B** (full netstandard impl whose CodeGenerator NREs on
    the empty-comments path). ncc-core@net10 copies the full impl into the shared
    stage/install/test dirs, so `codedom.n` binds the buggy CodeGenerator.
  - This is 3.1c territory (stop ncc-core copying package libs); task said do NOT fix.

### F — diff scope: PASS
`git diff --stat` = `ncc-core.nproj | 16 +++++++++++++++-` (15+/1-) only. BOM intact
(`EF BB BF`), all CRLF (0 LF-only), first line not in any hunk. (Progress doc is an
untracked new file, so not in `git diff --stat`.)

## Verdict
A, B, C, F PASS. The net10 host builds and runs directly on .NET 10 (no roll-forward).
E cannot pass within 3.1a scope: (1) the apphost required by B breaks Validate's
ildasm glob (NemerleAll.nproj, out of scope; deferred to 3.4b); (2) ncc-core copying
the full System.CodeDom.dll into the shared dir regresses codedom.n (3.1c, explicitly
"do NOT fix"). Both are direct consequences of the flip that the plan assigns to
later atoms. NOT committed (per instructions).
