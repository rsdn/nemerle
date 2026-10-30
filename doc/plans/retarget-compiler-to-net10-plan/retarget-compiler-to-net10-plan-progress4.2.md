# Progress — 4.2: `boot-net10\` seed directory (framework-dependent, D4)

Status: DONE. `boot-net10\` created (untracked, NOT committed), `NRootBoot`
still `$(NRoot)\boot-dnlib` (switch is 4.3), one minimal `NemerleAll.nproj`
edit (`NPrepareBoot` `*.json` glob, see below). No repository build entry
points were run; all binaries taken from validated
`bin\Debug\net-4.0\Stage2\` (FC ×5 fixed point vs Stage3, 468+167 green).

## Manifest (`boot-net10\` ← Stage2)

| file | bytes | consumer |
|---|---:|---|
| ncc-core.exe | 161,280 | apphost; `Ncc.FindExecutable` prefers `$(CompilerPath)\ncc-core.exe` (MSBuildTask.cs, 3.4a) → the Stage1 host after 4.3 |
| ncc-core.dll | 9,216 | managed app the apphost loads (entry point) |
| ncc-core.runtimeconfig.json | 270 | host policy: tfm `net10.0`, framework `Microsoft.NETCore.App 10.0.0` → framework-dependent (D4) |
| ncc-core.deps.json | 2,365 | host TPA/dependency map for ncc-core.dll (dnlib, System.CodeDom, Nemerle, Nemerle.Compiler) |
| Nemerle.dll | 302,080 | deps.json runtime entry; also default `-r` resolved by ncc from the compiler dir (smoke b) |
| Nemerle.Compiler.dll | 1,691,136 | deps.json runtime entry; compiler core |
| Nemerle.Macros.dll | 600,576 | std macros on-demand-loaded from the compiler dir (smoke b: `[StructuralEquality]`) |
| Nemerle.MSBuild.Tasks.dll | 22,528 | `UsingTask AssemblyFile="$(Nemerle)\Nemerle.MSBuild.Tasks.dll"` (Nemerle.MSBuild.targets:3-5); overwritten in `$(NBoot)` by the freshly built one (NTasks) before each stage |
| dnlib.dll | 1,178,624 | deps.json runtime entry; emit backend |
| System.CodeDom.dll | 25,192 | deps.json runtime entry (Nemerle.Compiler dep, 4.0.3.0) |

Excluded, with reason:
- FX `ncc.exe` / `ncc.exe.config` — FX host must not appear in the net10 seed
  (boot-dnlib stays the emergency FX seed, 4.5). No `ncc-core.exe.config`
  exists in Stage2 either (SDK writes none here).
- `*.pdb`, `*.dll.il`, `*.dll.res` — boot-dnlib carries no pdbs; IL-dumps are
  Validate-diagnostic artifacts, not runtime inputs.
- `Nemerle.deps.json`, `Nemerle.Compiler.deps.json`, `Nemerle.Macros.deps.json`
  — class-library deps files; the apphost reads only `ncc-core.deps.json`.
- `netstandard.dll` + ~95 `System.*` facade dlls — boot-dnlib needs them
  because its FX ncc.exe runs on .NET Framework; the net10 shared framework
  supplies them and `ncc-core.deps.json` lists none of them.
- `Nemerle.MSBuild.targets`, `Nemerle.Sdk.props`, `Nemerle.Sdk.targets` —
  supplied to `$(NBoot)` by **NTasks** (`CopyNemerleTargetsToOutput` with
  `OutputPath=$(NBoot)`), deliberately absent from `boot-dnlib\` too; mirroring
  boot-dnlib's role they are not seed content (the checklist line's
  "targets/props" is covered by that mechanism, proven green by the whole
  Stage1-4 chain).
- `ncc` / `ncc.bat` — XBuild/mono-only tool names (MSBuildTask.cs), unused on
  the MSBuild path.

## Smoke (no repo build)

(a) version — exit 0:

```
> .\boot-net10\ncc-core.exe -version
Nemerle Compiler (ncc) version 10.0.25.0 (git)
exit=0
```

(b) compile from scratch dir, default reference resolution (no explicit `-r`,
no `-no-stdlib`), run with CWD = repo root:

```
> .\boot-net10\ncc-core.exe -nologo -target:library -out:.tmp\phase42\smoke.dll .tmp\phase42\Smoke.n   # `public class C { }`
exit=0    # smoke.dll, 2048 bytes
```

Stronger variant proving compile-time resolution of BOTH `Nemerle.dll` (typing)
and `Nemerle.Macros.dll` (macro expansion) from the compiler directory:

```
> .\boot-net10\ncc-core.exe -nologo -target:library -out:.tmp\phase42\smoke-macros.dll .tmp\phase42\SmokeMacros.n   # [StructuralEquality] class
exit=0
```

Negative control (`-no-stdlib`, same macro source) genuinely fails:
`internal type System.Void not found` → ICE, exit `-2146232797`
(0x80131623). Side-observation for 3.1e: on net10 the ICE assertion path
fail-fasts ("Process terminated. Assertion failed.") since the apphost ignores
`ncc.exe.config` — the 3.1.1 FX sidecar has no net10 counterpart; non-zero
exit is preserved, only the message channel differs.

## NPrepareBoot glob coverage

Globs before this atom: `*.exe`, `*.dll`, `*.config` → would MISS the required
`ncc-core.runtimeconfig.json` / `ncc-core.deps.json` (apphost cannot start
without them). Minimal edit applied to `NemerleAll.nproj`:

```diff
@@ -237,6 +237,12 @@
     <CreateItem Include="$(NRootBoot)\*.config">
       <Output ItemName="NBootFiles" TaskParameter="Include" />
     </CreateItem>
+    <!--ncc-core.runtimeconfig.json/ncc-core.deps.json: required by the .NET
+        apphost next to ncc-core.dll (boot-net10 seed, 4.2). Harmless for
+        boot-dnlib, which carries no *.json files.-->
+    <CreateItem Include="$(NRootBoot)\*.json">
+      <Output ItemName="NBootFiles" TaskParameter="Include" />
+    </CreateItem>
     <Copy SourceFiles="@(NBootFiles)" DestinationFolder="$(NBoot)" />
```

File integrity verified: BOM `EF BB BF` preserved, 584 CRLF / 0 bare LF,
size 36,738 → 37,077 (+339), parses as XML.

Dry simulation after the edit — globs `*.exe;*.dll;*.config;*.json` over
`boot-net10\`: total=10, covered=10, MISSED=0. For `boot-dnlib\` the new glob
matches 0 files (no `*.json` there) → current behavior byte-identical until
4.3 flips `NRootBoot`. Everything else a stage build consumes from `$(NBoot)`
(task dll, targets/props) is produced there by NTasks, and the ncc host lookup
prefers `ncc-core.exe` in `$(NPrevBin)` — covered.

## 3.1e-risk observation for this layout (flat boot dir after 4.3)

After 4.3 the flat `$(NBoot)` will contain `ncc-core.deps.json` (and
`ncc-core.runtimeconfig.json`) and NO FX `ncc.exe` at all. Candidate FX
consumers confused by a stray `*.deps.json` (3.1e-b concern): only the Ncc
ToolTask lives in an FX process — but it runs in MSBuild's own load context,
is loaded as `$(NBoot)\Nemerle.MSBuild.Tasks.dll`, and merely *spawns*
`ncc-core.exe` as a child process; `*.deps.json` is parsed exclusively by
hostpolicy when the apphost starts, never by FX Fusion. No FX consumer of the
boot dir remains after 4.3. Expectation: no confusion; keep as a gate-check
item for 4.4 anyway.

## Files touched

- NEW `boot-net10\` (10 files above; untracked, not committed)
- `NemerleAll.nproj` — single NPrepareBoot hunk (quoted above)
- `doc\plans\retarget-compiler-to-net10-plan\retarget-compiler-to-net10-plan-progress4.2.md` (this file)

Pre-existing dirty/untracked state untouched: `doc/plans/retarget-compiler-to-net10-checklist.md`
(modified before this atom), `AGENTS.md` (untracked). Scratch: `.tmp\phase42\`.
