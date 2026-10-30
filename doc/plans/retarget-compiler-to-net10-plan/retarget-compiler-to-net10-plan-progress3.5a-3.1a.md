# Progress — 3.5a (main.n CodeBase fix) + 3.1a (ncc-core TFM netcoreapp2.1→net10.0)

Branch `retarget-compiler`, tree `C:\RSDN\nemerle`. NOT committed (per task).
Date: 30.10.2026. Builds run one at a time, nothing else running.

## Scope

- **3.5a** (report Q3 row 1, [A] edit): `ncc/main.n:52` used
  `IO.Path.GetDirectoryName(Uri(typeof(MainClass).Assembly.CodeBase).LocalPath)` —
  `Assembly.CodeBase` throws `PlatformNotSupportedException` on Core ≥3.0, caught by the
  catch-all in `main_with_catching` → ICE + exit 2 on every run. Replaced with
  `Assembly.Location` + empty-check fallback to `AppContext.BaseDirectory`.
  Framework behavior identical (Location is non-empty on Framework).
- **3.1a**: `ncc-core.nproj:16` `<TargetFramework>netcoreapp2.1</TargetFramework>` →
  `net10.0`, nothing else in the project.

## Headline result

**3.5a is green and verified on a .NET 10 host. 3.1a as scoped (TFM flip alone) is
NOT buildable** — the TFM flip feeds the ncc-core compile a net10 reference set that
the *boot* compiler cannot reflect: it ICEs in `InitSystemTypes` before compiling any
source. Root cause fully isolated with a minimal deterministic repro (see Blocker).
Both edits are left in the tree (uncommitted); 3.1a needs a companion compiler fix
that is out of this sub-atom's scope.

## Edits (final tree state, `git diff` quoted)

1. `ncc/main.n`:
   ```nemerle
   -        def compilerDirectory = IO.Path.GetDirectoryName(Uri(typeof(MainClass).Assembly.CodeBase).LocalPath);
   +        // Assembly.CodeBase throws PlatformNotSupportedException on .NET Core, use Location with a fallback to AppContext.BaseDirectory
   +        def loc = typeof(MainClass).Assembly.Location;
   +        def compilerDirectory =
   +          if (string.IsNullOrEmpty(loc))
   +            AppContext.BaseDirectory
   +          else
   +            IO.Path.GetDirectoryName(loc);
   ```
2. `ncc-core.nproj:16`: `<TargetFramework>netcoreapp2.1</TargetFramework>` →
   `<TargetFramework>net10.0</TargetFramework>`.

## Acceptance

### A. Stage1 exit 0 — PARTIAL (3.5a: yes; 3.1a: no, root-caused)

- **3.5a alone** (TFM kept at netcoreapp2.1): `Build-core-base.cmd Debug Build Stage1`
  → **exit 0**, 0 Warning(s) 0 Error(s). (`stage1-mainn-only.log`)
- **3.5a + 3.1a flip** (final tree state): Stage1 → **exit 1**, ICE (verbatim root cause
  in *Blocker* below; full output in `%TEMP%\stage1-with-flip.log`). Reproduced twice.

### B. net10 runtimeconfig / ncc-core.exe / no dev.json — BLOCKED by A (3.1a)

CoreCompile fails before the SDK's `GenerateRuntimeConfigurationFiles`/`_CreateAppHost`
steps (both run after CoreCompile), so no net10 runtimeconfig/apphost was produced.
Q4 expectations (tfm restamp, automatic apphost for TFM ≥ 3.0, dev.json disappearing)
remain untested. Note: under the *current* build (netcoreapp2.1) Stage1 still contains
`ncc-core.runtimeconfig.json` tfm `netcoreapp2.1` + `ncc-core.runtimeconfig.dev.json`.

### C. ncc-core `-version` on modern host — VERIFIED for 3.5a (muxer form)

`bin\Debug\net-4.0\Stage1\ncc-core.exe` doesn't exist (netcoreapp2.1 TFM ⇒ no apphost,
per Q4), so the fixed ncc-core was run via the muxer on .NET 10 instead:

```
$ env:DOTNET_ROLL_FORWARD='LatestMajor'; dotnet bin\Debug\net-4.0\Stage1\ncc-core.dll -version
EXIT=0
Nemerle Compiler (ncc) version 10.0.6.0 (git)
Copyright (c) University of Wroclaw 2003-2008, Nemerle Project Team 2008-2013
All rights reserved.
```

Exit 0, no ICE / `Unhandled exception` banner → the `main.n:52` CodeBase crash is fixed
(the old code exited 2 with an ICE on every Core run, report Q3 row 1). The apphost
form of this check belongs to 3.1 once 3.1a can build. The PDB-writer issue (report Q3
row 12) was NOT touched, per instruction — `-version` does not exercise it either.

### D. Fixed-point sanity — DONE with corrected methodology

SHA256 equality of Stage1-vs-Stage2 (as the task worded it) is **not achievable on this
toolchain and is not the repo's own gate**:

1. Each stage binary embeds per-stage stamps: PE `TimeDateStamp` (offset 0x88), the
   #GUID heap / MVID (~0x250), the RSDS PDB GUID, and the literal output path
   (`...\Stage2\Nemerle.pdb` vs `...\Stage3\Nemerle.pdb` in the debug directory).
   `NemerleAll.nproj:312-328 Validate` therefore IL-diffs **Stage2 vs Stage3** (`Asm2`/`Asm3`), not Stage1 vs Stage2 — Stage1 is compiled by the older checked-in
   boot seed, so Stage1-vs-Stage2 can only differ unless boot is the fixed point.
2. Empirical hashes (3.5a-only config, forced clean Stage2+Stage3):

   | file | Stage1 | Stage2 | Stage3 |
   |---|---|---|---|
   | Nemerle.dll | `3F208395…811E828D` | `60B88461…C26E033A` | `24F323A3…95E14D83` |
   | Nemerle.Macros.dll | `4B973F89…0F288258` | `FA8BB63C…18B4B261` | `4909F954…B4B261` |

   (Stage1 ≠ Stage2 for both; Stage2 ≠ Stage3 by SHA256 for both.)
3. **Byte-run determinism proven**: deleting and rebuilding Stage2's `Nemerle.dll` +
   `Nemerle.Macros.dll` with the *same* Stage1 compiler produced **identical SHA256**
   (`Nemerle.dll 60B88461997721B3…`, `Nemerle.Macros.dll FA8BB63C7B39E206…`), so all
   observed differences are compiler-generation / stage dependent, not random.
4. **Stage2 ≡ Stage3 fixed point (IL-level)**: Stage2-vs-Stage3 diffs are *stamp-only*
   (same file length; 162–164 differing bytes, all inside PE timestamp, #GUID heap,
   RSDS PDB GUID, and the `Stage2`→`Stage3` path string). No IL/table differences.
   `Nemerle.Macros.dll` even reaches fixed point at Stage1-vs-Stage2 (its 168-byte
   Stage1↔Stage2 diff is likewise stamp-only).
5. `Nemerle.dll` Stage1-vs-Stage2 has a real (non-stamp) diff (~3267 bytes incl.
   metadata/string-heap regions) ⇒ the **boot seed predates the current compiler for
   `lib/`** — pre-existing (my edits touch no Nemerle.dll input; Stage1 side was built
   by the untouched boot), exactly the boot-is-stale situation AGENTS.md describes.
   Not my regression; Stage2≡Stage3 is green.
6. The 3.5a edit itself self-compiles: Stage2/Stage3 built green on top of the
   main.n edit (Stage2 `ncc-core.dll` compiled by the new Stage1 ncc).

### E. git diff — as required (plus one pre-existing file)

```
 doc/plans/retarget-compiler-to-net10-checklist.md | 3 ++-   <- PRE-EXISTING, not from this task
 ncc-core.nproj                                    | 2 +-
 ncc/main.n                                        | 8 +++++++-
```
Progress doc is untracked/new. Nothing committed.

## Blocker for 3.1a — boot compiler ICE on the net10 reference set

**Symptom** (verbatim excerpt, Stage1 with `TargetFramework=net10.0`, during ncc-core's
CoreCompile, i.e. before any of my edited source is parsed — the stack shows
`MainClass.main_with_catching → ManagerClass.Run → LoadExternalLibraries →
InternalTypeClass.InitSystemTypes`):

```
error : internal compiler error : assertion ``InternalType.Void != null'' failed in file
C:\RSDN\nemerle\ncc\frontend\hierarchy\Types\TypeInfo.n, line 551.
   at Nemerle.Compiler.TypeInfo.ConstructSubst(...)
   at Nemerle.Compiler.TypeInfo.MakeSubst(list`1 parms, Boolean check_parms)
   at Nemerle.Compiler.FixedType.Require(FixedType t)
   at Nemerle.Compiler.TypeVar.TryRequire(TypeVar t)
   at Nemerle.Compiler.TyCodec._N_minimizeInterfaces_55595(...)
   at Nemerle.Compiler.TyCodec.ReflectTypeBuilder(LibraryReference lib, Map`2 tenv, TypeDef original)
   at Nemerle.Compiler.ExternalPrimitiveTypeInfo..ctor(...)
   ...
   at Nemerle.Compiler.InternalTypeClass.InitSystemTypes()
```

**Root cause** (fully evidenced):

- With net10 TFM the ref set switches to
  `C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\10.0.13\ref\net10.0` (167
  assemblies; the build log shows the new `/r:` list).
- In the **net10** ref set, primitive types gained generic interfaces, absent in the
  netcoreapp2.1 set. dnlib dump of `System.Runtime.dll`:
  - netcoreapp2.1 `System.IntPtr`: (only non-generic ISerializable; `System.Void`,
    `System.Enum`, `System.ValueType`, `System.String`: no generic interfaces)
  - net10.0 `System.IntPtr`: `IComparable`1, IEquatable`1, IParsable`1,
    ISpanFormattable, IUtf8SpanFormattable, IAdditionOperators`3, IBinaryInteger`1,
    **INumber`1, INumberBase`1, IComparisonOperators`3**, … (26 interfaces, mostly
    generic); `System.Enum`: `ISpanFormattable`; `System.String`: `IParsable`1,
    `ISpanParsable`1, `IEnumerable`1`, …
- `InternalTypeClass.InitSystemTypes()`
  (`ncc/backend/hierarchy/InternalTypeClass.n:306-332`) looks up `System.IntPtr`
  (`:315`) **before** setting `Void` (`:317` "Void_tc = lookup ("System.Void")").
  Reflecting IntPtr runs `TyCodec.ReflectTypeBuilder → minimizeInterfaces`
  (`ncc/backend/hierarchy/Codec.n:186`, `Typer.GetMinimal(allInterfaces, _.TryRequire(_))`);
  with ≥2 generic interfaces `TryRequire → FixedType.Require → TypeInfo.MakeSubst(check_parms=true)`
  hits `assert(InternalType.Void != null)` at `ncc/frontend/hierarchy/Types/TypeInfo.n:551`
  — null because we are *inside* `InitSystemTypes`.
- **Minimal deterministic repro** (proves it is the ref set, not the main.n edit — the
  ICE fires before the edit is even parsed):
  `boot\ncc.exe -no-stdlib -target:library -r:"…\10.0.13\ref\net10.0\System.Runtime.dll" t.n`
  → same assertion, exit 2. Control with the netcoreapp2.1 `System.Runtime.dll` (+
  `System.Collections.dll`) → no ICE (exit 1, ordinary compile errors on the reduced
  ref set).

**Implication for the plan**: report "Recommended atomic split" 3.1a ("nothing else")
is falsified — 3.1a needs a small companion compiler change to land, e.g. (options for
the next sub-atom, each needs Stage2≡Stage3 + testsuite 468/167 validation):
(a) initialize `Void` (and any needed primitives without generic-interface deps — Void
itself has no interfaces) earlier in `InitSystemTypes`, before `IntPtr`/`String`; or
(b) skip the `check_parms`/Void assertion while `InternalType` init is in progress
(guard `val.Equals(InternalType.Void)` with a null check in `ConstructSubst`); or
(c) `TyCodec.minimizeInterfaces` with `check_parms=false` during reflection-time
requirement checks. This is compiler-semantics territory — deliberately NOT attempted
here (out of 3.5a/3.1a scope; analogous to the forbidden 3.1b PDB fix).

## Environment notes

- Build entry used: `cmd /c "`"C:\RSDN\nemerle\Build-core-base.cmd`" Debug Build StageN"`.
  No locked-boot failures occurred; `dotnet build-server shutdown` not needed.
- Logs kept: `%TEMP%\stage1-mainn-only.log` (green), `%TEMP%\stage1-with-flip.log`
  (ICE), `%TEMP%\stage2-clean.log`, `%TEMP%\stage3-clean.log` (both green, forced
  rebuilds), `%TEMP%\stage2-rerun.log`.
- To leave the tree in the task's intended final state, the final Stage1 with the flip
  in place fails as documented above; `bin\Debug\net-4.0` currently holds the
  3.5a-only (netcoreapp2.1, working) compiler from the green chain build.
