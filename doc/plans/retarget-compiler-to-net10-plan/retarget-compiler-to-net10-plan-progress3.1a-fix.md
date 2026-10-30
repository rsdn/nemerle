# Progress — 3.1a-fix: init `Void`/`Object`/`ValueType` before reflective lookups

Branch `retarget-compiler`, tree `C:\RSDN\nemerle`. NOT committed (per task).
Date: 30.10.2026. One build at a time.

## Scope

Sub-atom **3.1a-fix** of the net10 plan: make the compiler survive reflecting
.NET 10 reference assemblies. On net10 the primitive structs gained generic
interfaces absent from the old ref sets (`System.IntPtr`: ~26 mostly-generic
interfaces — `IEquatable`1, IParsable`1, INumber`1, …; net10 `System.Boolean`:
the whole `INumber`/operator-interfaces family), and `InternalTypeClass.InitSystemTypes`
(`ncc/backend/hierarchy/InternalTypeClass.n:298-`) reflected types **before**
setting the `InternalType` fields the type solver dereferences. Reflecting such a
type runs `TyCodec.ReflectTypeBuilder → minimizeInterfaces`
(`ncc/backend/hierarchy/Codec.n:186`) → `TypeVar.TryRequire → FixedType.Require →
MakeSubst(check_parms=true)` → `assert(InternalType.Void != null)` at
`ncc/frontend/hierarchy/Types/TypeInfo.n:551` → ICE, exit 2. Root cause and
minimal repro in `…-progress3.5a-3.1a.md` ("Blocker").

`ncc-core.nproj` TFM stays `netcoreapp2.1` (untouched).

## Operational workaround: assert dialogs (MANDATORY for all repro runs)

Failed asserts in these Debug builds can pop an **interactive Windows
Debug.Assert dialog** ("assertion failed … Abort/Retry/Ignore") that blocks the
process with ~0 CPU and no output; two earlier "hangs" of >3 min were such
dialog waits, not computation (the same runs complete in seconds once the UI is
off). Per user instruction the dialog must never appear. Authoritative fix —
`ncc.exe.config` placed next to each ncc.exe **copy** that is executed
(`bin\Debug\net-4.0\boot\ncc.exe.config`, `bin\Debug\net-4.0\Stage1\ncc.exe.config`;
`bin\` is untracked build output; the git-tracked `boot-dnlib\` is NEVER touched):

```xml
<configuration>
  <system.diagnostics>
    <assert assertuienabled="false" />
  </system.diagnostics>
</configuration>
```

(Note: `<trace assertuienabled="false"/>` is rejected by the .NET Framework
config schema — the element is `<assert>`.) With it, failed asserts go to stderr
and the process exits with a code instead of showing UI. `$env:ASSERT_POPUP_ERRORS='0'`
is also set in every session for belt-and-braces (it alone did NOT prevent the
dialog). All A/A2/C/E runs below were executed dialog-free.

## Chosen fix: candidate (a) — reorder `InitSystemTypes` (decision + iterations)

**Candidate (a) chosen** (init primitives early by reordering existing
`lookup(...)` assignments only; candidate (b) — null-tolerant `ConstructSubst` —
not needed, see iterations). Rationale for (a): the asserts are the only
consumers of `Void/Object/ValueType == null` during reflection (nothing uses
them as "init in progress" sentinels: `git grep "assert(InternalType" ncc` shows
each is just a precondition deref), and moving whole existing lines keeps typing
semantics byte-identical.

Static analysis initially suggested moving only `Void`; **runtime evidence
required widening the moved set to `Void`, `Object`, `ValueType`** — iterations,
all reproduced by the deterministic full-refs repro below (fixed compiler = the
one built after each iteration):

1. *Void moved just before `IntPtr`* → ICE moved **earlier**: `Void != null`
   assert at `InternalTypeClass.n:306` — **`System.Boolean`** (the first lookup!)
   triggers the same path on net10; the Blocker doc's IntPtr attribution was
   plausible but the first trigger is Boolean (`INumber<bool>` family).
2. *Void moved to very top* → next ICE: ``assert(InternalType.Object != null)``
   at `ncc/frontend/typing/StaticTypeVar.n:83` — `StaticTypeVar.LowerBound`
   returns `InternalType.Object` (or `.ValueType` for value-constrained typars)
   as the default lower bound; `FixedType.Require`'s
   `(StaticTypeVarRef, t) => tv1.LowerBound.Require(t)` arm (`FixedType.n:332`)
   hits it while minimizing the open generic operator-interfaces
   (`IAdditionOperators`3` etc.) of Boolean's/IntPtr's closure.
3. *`Void`, `Object`, `ValueType` moved to the top* (final fix) → `InitSystemTypes`
   completes on the full net10 ref set; no assertion in any form of the repro.

Safety of the three moved lines (derived from the code): `System.Void` and
`System.Object` have no base type and no interfaces, so their
`ReflectTypeBuilder` calls `minimizeInterfaces()` on the empty list and cannot
recurse into the solver; `System.ValueType` derives from the just-initialized
`Object` and has no interfaces. Their lookups therefore need nothing that is not
already set (`Pointer_tc` setup above them, unchanged). The
`// ordering is important here` Boolean/Int32 block and everything after keep
their relative order; the duplicate-free move removes the old `ValueType` line at
its former position. No other file touched.

## Repro forms (both deterministic, no build needed)

Dialog suppression via `ncc.exe.config` applies to all (see above).

**Reduced repro** (one ref assembly, `-no-stdlib`) — the form from the Blocker
doc. ⚠️ **Artifact warning**: with only `System.Runtime.dll` the run later dies
with ``internal type System.Runtime.InteropServices.DllImportAttribute not
found`` (assert at `InternalTypeClass.n:359` etc.) — that is NOT the target bug:
net10 ref assemblies type-forward `DllImportAttribute` & co out of
`System.Runtime.dll`, so the attribute lookups of `InitSystemTypes` cannot
succeed on a reduced set. Likewise ``internal type
Nemerle.Core.MatchFailureException not found`` (assert at `ManagerClass.n:725`,
reached from `InitNemerleTypes` via `ManagerClass.Run:649`) is a pre-existing,
orthogonal `-no-stdlib` limitation: **unmodified boot ncc hits it identically**
on a full *older-generation* ref set (verified: boot + full netcoreapp3.1 refs
+ `-no-stdlib` → same ICE, exit 2). The reduced repro still shows the target
assert first (it fires during `InitSystemTypes`), so it remains useful, but the
**full-refs forms below are the gates**.

**Full-refs repro** (all 167 ref assemblies of the net10 ref pack, stdlib
present — mimics the real net10-TFM build where ncc-core compiles against the
full ref set + Nemerle.dll):

```powershell
$refdir = "C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\10.0.13\ref\net10.0"
$refs = Get-ChildItem "$refdir\*.dll" | ForEach-Object { "-r:$($_.FullName)" }
& <ncc.exe> -nologo -target:library @refs C:\RSDN\repro31a\t.n   # t.n = class T { }
```

(`-no-stdlib` variant of the full-refs form was also run; it distinguishes the
same way up to the orthogonal `MatchFailureException` stop.)

Note: this compiler spells the flag `-nologo`; `-no-logo` is rejected as an
unknown argument (exit 1) — irrelevant to the ICE.

## A. Before-fix baseline (verbatim) — boot ncc.exe

### A1. reduced repro (boot, `-no-stdlib`, only System.Runtime.dll)

```
& C:\RSDN\nemerle\bin\Debug\net-4.0\boot\ncc.exe -nologo -no-stdlib -target:library "-r:C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\10.0.13\ref\net10.0\System.Runtime.dll" C:\RSDN\repro31a\t.n
```

exit **2** (`C:\RSDN\phase31a-logs\A-baseline-boot-ICE.log`):

```
error: internal compiler error: assertion ``InternalType.Void != null'' failed in file C:\RSDN\nemerle\ncc\frontend\hierarchy\Types\TypeInfo.n, line 551.
   at Nemerle.Compiler.TypeInfo...ConstructSubst(...)
   at Nemerle.Compiler.TypeInfo.MakeSubst(list`1 parms, Boolean check_parms)
   at Nemerle.Compiler.FixedType...unifyTypeArgs_(...)
   at Nemerle.Compiler.FixedType.Require(FixedType t)
   at Nemerle.Compiler.TypeVar.TryRequire(TypeVar t)
   at Nemerle.Compiler.Typer.GetMinimal[T](...)
   at Nemerle.Compiler.TyCodec._N_minimizeInterfaces_55595(...)
   at Nemerle.Compiler.TyCodec.ReflectTypeBuilder(...)
   at Nemerle.Compiler.ExternalPrimitiveTypeInfo..ctor(...)
   ... at Nemerle.Compiler.ManagerClass.Lookup(String typeName)
   at Nemerle.Compiler.InternalTypeClass.lookup(String type_name)
   at Nemerle.Compiler.InternalTypeClass.InitSystemTypes()
   at Nemerle.Compiler.ManagerClass.LoadExternalLibraries(...)
```

### A2. full-refs repro (boot, 167 refs, stdlib) — exit **2**, Void assert present

```
BOOT-EXIT=2
BOOT-VoidAssert-present=True
error: internal compiler error: assertion ``InternalType.Void != null'' failed in file C:\RSDN\nemerle\ncc\frontend\hierarchy\Types\TypeInfo.n, line 551.
```

(`C:\RSDN\phase31a-logs\A2-baseline-boot-fullrefs-stdlib.log`; the boot's own
stderr begins with duplicate-type warnings, then the ICE — the assert fires
while reflecting `System.Boolean`, before any stdlib use. Also run with
`-no-stdlib`: same Void ICE, exit 2 — `A-baseline-boot-ICE.log`.)

## C. After the fix — fixed Stage1 ncc.exe (built by boot, TFM still netcoreapp2.1)

```
& C:\RSDN\nemerle\bin\Debug\net-4.0\Stage1\ncc.exe -nologo -target:library @refs C:\RSDN\repro31a\t.n
EXIT=0
```

**`class T { }` compiles cleanly against the full net10 ref set** (exit 0,
`t.dll` produced). Output is warning-only: duplicate-type warnings of the form
`using type '[System.Runtime …]System.Object' that was defined in more than one
assembly: '[mscorlib …]System.Object' (the first version was used)` — expected,
because stdlib (`Nemerle.dll`) drags mscorlib in alongside the net10 refs and
first-one-wins is the designed reflection behavior
(`C:\RSDN\phase31a-logs\C-repro-stage1.log`). **No `assertion` anywhere in the
output.** Reduced repro with the fixed compiler: no `Void`/`Object` assert
either; it stops only at the orthogonal `-no-stdlib`/forwarded-attribute
artifacts documented above.

## D. Full bootstrap validation

### D-1 `Stage3,Validate` — GREEN baseline reproduced

`cmd /c "`"C:\RSDN\nemerle\Build-core-base.cmd`" Debug Build `"Stage3,Validate`""`
(log `C:\RSDN\phase31a-logs\D-stage3-validate.log`, exit 1 = known-red tail):

* **`FC: no differences encountered` × 5** (fixed-point IL diffs of the last two
  stages — the fix changed no emitted IL).
* Stage2/Stage3 `Nemerle.Compiler.dll` freshly rebuilt from the fixed source
  (timestamps 22:04/22:06).
* Only failure at the very end: `peverify.exe Stage3\Nemerle.Compiler.dll`
  (MSB3073 + its `[MD] .NET Framework Internal error : 0x80131252` lines) —
  the known-red baseline tail, present before this change too.

### D-2 `Stage1,CompilerTests`

First attempt aborted with a transient **file-lock** (not a test failure):
residual MSBuild node PID 53436 (node-reuse race from the D-1 run) held
`Stage1\Nemerle.MSBuild.Tasks.dll` (MSB3026/MSB3027) before tests started.
Cleared with `dotnet build-server shutdown` + killing the stale node; re-run:

`cmd /c "`"C:\RSDN\nemerle\Build-core-base.cmd`" Debug Build `"Stage1,CompilerTests`""`
→ **exit 0**, `Build succeeded. 0 Error(s)`
(log `C:\RSDN\phase31a-logs\D-stage1-compilertests.log`):

* positive suite: **468** `testsuite\positive\…....passed` lines,
  `All tests passed. Time taken: 00:03:56.9932889`
* negative suite: **167** `testsuite\negative\…....passed` lines,
  `All tests passed. Time taken: 00:01:07.7904649`

(`All tests passed.` ×2.)

## F. Final diff

```
 ncc/backend/hierarchy/InternalTypeClass.n | 14 +++++++++++---
 1 file changed, 11 insertions(+), 3 deletions(-)
```

One .n source + this untracked progress doc. `bin\Debug\net-4.0\{boot,Stage1,
Stage2,Stage3}\ncc.exe.config` are untracked build-output-local dialog
suppression configs (see workaround section), never git-tracked paths.

## E. Bonus — full-refs repro with Stage2 ncc.exe

```
& C:\RSDN\nemerle\bin\Debug\net-4.0\Stage2\ncc.exe -nologo -target:library @refs C:\RSDN\repro31a\t.n
STAGE2-EXIT=0   VoidAssert=False   HasAssertion=False
```

(`C:\RSDN\phase31a-logs\E-repro-stage2.log`) — exit 0, no assertion text.
