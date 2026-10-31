# Progress 8.3f — default-reference precedence + load-failure silence (R2-01/R2-02)

Status: **DONE** (2026-10-31; gates A-D green, see §Evidence; findings R2-01
(proven non-reproducible — no precedence change), R2-02 (warning added) from
report-review2, the 6.2b Core default-ref branch in
`ncc/frontend/ManagerClass.n` `LoadExternalLibraries`).

## Findings and resolution

### R2-01 — "explicit `-r` loses to the 167-dll default set": DOES NOT REPRODUCE (direction inverted)

review2 assumed the duplicate-type policy is *first-registered-wins* and that
the defaults (loaded at `ManagerClass.n:590-595`) therefore shadow a later
explicit `-r` (loaded at `:607`). The registry actually keeps **last-registered
first**, so the user `-r` already wins:

- `LibraryReferenceManager.LoadTypesFrom` **prepends** each new type entry to
  the namespace node's cache list
  (`ncc/backend/hierarchy/LibraryReferenceManager.n:476-481`):

  ```nemerle
  match (mainnode.Value)
  {
    | NotLoaded(x)      => NamespaceTree.TypeInfoCache.NotLoadedList([e, x]);
    | NotLoadedList(xs) => NamespaceTree.TypeInfoCache.NotLoadedList(e :: xs);
    | _                 => NamespaceTree.TypeInfoCache.NotLoaded(e);
  };
  ```

- `NamespaceTree.Node.LoadValue` folds `elems` head-first and keeps the head
  entry per generic arity (`ncc/backend/NamespaceTree.Node.Hacks.n:19-37`,
  warning "(the first version was used)"). Head = last registered.

`AddLibrary` itself (`LibraryReferenceManager.n:70-74`) only appends to
`_referencedAssemblies`, and `LoadLibrariesContents` (:280-284) registers in
that insertion order — but the prepend above inverts the effective *type*
precedence, so with the current order (defaults, then `-r`) the explicit `-r`
copy is the one used. Live probe below proves it with two assemblies of the
same identity (`System.Data.Common, Version=0.0.0.0` shadow vs the runtime's
`Version=10.0.0.0`): the warning names the **shadow** as the used one and the
shadow-only API compiles.

Consequences for the fix choice: the candidate **reorder** ("load user `-r`
before defaults") would *introduce* the bug it tries to fix (the defaults are
registered last → prepend → the *defaults* would win), and the **skip**
variant (skip defaults whose simple name an explicit `-r` will provide) is
unnecessary for precedence and only changes the duplicate-load noise. Chosen
mechanic: **none — no precedence change**; instead a comment inside the gate
documents the prepend-wins semantics so nobody "fixes" it by reordering.
(For reference, a bogus `-r` file is a hard error, not a silent drop:
`ncc -r this_does_not_exist.dll` exits 2 with
`internal compiler error: Could not find file ...` from the `LibraryReference`
ctor's `File.ReadAllBytes`; so even a hypothetical skip could not silently
lose types on a typo'd path. Review2's 5-warning probe used *identical*
files, where the warning direction is not observable — that is how the
inversion was missed.)

### R2-02 — per-dll load failures swallowed silently: FIXED (warning)

`catch { | _ => () }` in the Core default-ref loop now emits
`Message.Warning("skipping default framework reference '<dll>': could not be
loaded as metadata (<ExceptionTypeName>)")` and continues. `Message.Warning`
is the existing sink used across the compiler (`ncc/frontend/parsing/Utility.n:173`,
same call style as `LibraryReferenceManager.n:595`). The `is_native` filter
runs before the `try`, so the known native payloads stay silent, and the
healthy fwdir run emits zero of these warnings (control logs clean). On FX
nothing changes: the whole block stays inside the
`typeof (object).Assembly.FullName.StartsWith("System.Private.CoreLib")`
gate, which is false on .NET Framework.

## Scope / constraints

- Only `ncc/frontend/ManagerClass.n` + this progress doc.
- Change stays inside the `System.Private.CoreLib` gate → FX path identical.
- File is CRLF, no BOM (verified before/after edit).

## BEFORE probes (pre-change, Stage3 ncc-core.exe, 8e0ab9688 tree)

Runner: `C:\RSDN\nemerle\bin\Debug\net-4.0\Stage3\ncc-core.exe` (2026-10-31
07:41 drop). Sources in `.tmp\phase83f\probe\`:

- `SystemDataMarker.n`/`ShadowDataSet.cs` → shadow libs (csc, /nostdlib+,
  referencing System.Private.CoreLib) exposing a `System.Data.DataSet` class
  with only `static Marker83F() : int` — colliding *type identity* with the
  runtime's `System.Data.DataSet` (defined in `System.Data.Common.dll`),
  built under two names: `System.Data.dll` and `System.Data.Common.dll`.
  (A new-type-only API would NOT demonstrate precedence: unique names merge
  into the node list without collision.)
- `Test.n` calls `System.Data.DataSet.Marker83F()`; `Hello.n` control;
  `TestLib.n` + `ProbeLib.dll` non-colliding `-r` control.

| probe | result |
|---|---|
| zero-`-r` control | exit 0, no output |
| non-colliding `-r ProbeLib.dll` | exit 0, no output |
| `-r ...\probe\System.Data.dll` (shadow DataSet) | exit 0 + `warning: using type '[System.Data, Version=0.0.0.0, ...]System.Data.DataSet' that was defined in more than one assembly: '[System.Data.Common, Version=10.0.0.0, ...]System.Data.DataSet' (the first version was used)` |
| `-r ...\probe\System.Data.Common.dll` (same simple name as fwdir file) | exit 0 + warning naming `[System.Data.Common, Version=0.0.0.0]` as the used one |
| `-r this_does_not_exist_83f.dll` | exit 2, `error: internal compiler error: Could not find file ...this_does_not_exist_83f.dll` |

The two shadow rows used the **shadow** copy (its version `0.0.0.0` and its
`Marker83F` API compile) → explicit `-r` wins today → R2-01 as described
cannot be reproduced and no precedence change is warranted.

## AFTER probes (post-change, same Stage3 ncc-core.exe rebuilt from the edited tree)

| probe | result |
|---|---|
| zero-`-r` control | exit 0, no output (no spurious default-ref warnings) |
| non-colliding `-r ProbeLib.dll` | exit 0, no output |
| `-r ...\probe\System.Data.Common.dll` (same simple name) | exit 0 + `using type '[System.Data.Common, Version=0.0.0.0, ...]System.Data.DataSet' ... (the first version was used)` → shadow used |
| `-r ...\probe\System.Data.dll` | exit 0 + warning naming `[System.Data, Version=0.0.0.0]` as used → shadow used |
| `-r this_does_not_exist_83f.dll` | exit 2, `error: internal compiler error: Could not find file ...` (unchanged) |

### P2 — warning on unloadable default dll (runtime-proven, not just review)

The active shared-framework dir must not be written to, so the branch was
exercised through a **private copy**: `robocopy` of the runtime dir + the
`host\` folder into `.tmp\phase83f\dotnetroot\dotnet\shared\Microsoft.NETCore.App\10.0.13\`,
a garbage-bytes `ZZTest83f.dll` (name not in the `is_native` filter) dropped
into the copy, and `ncc-core.exe` launched with `DOTNET_ROOT` pointed at the
fake root (the apphost resolves the framework from there; the real install is
untouched). `typeof (object).Assembly.Location` then sits in the copy, so the
6.2b enumeration sees the corrupt dll:

```
warning: cannot load default framework reference 'ZZTest83f.dll' (skipped; BadImageFormatException)
exit=0
```

(identical with the Stage1 and the rebuilt Stage3 compiler; the run still
produced hello.exe). The copy also contains all natives (`coreclr.dll`,
`hostpolicy.dll`, `clrjit.dll`, `mscordaccore*`, `System.Private.*`,
`Microsoft.DiaSymReader.Native*`) — the `is_native` filter ran before the
`try`, so none of them emitted a warning; exactly one line, for ZZTest83f.
Healthy-fwdir controls (rows 1-2 above) emit zero of these warnings.

## Gates

### B. `./build.ps1 -Targets Stage3,Validate -Rebuild` (`.tmp\phase83f\validate.log`)

`EXITCODE=0` (last log line), `Build succeeded. Nemerle installed to ...`,
zero `: error`, zero `Build FAILED`, **FC: no differences ×10** (5 assemblies
× both directions).

### C. `./build.ps1 -Targets Stage1,CompilerTests` (`.tmp\phase83f\tests.log`)

`EXITCODE=0` (last log line). Positives **461 passed + 7 "not a test on
core"** (the `NO-CORE` set), negatives **167 passed**, both sections ending
`All tests passed.` (×2), zero failures, zero `: error`.

### D. `git diff --stat`

```
 ncc/frontend/ManagerClass.n | 17 +++++++++++++++--
 1 file changed, 17 insertions(+), 2 deletions(-)
```
plus the untracked progress doc. File stays CRLF (892 CRLF = 892 LF), no BOM.
All changed lines (590-610) sit inside the
`when (typeof (object).Assembly.FullName.StartsWith("System.Private.CoreLib"))`
block (line 576) → on .NET Framework the whole block is skipped and the FX
path is unchanged.
