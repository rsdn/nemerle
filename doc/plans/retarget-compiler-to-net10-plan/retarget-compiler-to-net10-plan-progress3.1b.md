# Progress 3.1b — host-aware PdbType default (Portable on .NET Core, Windows on FX)

Branch `retarget-compiler`. Do NOT commit (per orchestrator). Logs: `.tmp\phase31b-logs\`.

## Intentions

1. Baseline (pre-fix) repro capture:
   - FX: `Stage1\ncc.exe -nologo -g -target:library -out:...fxfix.dll t.n` → exit 0,
     record `fxfix.pdb` first bytes (expect MSF "Microsoft C/C++ MSF 7.00").
   - Core: same with `ncc-core.exe` → capture the pre-fix failure text verbatim.
   - NOTE: `.cs` input fails in Stage1 ("parser not registered" — C# parser plugin absent),
     so the trivial input is `class C {}` as `.n`.
2. Real code shape (plan context was approximate; verified against HEAD 57d5856f6):
   - `PdbType` enum (Windows|Portable|Embedded) is in `ncc/backend/generation/TypesManager.n`
     (no `None`, no `SetDebugDirectory`; dnlib `PdbState.PdbFileKind` is picked in
     `CreatePdbDocument` from `Manager.Options.PdbType`).
   - Default lives solely in `CompilationOptions.PdbType` — `public mutable PdbType : PdbType;`
     (implicit 0 = Windows). CLI flag is `-pdb-type` (alias `-pdbtype`), parsed from
     `parse_command_line()` in `ncc/main.n` AFTER `Options = CompilationOptions()` →
     field initializer is a safe default slot; explicit flag always wins.
   - `RuntimeInfo`/`Environment.n` probe mentioned in the plan context does NOT exist in this
     checkout → implement a minimal netstandard2.0-safe host probe locally.
3. Change 1 (the atom): `ncc/frontend/CompilationOptions.n` — initialize `PdbType` as
   host-dependent default: .NET (Core) host → `Portable`, .NET Framework host → `Windows`
   (unchanged behavior). Detection without reflection risk: primitives live in
   `System.Private.CoreLib` on .NET (Core) and in `mscorlib` on .NET Framework.
4. Expected prerequisite (to verify): the Core-host repro ICEs BEFORE reaching the pdb writer —
   assertion `InternalType.Delegate_Combine != null` (TypeInfo.n:266) fired reentrantly from
   `InitSystemTypes` (`lookup("System.Runtime.CompilerServices.RuntimeHelpers")` → its
   `decode_extension_methods` on Core corlib where RuntimeHelpers is a static class) → some
   concrete delegate's `InitBuiltinMembers` runs before `Delegate_Combine` is assigned at the
   tail of `InitSystemTypes`. If confirmed, fix minimally in the exact spirit of the committed
   3.1a-fix (e9741864a: moved Void/Object/ValueType lookups up): move the local `is_right`/
   `single` defs + the two `Delegate_Combine`/`Delegate_Remove` assignments up to right after
   the `MulticastDelegate_tc` lookup in `ncc/backend/hierarchy/InternalTypeClass.n`.
   Same assignments, only earlier; FX ordering semantics preserved.
5. Validation: A `Stage1` exit 0; B FX pdb = MSF (compare bytes to pre-fix); C Core pdb = BSJB
   and exit 0; D explicit `-pdb-type windows` on Core still attempts MSF (DiaSymReader error
   proves the flag wins); E `Stage3,Validate` FC ×6 + sole red pefverify, `Stage1,CompilerTests`
   468+167/0; F `git diff --stat` limited to ncc/ sources + this doc.

## Results

DONE (not committed, per instruction). Diff: `ncc/frontend/CompilationOptions.n`
(+16/-1: host-gated `PdbType` default + `IsRunningOnNetCore` probe),
`ncc/backend/hierarchy/InternalTypeClass.n` (net move of the `is_right`/`single` defs and the
`Delegate_Combine`/`Delegate_Remove` assignments from the tail of `InitSystemTypes` up to just
after `ParamArrayAttribute_tc`), plus this untracked doc.

### Deviations from the atom context (all verified against HEAD 57d5856f6)

1. No `RuntimeInfo`/`Environment.n` probe exists; no `SetDebugDirectory`/`PdbType.None`; the
   enum is `Windows|Portable|Embedded` (`ncc/backend/generation/TypesManager.n`) and the flag
   is `-pdb-type` (not `-pdb:`). Default slot = `CompilationOptions.PdbType` field initializer;
   `main.n` constructs Options before `parse_command_line()`, so the explicit flag wins.
2. Host probe: `typeof(int).Assembly.FullName.StartsWith("System.Private.CoreLib")` —
   reflection-free, netstandard2.0-safe; FX/Mono → `mscorlib` → Windows default unchanged.
3. The Core repro's pre-fix failure was NOT the DiaSymReader error — it was an earlier ICE
   (`.tmp\phase31b-logs\pre-core.txt`): `assertion 'InternalType.Delegate_Combine != null'
   failed ... TypeInfo.n, line 266` — reentrant from `InitSystemTypes`' `RuntimeHelpers`
   lookup (`decode_extension_methods` → `collect_members` constructs a concrete delegate before
   the tail assignment; Core corlibs treat `RuntimeHelpers` as an extension holder). Fixed by
   the minimal reorder described above (same precedent as 3.1a-fix e9741864a). First reorder
   attempt (right after `MulticastDelegate_tc`) NRE'd instead — `ExternalMethodInfo` ctor needs
   `ParamArrayAttribute_tc` (`SystemHelpers.IsDefined` null-`TypeInfo` NRE); the final position
   after `ParamArrayAttribute_tc` satisfies both.
4. Trivial repro input is `class C {}` as **`.n`** (`.cs` → "can't parse file with extension
   'cs', parser not registered" in Stage1; C# parser plugin not deployed there).
5. dnlib 4.5.0 writes Windows MSF on the Core host fine even without any
   `Microsoft.DiaSymReader.Native` binary in the stage dir (D below) — the atom premise's
   error mode is gone with dnlib 4.5; the host-aware default is still delivered as specified.

### Acceptance evidence

- A. `Build-core-base.cmd Debug Build Stage1` → `Stage1 exit: 0` (twice; final build log
  `.tmp\phase31b-logs\stage1-build2.log`, `0 Error(s)`).
- B. FX path unchanged: `Stage1\ncc.exe -nologo -g -target:library -out:...\fxcsc.dll t.n`
  → exit 0; `post-fx.pdb` (post-fix) and `fxfix.pdb` (pre-fix, same command) are both 9728 B
  with identical head `4d 69 63 72 6f 73 6f 66 74 20 43 2f 43 2b 2b 20 4d 53 46 20 37 2e 30 30
  0d 0a 1a 44 53` = ASCII `Microsoft C/C++ MSF 7.00\r\n\x1aDS\0\0\0` (MSF magic).
- C. Core path: pre-fix `ncc-core.exe -g` → exit 2, ICE `Delegate_Combine` (recorded above);
  post-fix → exit 0 and `post-core.pdb` first 4 bytes `42 53 4a 42` = **`BSJB`** (Portable;
  header `BSJB............PDB v1.0`).
- D. Explicit flag wins on Core: `ncc-core.exe -nologo -pdb-type:windows -g` → exit 0 and
  `corewin.pdb` head = `Microsoft C/C++ MSF 7.00\r\n\x1aDS` (MSF) while the flagless default
  produced `BSJB` with the same exe → the explicit value is honored. No DiaSymReader failure
  occurred (dnlib 4.5's managed MSF writer works on this host; outcome recorded per spec).
- E. `Build "Stage3,Validate"` → `FC: no differences encountered` count **6** (ncc-core.dll,
  ncc.exe, Nemerle.Compiler.dll, Nemerle.dll, Nemerle.Macros.dll, Nemerle.MSBuild.Tasks.dll);
  sole error = known-red `peverify.exe Stage3\Nemerle.Compiler.dll` (MSB3073).
  `Build "Stage1,CompilerTests"` → exit 0, `All tests passed.` ×2; harness reruns with full
  capture: positive `passed-lines=468 failed=0`, negative `passed-lines=167 failed=0`.
- F. `git diff --stat`: only `ncc/frontend/CompilationOptions.n` and
  `ncc/backend/hierarchy/InternalTypeClass.n` among tracked files, plus this doc; the
  `doc/plans/retarget-compiler-to-net10-checklist.md` modification pre-dated this atom
  (adds the 3.1b/3.1e entries; untouched by me). Nothing committed.

### Notes for later phases

- The `Delegate_Combine` reorder also unblocks *compiling* (not just `-version`) with
  `ncc-core.exe` on the .NET 10 host — a `class C {}` reference-set compile now runs end-to-end.
  Deeper Core-host compile work (3.5) starts from a better state.
- ncc-core ICE pre-fix repro kept at `.tmp\phase31b-logs\pre-core.txt` (also
  `pre-corenog.txt` — ICE occurs with and without `-g`).
