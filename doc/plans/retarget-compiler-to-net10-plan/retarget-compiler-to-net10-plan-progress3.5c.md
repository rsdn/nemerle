# Progress 3.5c — LibraryReferenceManager: Core-safe assembly-location diagnostics + lib_path load fallback

## Intentions

Authority: report items 3 and 4 of the LibraryReferenceManager section
(`retarget-compiler-to-net10-plan-report3.1-3.5.md`, ~lines 72-79).
File: `ncc/backend/hierarchy/LibraryReferenceManager.n` (the task text says
`ncc/frontend/hierarchy/...`; git grep places the file under `ncc/backend/hierarchy/` — same file, moved by the split).

1. **Item 3 — location diagnostics.** `getAssemblyLocation` (both overloads, ~:157-165)
   and the duplicate-IMacro-identity compile error (~:199) use
   `System.Uri(assembly.CodeBase).LocalPath`, which throws
   `PlatformNotSupportedException` on .NET Core ≥ 3.0 — the *diagnostics themselves*
   crash (callers ~:322, ~:349). Fix: ONE private helper
   `private GetAssemblyLocation (asm : SR.Assembly) : string` that tries
   `Uri(asm.CodeBase).LocalPath` and falls back to `asm.Location` on
   `NotSupportedException` / null `CodeBase`; empty-safe. Use the same helper in
   both overloads and at the identity-error site; route the two diagnostics through it.
   Where a location resolves to `""` (assembly not loaded from disk), callers must
   still format sensibly — fall back to `"<" + asm.GetName().Name + ">"` at the
   message level while keeping the current wording.
2. **Item 4 — load fallback (~:282).** After `SR.Assembly.Load(name)` fails, the code
   retries `Assembly.Load(name + strongPart)`, which on Core only probes ALC.Default
   via deps.json and never resolves arbitrary strong names. Add, BEFORE the strong-part
   retry, a probe of every `_lib_path` entry:
   `SR.Assembly.LoadFrom(SR.Path.Combine(lib, name + ".dll"))` guarded by
   `File.Exists`; any failure there must not mask the original error — the original
   "not found" diagnostic must still be produced. Keep the strong-part retry
   (FX-superset compatibility).
3. Do NOT touch `DirectoryOfCodebase` (~:123-143, dead mono gac hack, Phase 7.1) or
   `LibraryReference.n` (atom 3.5d). No other files.

## Verification plan

- A. `Stage1` build exit 0.
- B. Behavioral probe on the CORE host: two different dlls each defining macro `M.Foo`,
  consumer .n referencing both with `-r:` ×2 → clean compile ERROR naming both locations,
  no PNSE stack / ICE. Capture exact error text.
- C. FX-host parity: `bin\Debug\net-4.0\boot\ncc.exe` compiles the same repro → same
  error text (quote both).
- D. `Rebuild "Stage3,Validate"` → `FC: no differences encountered` ×5, sole
  `: error MSB` = known-red pefverify MSB3073 on `Stage3\Nemerle.Compiler.dll`;
  `Build "Stage1,CompilerTests"` → 468 positive + 167 negative, 0 failed.
- E. `git diff --stat` = only `ncc/backend/hierarchy/LibraryReferenceManager.n` (+ this doc).

## Results

### Code changes (single file: `ncc/backend/hierarchy/LibraryReferenceManager.n`, +84/-14)

1. **Helper (new)** `private static GetAssemblyLocation (asm : SR.Assembly) : string`:
   `CodeBase == null ? asm.Location : Uri(CodeBase).LocalPath`, `catch NotSupportedException => asm.Location`
   (covers the documented PNSE + dynamic assemblies; empty result possible, callers tolerate it).
2. **`getAssemblyLocation(SR.Assembly)`** — body replaced by a call to the helper (was `Uri(CodeBase).LocalPath`).
3. **`getAssemblyLocation(SR.AssemblyName)`** — resolves the already-loaded `SR.Assembly` with matching
   `FullName` (AppDomain) and routes it through the same helper; otherwise null-safe CodeBase → `""`
   (`SR.AssemblyName` carries no Location; its CodeBase is null on Core).
4. **IMacro-identity error (old :197-202)** — local `getAssemblyPath(type)` now calls the helper and renders
   `"<" + asm.GetName().Name + ">"` when the location is empty; wording of the `Message.Error` unchanged.
5. **Diagnostics (old :322, :349)** — unchanged text; already routed through `getAssemblyLocation` → helper.
6. **Load fallback (old :268-283)** — the `_lib_path` sweep in `LookupAssemblyForExecution` (the probe that
   must run BEFORE the `Assembly.Load(name + strongPart)` fallback, which on Core only probes ALC.Default via
   deps.json) is now the hardened `TryLoadFromLibPath(name)`: same LoadFrom + File.Exists semantics as the old
   inline `lookup`/`assembly_by_name`, but each directory probe swallows ALL failures (`catch { _ => null }`),
   so a bad image in one search dir can never mask the original "not found" diagnostic (the old inline sweep
   propagated non-FileNotFound/ArgumentException throws). The strong-part retry itself is untouched (FX superset).
7. `DirectoryOfCodebase` untouched; no other files. `getAssemblyLocation` public virtual signatures preserved
   (IDE `IntelliSenseModeLibraryReferenceManager` override still binds).

### A — Stage1

`Build-core-base.cmd Debug Build Stage1` → `0 Error(s)`, `EXIT=0` (`.tmp\phase35c-logs\stage1.log`).
One intermediate iteration: `SR.AppDomain` was an alias bug (`SR = System.Reflection`); fixed to `System.AppDomain`.

### B — Core-host behavioral probe of the site-(b) diagnostic

Repro (`.tmp\phase35c-logs\repro\`): `Macro1.n` = trivial `macro Foo` in `namespace M` (real IMacro);
`Macro2.n` = class `M.Foo` implementing a **foreign** `Nemerle.Compiler.IMacro` declared inside Macro2 itself +
`[assembly: Nemerle.Internal.ContainsMacro("M.Foo")]` — the exact "duplicate IMacro identity" condition.
Both referenced from `consumer.n` (`-r:` ×2), macro loaded by bare name via `-lib:.` (exercises the
`TryLoadFromLibPath` sweep; on Core `Assembly.Load("Macro2")` could not resolve it), Stage1 `ncc-core.exe` (.NET 10):

```
error: The macro `M.Foo' implement the IMacro interface from 'C:\RSDN\nemerle\.tmp\phase35c-logs\repro\Macro2.dll' but the compiler use this interface from 'C:\RSDN\nemerle\bin\Debug\net-4.0\Stage1\Nemerle.Compiler.dll'.
CORE_EXIT=1
```

Clean compile ERROR, both locations resolved, no PNSE stack, no ICE, exit 1.

Runtime reality check (`cbtest`): shipped .NET Core 3.1.32 AND 10.0.13 no longer throw from `CodeBase`
for disk-loaded assemblies (returns `file:///…`; PNSE was reverted before 3.0 RTM), so the pre-fix binary
(Stage2, old code) prints the same error on .NET 10 — the report's premise is conservative for these runtimes.
The helper's catch is NOT dead code: on .NET 10 a **dynamic assembly** throws `NotSupportedException` from
`CodeBase` with `Location == ""` (measured), i.e. exactly the crash class the helper now survives.

### C — FX-host parity

Same shell, same repro sources compiled per host (FX cannot load Core-emit: refs `System.Private.CoreLib`),
`bin\Debug\net-4.0\boot\ncc.exe`:

```
error: The macro `M.Foo' implement the IMacro interface from 'C:\RSDN\nemerle\.tmp\phase35c-logs\repro\Macro2.dll' but the compiler use this interface from 'C:\RSDN\nemerle\bin\Debug\net-4.0\boot\Nemerle.Compiler.DLL'.
FX_EXIT=1
```

Identical wording/structure; the second path differs only because the FX host runs the compiler out of `boot\`.
On FX the helper takes the same old `Uri(CodeBase).LocalPath` branch (CodeBase non-null) → behavior unchanged in kind.

### D — Parity gates (logs in `.tmp\phase35c-logs\`)

* `Rebuild "Stage3,Validate"` → `FC: no differences encountered` count = **5**; sole `: error MSB` line =
  `NemerleAll.nproj(359,5): error MSB3073: … pefverify.exe … Stage3\Nemerle.Compiler.dll … exited with code 1`
  (known red). (`stage3-validate.log`)
* `Build "Stage1,CompilerTests"` → positive **468** passed, negative **167** passed, two × `All tests passed`,
  `0 Error(s)`. (`stage1-tests.log`)

### E — Diff scope

`git diff --stat`: `ncc/backend/hierarchy/LibraryReferenceManager.n` (+79/-14 region) plus this untracked
progress doc. Pre-existing unrelated changes not made by this atom: `doc/plans/retarget-compiler-to-net10-checklist.md`
(was already modified at start) and untracked `AGENTS.md`. Not committed (per instructions).
