# Progress 3.5e — resx macro: explicit Core-host diagnostic

## Intentions

Scope (down-sized by orchestrator decision): make the `Resource(...)` resx macro in
`macros/Resource.n` fail with an EXPLICIT, actionable compile error when the compiler
runs on a .NET (Core) host, instead of surfacing a raw `FileNotFoundException` from
`Assembly.Load("System.Windows.Forms, ...")` via the generic catch.

Orchestrator decision context:
- The testsuite has ZERO resx consumers (verified via `git grep`), so implementing a
  full resx parser or adding `System.Resources.Extensions` packages is DEFERRED.
  This atom only fixes the diagnostic.

Planned steps:
1. Read `macros/Resource.n`; locate the `Assembly.Load("System.Windows.Forms, ...")`
   call (~:67-69) and the generic catch (~:146).
2. Read the PdbType host-gate probe idiom in `ncc/frontend/CompilationOptions.n` and
   mirror it as an inline local probe in the macro (e.g. checking whether
   `typeof(int).Assembly.FullName` starts with `System.Private.CoreLib`).
3. Before the `Assembly.Load`, when on a Core host: `Message.Error(...)` with an
   explicit message: `Resource(...)` is not supported on .NET (Core) hosts because
   `ResXResourceReader` (System.Windows.Forms) is Framework-only; include the macro
   name and the resx path being processed. Framework host behavior unchanged.
4. Decide on prefixing the generic catch (~:146) with a "resx reading failed in
   Resource macro" hint — only if it does not touch the Framework success path;
   record the decision here.
5. Build Stage1; probe with Core `ncc-core.exe` (expect new explicit message, exit != 0)
   and FX `boot\ncc.exe` (expect NO new Core-only message).
6. Full gates: Stage3+Validate (FC x5, sole error known-red pefverify) and
   CompilerTests (468+167/0).
7. `git diff --stat`: only `macros/Resource.n` + this progress doc. No commits.

## Results

### Code change (`macros/Resource.n`, +9/-1, LF endings preserved, 5555 bytes)

Inside `ResourceImpl.makeWrapperClass`, immediately before the
`Assembly.Load("System.Windows.Forms, Version=4.0.0.0, ...")`:

```nemerle
// Host probe mirrors CompilationOptions.IsRunningOnNetCore: .NET (Core)
// implements primitives in System.Private.CoreLib, .NET Framework in mscorlib.
when (typeof (int).Assembly.FullName.StartsWith("System.Private.CoreLib"))
{
  Message.Error($<#The Resource("$(resPath)") macro is not supported when the compiler runs on .NET (Core): ResXResourceReader (System.Windows.Forms) is Framework-only.#>);
  return;
}
```

The probe idiom is copied from `ncc/frontend/CompilationOptions.n:73-76`
(`IsRunningOnNetCore`, the PdbType host gate) as an inline local probe — no new
dependency from macros/ onto ncc internals. Message includes the macro name and
the resolved resx path (`resPath`).

Catch decision (item 3): APPLIED. The catch at old :146 wraps only the resx
read/generate flow inside `makeWrapperClass` (not shared with unrelated flows),
and only runs on the exception path, so the Framework success path is untouched:
`catch { | e => Message.Error("resx reading failed in Resource macro: " + e.Message); }`.

Determinism for FC: the probe and both strings are constant IL; `Message.Error`
text is runtime diagnostics only — nothing environment-dependent in emitted IL.

### A. Stage1 build

`cmd /c "Build-core-base.cmd Debug Build Stage1"` → **EXIT=0**
(0 Error(s); log `.tmp\phase35e\stage1-build.log`; Nemerle.Macros.nproj rebuilt
with the changed Resource.n).

### B. Core-host probe (new explicit error)

`.tmp\phase35e\resxprobe.n` = `[assembly: Nemerle.Macro.Resource("test.resx")];` +
trivial module; `test.resx` = valid minimal resx with one string entry.
Compiled with Stage1 Core compiler (note: `-project-path` is treated as the
*project file* path, the macro takes its directory):

```
C:\RSDN\nemerle\bin\Debug\net-4.0\Stage1\ncc-core.exe -nologo "-project-path:...\resxprobe.nproj" resxprobe.n
CORE_EXIT=1
resxprobe.n(2,12,2,47): error: The Resource("C:\RSDN\nemerle\.tmp\phase35e\test.resx") macro is not supported when the compiler runs on .NET (Core): ResXResourceReader (System.Windows.Forms) is Framework-only.
```

Exit != 0, explicit actionable message, no raw `FileNotFoundException` string. ✓

### C. Framework path untouched

Same probe with FX boot compiler
(`bin\Debug\net-4.0\boot\ncc.exe` — managed FX PE, has `ncc.exe.config`, no
runtimeconfig.json): **FX_EXIT=0, empty stdout/stderr**, emitted `out.exe`
containing the macro-generated `Hello` accessor and `ResourceManager` references
(byte-scan: contains 'Hello' = True, 'ResourceManager' = True) — i.e. the FX
host loaded System.Windows.Forms and read the trivial resx exactly as before;
the new Core-only message is NOT printed. ✓

### D. Full gates

- `Debug Rebuild "Stage3,Validate"` (log `.tmp\phase35e\stage3-validate.log`):
  FC ×5 **all** `FC: no differences encountered` (Stage2 vs Stage3 IL:
  ncc-core.dll, Nemerle.Compiler.dll, Nemerle.dll, Nemerle.Macros.dll,
  Nemerle.MSBuild.Tasks.dll). Sole MSBuild error = known-red pefverify:
  `NemerleAll.nproj(359,5): error MSB3073: ... peverify.exe
  "bin\Debug\net-4.0\Stage3\Nemerle.Compiler.dll" exited with code 1`
  (its 1126 `[MD] 0x80131252` pefverify diagnostics + 1 MSB3073 = the reported
  "1127 Error(s)"; no other `: error` lines in the log). ✓
- `Debug Build "Stage1,CompilerTests"` (log `.tmp\phase35e\compiler-tests.log`):
  **EXIT=0**, `All tests passed.` ×2; counted passed-result lines:
  positive **468** (459 `.n` + 9 `.cs`) **/ 0 failed**, negative **167 / 0
  failed** — exact baseline match. (The 5 tracked
  `testsuite/positive/Bugs/*.n` files live in a subdir the harness glob
  doesn't enumerate — pre-existing, unrelated.) ✓

### E. git diff --stat

```
 macros/Resource.n | 10 +++++++++-
 1 file changed, 9 insertions(+), 1 deletion(-)
```

Plus untracked `doc/.../retarget-compiler-to-net10-plan-progress3.5e.md`
(this file) and pre-existing untracked `AGENTS.md` (not touched). No commits.
