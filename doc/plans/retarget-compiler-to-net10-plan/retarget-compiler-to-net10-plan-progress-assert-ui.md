# Progress — suppress Win32 assert dialogs in builds (switchable)

Branch `retarget-compiler`, tree `C:\RSDN\nemerle`. Date: 30.10.2026.

## Problem

Debug ncc.exe (.NET Framework) shows a modal Win32 "Assertion Failed:
Abort/Retry/Ignore" dialog on ICE asserts. Agents/CI cannot dismiss it —
builds block on the user's machine. Previously handled by ad-hoc untracked
`ncc.exe.config` copies under `bin\` (which the binding-redirect build step
ignores/overwrote — those stage `ncc.exe.config` files are generated output).

## Implemented design (default: dialogs OFF; assert text goes to output, process fails)

Authoritative knob for the .NET Framework assert UI is the exe's own config:
`<system.diagnostics><assert assertuienabled="false" /></system.diagnostics>`.
`Microsoft.Common.CurrentVersion.targets` `FindAppConfigFile` picks up an
`App.config` from `@(None)` items and `GenerateBindingRedirects` preserves
foreign sections — so the right fix is a source App.config per exe:

1. `ncc\App.config` (new) + `<None Include="ncc\App.config"/>` in `ncc.nproj` —
   every BUILT Framework ncc.exe (Stage1-4, Install; TestFramework copies) ships
   `ncc.exe.config` with the section (merged with its binding redirects).
2. `snippets\Nemerle.Test\Nemerle.Compiler.Test\App.config` (new) + `<None>` in
   its nproj — the CompilerTests harness runs the compiler in-process; its
   asserts would also dialog.
3. `boot-dnlib\ncc.exe.config` (new, tracked text sidecar to the seed binary) +
   `NPrepareBoot` now also copies `$(NRootBoot)\*.config` to `$(NBoot)` —
   direct manual/agent runs of the seed compiler are covered too. The copy
   overwrites the stale ad-hoc files in `bin\...\boot` on every build.

Toggle (documented in the file comments): edit `assertuienabled` to `true`
in the `.config` next to the exe (or in the source App.config) to get the
Abort/Retry/Ignore dialog back while debugging.

Not changed: MSBuildTask.cs (BeforeToolRun hook rejected in favour of the
source-level App.config), ncc-core (Core hosts have no assert UI).

## Acceptance (verified by the test subagent)

A. `Build-core-base.cmd Debug Build Stage1` exit 0; `Stage1\ncc.exe.config`
   contains BOTH `<assert assertuienabled="false"/>` and the binding redirects;
   `boot\ncc.exe.config` present (seed copy).
B. Stage2 run exits 0; `Stage2\ncc.exe.config` like A.
C. No-dialog assert trigger: `bin\Debug\net-4.0\boot\ncc.exe` on the reduced
   net10 ref set (`-no-logo -no-stdlib -target:library` + only
   `Microsoft.NETCore.App.Ref\10.0.13\ref\net10.0\System.Runtime.dll` +
   `class T {}`) → assert text in captured output, exit code != 0, and NO GUI
   dialog appears (user confirms).
D. Harness: `Tests\positive\Nemerle.Compiler.Test.exe.config` regenerated with
   the assert section (appears after CompilerTests target rebuilds it).
E. Full parity: `Stage3,Validate` FC 5/5 (peverify known-red only) +
   `Stage1,CompilerTests` 468+167/0.
F. `git status`: only the 5 files above (3 modified nproj, 2/3 new configs)
   + docs; boot-dnlib change is a TEXT config, not a seed binary.

## Results

Acceptance run by the test subagent, 30.10.2026 (22:20–22:45). Logs:
`C:\RSDN\assertui-logs\` (A-stage1.log, B-stage2.log, E1-stage3-validate.log,
E2-stage1-compilertests.log). **Overall: A–F PASS. No GUI assert dialog
appeared at any point, including during C.**

### A — Stage1 + configs: PASS

`cmd /c "Build-core-base.cmd Debug Build Stage1"` → exit 0 (`Build succeeded.
0 Warning(s) 0 Error(s)`).

1. `bin\Debug\net-4.0\Stage1\ncc.exe.config` — assert element AND redirects
   both present (redirects preserved alongside the foreign section):
   ```
   9:    <assert assertuienabled="false" />
   18:       <bindingRedirect oldVersion="0.0.0.0-4.0.11.0" newVersion="4.0.11.0" />
   ```
   (bindingRedirect entries continue to file end, e.g. line 330.)
2. `bin\Debug\net-4.0\boot\ncc.exe.config` — present (NPrepareBoot copy of the
   seed sidecar), assert element, no bindingRedirects. First (and only) 9 lines:
   ```
   <?xml version="1.0" encoding="utf-8"?>
   <configuration>
     <!-- Seed sidecar for boot-dnlib\ncc.exe (see ncc\App.config for the rationale):
          NPrepareBoot copies it into the boot folder next to the seed compiler.
          Toggle: assertuienabled="true". -->
     <system.diagnostics>
       <assert assertuienabled="false" />
     </system.diagnostics>
   </configuration>
   ```

### B — Stage2: PASS

`cmd /c "Build-core-base.cmd Debug Build Stage2"` → exit 0.
`bin\Debug\net-4.0\Stage2\ncc.exe.config`, line 9:
`    <assert assertuienabled="false" />`

### C — Dialog-free assert trigger: PASS

Ref pack verified: `C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\10.0.13\ref\net10.0\System.Runtime.dll` exists.
`bin\Debug\net-4.0\boot\ncc.exe -nologo -no-stdlib -target:library -out:C:\RSDN\repro311\t.dll -r:<System.Runtime.dll> C:\RSDN\repro311\t.n 2>&1`:
exit code **2**, duration **< 1 s**, full output:

```
debug: Internal compiler error 'internal type System.Runtime.InteropServices.DllImportAttribute not found', please report a bug to bugs.nemerle.org. You can try modifying program near this location.
error: internal compiler error: assertion failed in file C:\RSDN\nemerle\ncc\frontend\ManagerClass.n, line 725: internal type System.Runtime.InteropServices.DllImportAttribute not found.    at Nemerle.Compiler.ManagerClass.LookupTypeInfo(String typeName)   at Nemerle.Compiler.ManagerClass.Lookup(String typeName)   at Nemerle.Compiler.InternalTypeClass.lookup(String type_name)   at Nemerle.Compiler.InternalTypeClass.InitSystemTypes()   at Nemerle.Compiler.ManagerClass._N__N_lambda__66216__66402.apply_void()   at Nemerle.Compiler.ManagerClass._N_tryEx_66247(...)   at Nemerle.Compiler.ManagerClass.LoadExternalLibraries(Boolean forceInitNemerleTypes)   at Nemerle.Compiler.ManagerClass.Run()   at Nemerle.CommandlineCompiler.MainClass.main_with_catching()
```

Assertion text went to captured output and the process exited with code 2
immediately — an Abort/Retry/Ignore dialog would have blocked the process
indefinitely instead. **No modal dialog appeared.**

### D — Harness config: PASS

After the E2 `Stage1,CompilerTests` run,
`bin\Debug\net-4.0\Tests\positive\Nemerle.Compiler.Test.exe.config` line 6:
`    <assert assertuienabled="false" />`
Timestamp changed (20:35:45 → 22:41:40) — the TestFramework target regenerates
it with the section from the new App.config.

### E — Full parity: PASS

1. `Debug Build "Stage3,Validate"`: `FC: no differences encountered` **×5**
   (Stage2 vs Stage3 *.il). Sole MSBuild error is the known-red pefverify one:
   `NemerleAll.nproj(350,5): error MSB3073: The command "...peverify.exe"
   "...\bin\Debug\net-4.0\Stage3\Nemerle.Compiler.dll" exited with code 1.`
   (its 1125 `[MD] ... 0x80131252` token lines are that same check's tool
   output; no other error kind in the log).
2. `Debug Build "Stage1,CompilerTests"`: exit 0. Positive suite: **468**
   `...passed` lines; negative suite: **167** `...passed` lines; 0 failed
   lines; `All tests passed. Time taken: 00:02:06.4` and
   `All tests passed. Time taken: 00:00:31.4` — twice, as expected.

### F — git status: PASS with one anomaly

Modified (tracked): `NemerleAll.nproj`, `ncc.nproj`,
`snippets/Nemerle.Test/Nemerle.Compiler.Test/Nemerle.Compiler.Test.nproj` —
as expected. Untracked new: `ncc/App.config`,
`snippets/Nemerle.Test/Nemerle.Compiler.Test/App.config`,
`boot-dnlib/ncc.exe.config` (text sidecar; seed binary untouched), plus docs.

**Anomaly (not fixed, per instructions):** `doc/plans/retarget-compiler-to-net10-checklist.md`
is also modified — the diff is only the plan-checkbox bookkeeping for sub-atom
3.1.1 (adds the `[~] 3.1.1 Suppress assert dialogs...` line); it looks like
plan bookkeeping from the implementation session, but it is a modified tracked
file outside the listed 3. Additionally untracked (not tracked-modified, docs
or harness files): `AGENTS.md` and several other plan progress/report docs from
earlier phases.
