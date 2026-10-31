# Phase 6.1 — Test runner `Nemerle.Compiler.Test.nproj`: net461 → net10.0

Plan text: "Перевести тестовый раннер `Nemerle.Compiler.Test.nproj` `net461` →
`net10.0` (это Nemerle-код, собирается самим `ncc`); `Nemerle.Test.Framework` —
по D2." (D2: `Nemerle.Test.Framework` stays `netstandard2.0` — it already is,
`Nemerle.Test.Framework.nproj:13`, nothing to do.)

Scope of THIS atom: TFM flip + making the harness BUILD and START as a net10
apphost. Full-suite *content* parity on Core belongs to 6.2; only
harness-plumbing issues (refs, config, apphost) are fixed here.

## 1. Audit

### 1.1 Build path

* Project: `snippets\Nemerle.Test\Nemerle.Compiler.Test\Nemerle.Compiler.Test.nproj`,
  old-style-shaped .nproj importing `Nemerle.Sdk.props/.targets` (⇒ Microsoft.NET.Sdk),
  TFM `net461`, `NoStdLib=true`, OutputType=Exe, signed with
  `Nemerle.Compiler.snk` (`NKeysDir` from the stage chain).
* Built by `NemerleAll.nproj` target `TestFramework` (:448-456): MSBuild
  Restore + `$(NTargetName)` over `@(NTestFramework)` (4 projects:
  Nemerle.Test.Framework, **Nemerle.Compiler.Test**, Nemerle.Diff,
  Nemerle.Diff.Tests) with `Nemerle=$(NCurBin)` — i.e. compiled by the stage
  chain's `ncc-core.exe` via the `Ncc` MSBuild task (`Nemerle.MSBuild.targets`
  CoreCompile). Nemerle.Diff(.Tests) stay `netstandard2.0`/`net461` — not ours.
* `CompilerTests` (:367-397) copies `$(NBin)\TestFramework\*.*` + fixtures +
  `Nemerle.WPF.dll` into `$(NBin)\Tests\{positive,negative}` and Execs
  `Nemerle.Compiler.Test.exe` **from those dirs** (`ExeRunner` empty on
  Windows). A net10 apphost needs `.exe` + `.dll` + `.runtimeconfig.json` +
  `.deps.json` co-located — `*.*` copy delivers all four.
* Reference model on net10 (proven by `ncc-core.nproj`, the only net10.0 .nproj
  today): NO explicit framework `<Reference>`s — the implicit
  `Microsoft.NETCore.App` targeting pack (SDK 10.0.402, ref pack 10.0.13 on
  disk) flows into `@(ReferencePath)` → `/ref:` per item; `NoStdLib=true` adds
  `/no-stdlib`, `GreedyReferences` unset (task default false) adds
  `/greedy-references:-` (`MSBuildTask.cs:231-236`). `ProduceReferenceAssembly=false`
  is required because ncc's CoreCompile emits no refint assembly
  (ncc-core.nproj:17-18 comment).

### 1.2 Harness sources (all .n, compiled in-process with the compiler under test)

`Main.n` (CLI, wiring) + `HostedNcc.n` (drives `Nemerle.Compiler.ManagerClass`
**in-process** — same netstandard `Nemerle.Compiler.dll` the previous phases
migrated; the FX-ness of the harness host was never needed for compilation) +
`ExternalNcc.n` / `ExternalVerifier.n` / `RuntimeProcessStartInfoFactory.n` /
`DefaultProcessStartInfoFactory.n` / `ProcessExtensions.n` /
`ThreadPoolUtils.n` (process plumbing) + `NccTest*.n`, `NccResult/Type.n`,
`Verifier*.n` (test semantics) + `Nemerle.Test.Framework` (P2P,
netstandard2.0, untouched).

FX-only API surface found (`git grep` over `snippets/Nemerle.Test`):

| Site | API | Core verdict |
|---|---|---|
| `NccTest.n:64` | `AppDomain.CurrentDomain.BaseDirectory` | works on Core (= app dir) |
| `NccTest.n:389` | `Assembly.LoadWithPartialName(reference)` (fallback in `ResolveAssemblyPath`, only hit when the reference file is NOT found in `-output:.`/BaseDirectory/CWD) | API exists in the net10 ref pack (verified `findstr /m` in ref pack 10.0.13 `System.Runtime.dll`); returns null / throws-free → becomes a "Failed to resolve reference" test error instead of an FX load; no code change in this atom |
| `Main.n:111` | `System.Diagnostics.Debug.Assert(false)` behind `-debugger` | exists; on Core no Abort/Retry/Ignore dialog exists (assert writes to stderr) — App.config suppression becomes moot, not harmful |
| `Main.n:185` | `typeof(object).GetType().FullName == "System.RuntimeType"` | true on Core too → `ThreadRunner` path kept |
| `NccTest.n:27` | `AppDomain.FirstChanceException` | commented-out, dead |
| `Properties/AssemblyInfo.n` | `Nemerle.Macro.Resource/Settings` attrs pointing at non-existent `Properties/Resources.resx`/`Settings.settings` | no-op macros today (files absent) — left untouched |
| `ThreadPoolUtils`, `ProcessExtensions`, `External*` | `Process`, `ThreadPool.QueueUserWorkItem`, `WaitHandle.WaitAll` | all Core-safe |

No `System.Configuration`/`ConfigurationManager`/Remoting/BinaryFormatter/
Registry usage anywhere in the harness sources.

### 1.3 External tools the harness Execs — and whether the green run uses them

`git grep -n "Process|peverify|ildasm|ProcessStartInfo" -- snippets/Nemerle.Test`:

* **peverify**: only via `-verifier/-verify <path>` (`Main.n:71-78` →
  `ExternalVerifier.Verify` → `Process.Start`). The `CompilerTests` Exec lines
  (`NemerleAll.nproj:394,396`) pass NO `-verifier` → `verifier = None()` →
  `verifyOutputAssembly` is never called. The PEVerify options in `--help` are
  opt-in only; on Core they simply stay unused (no Windows SDK peverify ship
  with the runtime — same situation as the 468+167 FX-green runs, which also
  never passed the flag).
* **ildasm**: zero references in the harness.
* **Generated test exes** (`testOutputAssembly`, `NccTest.n:299-371`): for
  tests with `BEGIN-OUTPUT`, the harness launches the freshly compiled
  `<test>.exe` **directly** (`DefaultProcessStartInfoFactory`, no `-r` passed).
  This is the one process launch the green suite actually performs. After the
  flip, ncc (running on Core) emits a plain managed exe — no apphost, no
  runtimeconfig (ncc/lib contain no "runtimeconfig" writer) — and a bare
  managed exe does not start on Core when exec'd by name. The harness already
  has the Core-safe mechanism for this: `-r dotnet -rp exec`
  (`RuntimeProcessStartInfoFactory` prefixes the runtime + args before the exe
  path). Wiring that into the `CompilerTests` Exec lines is 6.2 work (test-run
  semantics), recorded here; expect the BEGIN-OUTPUT subset to fail until 6.2.
* `WaitForExitOrKill` watchdog: FX killed the child after 20 s; Core keeps the
  same semantics (`Process.Kill` works, kill is child-only on Core).

### 1.4 FX-only artifacts

* `App.config` (`<system.diagnostics><assert assertuienabled="false"/>`, added
  in the 3.1.1-era commit c3af18bad): .NET config file, read only by the FX
  runtime. On Core it is never opened (no `System.Configuration.ConfigurationManager`);
  assert-UI does not exist on Core anyway. **Keep the file** (still consumed if
  anything FX-side ever runs the sources), note inert.
* `<Reference Include="mscorlib"/>` + `<Reference Include="System"/>` (:45-46):
  FX GAC-style names; `mscorlib` is absent from the Core ref pack (unresolvable
  reference). Must go — ncc-core.nproj's zero-`<Reference>` shape is the model.

## 2. Design (minimal coherent change, mirrors ncc-core.nproj)

1. `<TargetFramework>net461</TargetFramework>` → `net10.0`.
2. Add `<ProduceReferenceAssembly>false</ProduceReferenceAssembly>` with the
   ncc-core rationale (no refint from ncc's CoreCompile).
3. Delete the `mscorlib` and `System` `<Reference>` items; the implicit net10
   targeting pack supplies the whole compile surface via `@(ReferencePath)`.
   Keep the three Nemerle HintPath references (`Nemerle`, `Nemerle.Macros`,
   `Nemerle.Compiler`, `Private=True`) — they copy the compiler binaries the
   harness loads in-process next to the apphost AND flow into the
   `TestFramework\*.*` → `Tests\{positive,negative}` copies unchanged.
4. App.config: keep as `<None>`, comment its inertness on Core (kept in the
   nproj comment; file untouched).
5. NO source-code changes: the audit found no blocking FX API; apphost stays on
   (net10 default `UseAppHost=true` for Exe), so `Nemerle.Compiler.Test.exe`
   remains launchable by name from the Tests dirs exactly as the Exec lines
   spell it. `Nemerle.Compiler.Test.runtimeconfig.json` (+`.deps.json`) are
   generated by the SDK the same way they are for ncc-core.

## 3. Risks / watch-list (6.2 findings, not fixed here)

* **BEGIN-OUTPUT subset**: generated exes cannot exec by name on Core (§1.3,
  third bullet). Expected failures; fix = `-r dotnet -rp exec` in the
  CompilerTests Exec (6.2), which also needs the generated exes to be
  Core-loadable (`dotnet exec <bare managed exe>` without runtimeconfig uses
  default framework resolution — 6.2 to verify).
* `ThreadRunner` requests an 80 MB thread stack (`Main.n:186`); whether CoreCLR
  honors the requested size differs from the FX CLR in the general case —
  watch for StackOverflow-style test crashes on deep-recursion tests.
* `Assembly.LoadWithPartialName` fallback (§1.2) degrades to a clean
  "Failed to resolve reference" error for any test referencing an assembly not
  present in the Tests dir — a fixture-provisioning question for 6.2.
* Flat stage dirs (`$(NBin)\TestFramework\`) keep mixing the net10 harness
  with net461 Nemerle.Diff.Tests output; `*.*` copy is content-agnostic
  (ncc-core/Validate already prove this mix).
* `Nemerle.Compiler.Test.exe.config` (old App.config copy) may linger in
  `TestFramework\` from pre-flip builds; ignored by the Core host, deleted by
  the target's `DEL Tests` + fresh copies anyway.

## 4. Changes made

* `snippets/Nemerle.Test/Nemerle.Compiler.Test/Nemerle.Compiler.Test.nproj` —
  TFM flip (1), ProduceReferenceAssembly (2), FX `<Reference>` removal (3),
  explanatory comments. No .n source changes. No NemerleAll.nproj changes.

## 5. Evidence — gate B (`Stage1,CompilerTests`), first full run

Log `.tmp\phase61\tests.log` (`PS1-TESTS EXIT=1`).

* Stage1 chain + `TestFramework`: **build green**. Harness produced as net10
  apphost: `Nemerle.Compiler.Test.exe` PE Machine=0x8664 (native x64 apphost),
  plus `Nemerle.Compiler.Test.dll`, `.runtimeconfig.json`
  (`"tfm": "net10.0"`, framework `Microsoft.NETCore.App 10.0.0`),
  `.deps.json` (lists Nemerle / Nemerle.Compiler / Nemerle.Macros / dnlib /
  System.CodeDom). `-help` smoke printed the usage banner from the Core host.
* The suite **ran on Core**: positive summary verbatim
  `67 tests passed, 401 tests failed. Time taken: 00:02:45.4922542`.
  The positive `Exec` failed (MSB3073) so the negative Exec never ran;
  the negative command was re-run manually (identical arguments, from
  `Tests\negative`): `154 tests passed, 13 tests failed`
  (`.tmp\phase61\negative-manual.log`). FX baseline was 468+167/0.
* Per-failure clustering: `.tmp\phase61\failures-final.tsv` (+ the classifier
  scripts `cluster*.ps1`). Classification below (§7).

## 6. Root cause of the dominant class (proven, NOT harness plumbing)

Top signatures (verbatim, positive suite):

```
C:\RSDN\nemerle\testsuite\positive\AsLongBug-2.n(5,1,5,22):error: referenced namespace `System.Console' does not exist
C:\RSDN\nemerle\testsuite\positive\bug-1153.n(L,..):error: unbound name `Console' in `System'
C:\RSDN\nemerle\testsuite\positive\hello.n(L,..):error: unbound name `Console'
C:\RSDN\nemerle\testsuite\positive\Issue-0352.n: run......failed -> Test finished with exit code -532462766
C:\RSDN\nemerle\testsuite\positive\params.n: Failed to resolve reference 'params-lib.dll'.
```

Controlled experiment (same `Nemerle.Compiler.dll`, standalone CLI, Tests dir):

```
Stage1\ncc-core.exe -out:.tmp-hello.dll ..\..\..\..\testsuite\positive\hello.n
  -> error: unbound name `Console'                          (exit 1)
Stage1\ncc-core.exe -out:.tmp-hello.dll -r:<sharedFramework>\System.Runtime.dll -r:<sharedFramework>\System.Console.dll ..\hello.n
  -> exit 0
```

So the failure is NOT the harness (not deps.json / probing / co-location —
the standalone ncc-core.exe CLI fails identically): `ncc` never loaded the
Core-split BCL into the *compile-time* reference set. Source:
`ncc/frontend/ManagerClass.n:561-570` `LoadExternalLibraries` — when stdlib is
not disabled it loads exactly four assemblies via
`load(typeof(object))` / `typeof(Uri)` / `typeof(XmlNode)` / `NList`:

* on .NET Framework: mscorlib + System + System.Xml + Nemerle — the full BCL
  surface the suite was written against (why the FX harness was green);
* on Core: System.Private.CoreLib + System.Private.Xml + Nemerle — no
  Console/Regex/Linq/Data (implemented in separate assemblies).

The stage chain never exposed this because every stage build passes
`/no-stdlib` + explicit `/ref:` items. The 6.2 plan text ("run the suite on
Core") owns this compiler-side gap: `LoadExternalLibraries`' default set must
become host-aware (it lives in the shared netstandard `Nemerle.Compiler.dll`,
so the FX code path must keep working — e.g. `load()` additional types whose
FX assembly is already among the four). Fixing it in the harness (prepending
`-r:` args) would mask a genuine compiler-on-Core defect, so per the atom's
scope discipline it is NOT done here.

Plumbing check (6.1 scope) — nothing to fix found: the harness starts, drives
the compiler, the 67+154 passing tests exercise in-process macro compilation,
fixture copying (`TestFramework\*.*`, WPF, Linq/Unsafe) and reference
resolution (`ResolveAssemblyPath` file probes hit the copied fixtures;
`Assembly.LoadWithPartialName` degrades cleanly to
`Failed to resolve reference '<artifact>'` for genuinely absent upstream
outputs). All 401+13 failures are accounted for by the classes below.

## 7. Failure classification for 6.2

### Positive suite — 401 failed / 468

| Class | Count | Meaning | Disposition |
|---|---|---|---|
| A-default-BCL-not-referenced | **373** | Compile errors `unbound name 'Console'` / `referenced namespace 'System.*' does not exist` / `unbound type name 'Regex/Expression/IDataReader/…'`; includes 15 'Expected warning … hasn't occured' whose blocks carry the same BCL errors | Fix `ManagerClass.LoadExternalLibraries` default set for Core hosts (6.2); re-run — most C/D classes evaporate |
| B-run-launch | **14** | Tests with `BEGIN-OUTPUT` that compiled (macro-only/BCL-free) then failed at `run...`: the generated bare managed exe exec'd by name starts and dies with exit code 0xE0434352 (managed CLR exception — the FX mscoree shim loading a Core-targeted assembly) | Wire `-r dotnet -rp exec` (`RuntimeProcessStartInfoFactory`, already implemented in the harness) into the `CompilerTests` Exec args (6.2) |
| C-cascade-upstream-artifact | **10** | `Failed to resolve reference '<X>-lib.dll'` — the `X-lib.n` producer test itself died in class A, so its artifact was never built | Auto-resolve when A is fixed |
| F1/F2/F3 content | **3** | `form.n` (`System.Windows.Forms`), `access-checks.n` (`System.Web.UI.Page`), `security.n` (`PermissionSet` not an attribute — CAS absent) — FX-only APIs resolved via FX GAC/`LoadWithPartialName` today | Per-test 6.2+ decisions (skip/port; suite-content, not plumbing) |
| Z-residual | **1** | `extension-methods.n` (no detail line; `extension-methods-lib.dll` producer failed in class A) | cascade of A |

### Negative suite — 13 failed / 167 (manual re-run)

| Class | Count | Tests |
|---|---|---|
| A direct (BCL errors break the test) | 3 | main.n, external-extensions.n, unbound-member.n |
| Expected-diagnostic mismatch (A-cascade: unexpected unbound-BCL errors change the emitted message set / line mapping — e.g. base-calls.n expected the closure error at line 7, it surfaced at line 21 next to `unbound name 'Console'`) | 10 | base-calls.n, bug-0664.n, calling.n, cg-post.n, comp-branch.n, notifypropertychanged.n, tyenf.n, void-parm2.n, void-parm3.n, where-constraints0.n |

Expectation for 6.2: fixing the class-A default reference set should restore
≈ 373+10+1+3(pos) and 3+10(neg); the persistent remainder is B (14, needs the
runtime-exec wiring) + F (3 content).

## 8. Evidence — gate A (`Stage3,Validate -Rebuild`)

(Filled in below after the run; log `.tmp\phase61\validate.log`.)

