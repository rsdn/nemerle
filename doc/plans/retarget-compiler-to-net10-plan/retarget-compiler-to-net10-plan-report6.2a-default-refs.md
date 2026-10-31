# Report 6.2a — Default references on Core + run-launch: decision report

Research-only atom: no repository files modified, no repository builds. All
experiments ran existing `bin\Debug\net-4.0` binaries in `.tmp\phase62a\`.

## Q1 — DEFAULT REFERENCE SET ON A CORE HOST

Current: `ncc/frontend/ManagerClass.n:561-570` (`LoadExternalLibraries`, stdlib
branch) loads exactly `typeof(object)`/`typeof(Uri)`/`typeof(XmlNode)`/`NList`
assemblies + `Options.References` at :580-581; `Nemerle.Macros` loads
separately (:609-617), unaffected. On Core those four are
`System.Private.CoreLib/Uri/Xml` + `Nemerle` — no `Console`/`Regex`/`Linq`.
Reproduced: `ncc-core.exe testsuite\positive\hello.n` → `error: unbound name
'Console'` (exit 1). The `-no-stdlib` branch (:532-559, already sorts
`mscorlib|netstandard|System.Runtime` first, :544-555) is untouched; every
stage/tooling project compiles with it (`NoStdLib=true`: `ncc.nproj:11`,
`ncc-core.nproj:11`, `Nemerle.nproj:14`, `Nemerle.Compiler.nproj:13`,
`Nemerle.Macros.nproj:13`, `Nemerle.Location.nproj:15`,
`Nemerle.Compiler.Test.nproj:20`; historically `ncc/frontend/Makefile:41`).
Only direct `ncc` invocations and the in-process harness (`HostedNcc.n:26-45`,
new `ManagerClass` per test, no `-no-stdlib`) hit the default set.

Options: (a) enumerate the shared-framework dir —
`typeof(object).Assembly.Location` is inside the ACTIVE framework dir
(`shared\Microsoft.NETCore.App\10.0.13\`), correct under roll-forward, zero
guessing; 185 dlls = 167 reference-relevant + 5 `System.Private.*` + 13
native. (b) ref pack (`packs\Microsoft.NETCore.App.Ref\<ver>\ref\net10.0`):
`DOTNET_ROOT`+version globbing, absent on runtime-only installs — fragile,
rejected as compiler mechanism; but note it holds exactly **167 dlls, none
`System.Private.*`** — the filtered dir (a) == the SDK's implicit framework
set. (c) deps.json: `ncc-core.deps.json` is the compiler's own closure, not
the target surface — rejected. Roslyn (measured, SDK 10.0.402 `csc.dll` on
Core): zero `/r:` fails even `System.Object` (CS0518); `/nostdlib-` identical.
Core csc has NO implicit BCL — zero-`-r` UX comes from MSBuild/SDK, not the
compiler; FX ncc (Nemerle) does have one and `println` expands to
`System.Console`, so FX-parity must live in the compiler.

**DECISION: compiler-side default set on Core host (option a/i), FX branch
untouched.** Harness-side `-r` passing is rejected: the shared Exec lines
(`NemerleAll.nproj:394,396`) would carry ~160 paths and, per 6.1 §6, it masks
a genuine compiler-on-Core defect. Spec — stdlib branch, after the 4 `load()`
calls:

```text
if typeof(object).Assembly.GetName().Name == "System.Private.CoreLib":  # Core host gate
    dir = DirectoryOf(typeof(object).Assembly.Location)
    for dll in Sorted(Directory.GetFiles(dir, "*.dll"), Ordinal):
        n = NameWithoutExt(dll)
        if not StartsWith(n, "System.Private.") and not IsNative(n):
            tryAddLibrary(dll, isUserReference=false)   # own try/catch, never fatal
# existing Options.References loop (:580-581) stays AFTER, unchanged
```

`IsNative`: skip exact `{coreclr, hostpolicy, clrjit, clretwrc, clrgc,
clrgcexp, mscorrc, msquic, mscordbi}`, prefixes `{mscordaccore,
Microsoft.DiaSymReader.Native, *.Native}`, and `System.Private.*` (already
loaded via `typeof`; avoids impl+facade double-ingest). Everything else
(`mscorlib`, `netstandard`, `System*`, `Microsoft.CSharp/VisualBasic*/Win32.*`,
`WindowsBase`) loads. The existing `tryEx` (:523-530) rethrows outside
IntelliSense mode, so the loop needs its own swallow. Ordering: defaults first
(Private.CoreLib first, as today), framework set in ordinal order, user `-r`
after (`isUserReference=true`); `-no-stdlib` and `Nemerle.Macros` untouched;
on FX the gate is false (name is `mscorlib`) → master-safe by construction
(still port per cross-branch rule).

Measured (existing binaries, 167-ref `@`-file): `hello.n`, `bug-1153.n`,
`Issue-1267.n`, `Issue-git-0160.n` compile exit 0, zero load errors; empty
compile 0.68 s (default 4) vs 1.02 s (167) → ≈ +0.34 s/compile; no cross-test
library cache exists (`LibraryReferenceManager.n:65-74` is per-instance), so
expect ≈ +2–3 min per suite run — acceptable; caching is a possible follow-up,
not 6.2b. Residual `Issue-git-0232.n` (`Nemerle.Linq` macro) /
`CompilerEventsTest.n` failures are fixture provisioning, not default refs.

## Q2 — CLASS B (14 run-launch tests): ROOT CAUSE PROVEN, FIX VALIDATED

Launch path: Exec (`NemerleAll.nproj:394,396`) passes no `-r` → `Main.n:155-159`
selects `DefaultProcessStartInfoFactory` → `Process.Start(<test>.exe)`
(`NccTest.n:301-309`). A bare managed PE without apphost is started by the FX
mscoree shim. Reproduced (compile `testsuite\positive\tyvars.n` via
`ncc-core.exe -t:exe`, exec by name):

```text
Unhandled Exception: System.IO.FileNotFoundException: Could not load file or
assembly 'System.Private.CoreLib, Version=10.0.0.0, ...'
EXIT=-532462766   ; = 0xE0434352 — exactly the harness-reported code
```

So the test compiles (BCL-free) but the emitted exe references Core-only
`System.Private.CoreLib` and has no apphost/runtimeconfig (ncc writes none —
grep: no runtimeconfig writer; ncc-core's own come from the SDK). Fix = two
parts, BOTH required, validated end-to-end with existing binaries:

1. Add `-r dotnet -rp exec` to the two `CompilerTests` Exec lines — the
   mechanism exists (`RuntimeProcessStartInfoFactory.n:23-35` runs
   `dotnet exec <exe>`; hostfxr accepts the `.exe` name, verified).
2. Harness writes `<output-name>.runtimeconfig.json` next to each `-t:exe`
   output when the host is Core (`NccTest.compile`, `NccTest.n:192-197`);
   content `{"runtimeOptions":{"framework":{"name":"Microsoft.NETCore.App",
   "version":"10.0.0"}}}`. Without it `dotnet exec` dies with hostpolicy fatal
   `-2147450749` (verified). Compiler-side emit rejected (shared ncc emit for a
   test-semantics problem; SDK builds already emit runtimeconfig via MSBuild).

Validation (`Tests\positive`, existing harness): with runtimeconfig pre-created
→ `tyvars.n: run ... passed` (exit 0); without → `failed ... exit code
-2147450749`. Runtime co-location already satisfied — `Nemerle.dll`,
`Nemerle.Macros.dll`, `dnlib.dll` are in both Tests dirs
(`TestFramework\*.*` copy, `NemerleAll.nproj:372-376,391-392`); without them
the run fails `FileNotFoundException: Nemerle, Version=10.0.33.0` (verified).

## Q3 — CLASS F (3) + NEGATIVES (13)

| Test | Missing | Disposition |
|---|---|---|
| `form.n` | `/// REFERENCE: System.Windows.Forms` (:1) — FX GAC via `LoadWithPartialName` (`NccTest.n:389`), unresolvable on Core | skip on Core |
| `access-checks.n` | `// REFERENCE: System.Web` (:2) — `System.Web.UI`/`SessionState` absent from `Microsoft.NETCore.App` | skip on Core |
| `security.n` | CAS `[PermissionSet(SecurityAction.PermitOnly,…)]` (:35) — CAS attributes absent | skip on Core |

The `Microsoft.WindowsDesktop.App` pack IS installed, but adding it to default
refs is overreach (FX never had WinForms in the implicit 4); both
REFERENCE-marker tests fail at harness resolution, not compile. Skip mechanism:
markers parse in `NccTestDescription.n:52-65` (`NO-TEST` → `NoTest` →
`GetNotRunResult("not a test")`, `NccTest.n:102-103`); no per-OS/host gate
exists. Add retarget-only token `// NO-CORE` parsed in `NccTestDescription`
when running on Core; the token avoids the substring `NO-TEST` so master's
`.*NO-TEST.*` regex (:64) cannot match it — shared test files stay inert on
FX. New norm: 465 passed + 3 not-run. Negatives (13) are all class-A cascades
— direct `main.n`, `external-extensions.n`, `unbound-member.n`; diagnostic
drift `base-calls.n`, `bug-0664.n`, `calling.n`, `cg-post.n`, `comp-branch.n`,
`notifypropertychanged.n`, `tyenf.n`, `void-parm2.n`, `void-parm3.n`,
`where-constraints0.n` — auto-resolve with Q1, as do positive class C (10
`*-lib.dll` cascades) and Z (1).

## Q4 — IMPLEMENTATION SPLIT FOR 6.2b/6.2c/6.2d

* **6.2b — Core default refs** (Q1): one file, `ncc/frontend/ManagerClass.n`
  stdlib branch. Gates: `hello.n`/`bug-1153.n` compile with zero `-r`;
  `Stage3,Validate` FC×5 green; `CompilerTests` re-run. **FC/bootstrap:** all
  stage compiles pass `-no-stdlib` (evidence above), so the new branch is
  INERT during Stage1-4 — the seed-vs-Stage1 "different default refs" worry
  does not arise; the fixed point is expected from the first rebuild (the
  change is runtime behavior, invisible to stage IL). If anyone ever drops
  `-no-stdlib` from a stage build, fixed point is only meaningful from Stage2
  on (seed applies old set, Stage1+ the new one).
* **6.2c — run-launch (class B)**: `NemerleAll.nproj:394,396` add
  `-r dotnet -rp exec`; `NccTest.n` writes `runtimeconfig.json` for `-t:exe`
  outputs, Core-host-gated. Retarget-only change; `-r dotnet` on an FX host
  would break master, so if this ever syncs, gate the Exec properties per host.
* **6.2d — class F skips**: `NccTestDescription.n` + one `// NO-CORE` comment
  line in each of the 3 test files. Overall 6.2 acceptance: positive 465
  passed / 3 not-run / 0 failed; negative 167/167; FC×5 green.
* Risks: (1) 167-assembly load per test (+0.34 s, memory across 468
  `ManagerClass` instances) — measure RSS on first green run; (2) facade
  ambiguity (`netstandard`+`System.Runtime`+`mscorlib` forwarders) — 4 sample
  compiles clean, full suite is the real gate; (3) user `-r` repeating a
  default path double-loads — pre-existing FX behavior
  (`LibraryReferenceManager.n:70-74`), unchanged; (4) Mono also hosts a
  `System.Private.CoreLib` — Mono is out of scope post-retarget.
* Cross-branch: the 6.2b source change is shared-surface (per AGENTS.md),
  cherry-pick to master; it is a no-op there via the host gate.
