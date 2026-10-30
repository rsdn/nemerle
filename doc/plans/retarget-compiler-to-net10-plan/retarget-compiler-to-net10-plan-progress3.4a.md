# Progress — Phase 3.4a: Ncc MSBuild task prefers the .NET 10 apphost `ncc-core.exe`

Branch `retarget-compiler`, working dir `C:\RSDN\nemerle`. Scope per report
`retarget-compiler-to-net10-plan-report3.1-3.5.md`, "Recommended atomic split"
row **3.4a**: make `ToolTask` resolve `ncc-core.exe` (net10 apphost, present in
every stage dir since 3.1a/3.1b) instead of the legacy Framework `ncc.exe`,
while `ncc.nproj` still emits both. Do NOT commit; `ncc.nproj` removal is 3.4b.

## Plan

1. `tools/msbuild-task/MSBuildTask.cs` — in `FindExecutable(string toolName)`,
   derive a core tool name from `toolName` (`"ncc.exe"` → `"ncc-core.exe"`;
   anything else, incl. MONO's `ncc.bat`/`ncc`, maps to nothing) and probe it
   **first** in the same candidate locations in the same order:
   `CompilerPath\`, then the task assembly's own directory. If the core
   variant is absent, the legacy resolution (returning `ncc.exe` / bare name)
   stays byte-for-byte intact — older installs and mono keep working.
2. Evidence line: if `ToolTask` does not log the tool path at normal
   verbosity, add one `Log.LogMessage(..., MessageImportance.Normal)` with the
   resolved path in `GenerateFullPathToTool` (sanctioned by the task spec).
3. No other behavior changes — arguments/response file/exit codes are
   produced by the same `NccCommandLineBuilder`.

## Acceptance (from task spec)

- A. `Build-core-base.cmd Debug Build Stage1` exit 0; normal-verbosity log at
  `.tmp\phase34a-logs\stage1-vn.log` shows the resolved `ncc-core.exe` path.
- B. `Stage3,Validate` → 6× "FC: no differences encountered", sole error the
  known-red pefverify MSB3073; `Stage1,CompilerTests` → 468 positive +
  167 negative passed, 0 failed.
- C. Static fallback reasoning (no runtime probe): with no `ncc-core.exe` in
  the dir, the unchanged legacy branch returns `ncc.exe`.
- D. `git diff --stat`: only `tools/msbuild-task/MSBuildTask.cs` + this file.

## Status log

- [x] Progress file created.
- [x] Edit MSBuildTask.cs (core-variant probe + one log line).
- [x] A: Stage1 green + tool-resolution evidence in /v:n log.
- [x] B: chain rebuilt on Core hosts — **FC fixed point regressed to 5/6** on
  `Nemerle.Macros.dll`; STOP per spec ("B fails where the Core host misbehaves
  → capture, revert nothing, diagnosis is the deliverable"). CompilerTests NOT
  run.
- [x] C: static fallback quote (also exercised at runtime by Stage1/boot).
- [x] D: `git diff --stat` → `tools/msbuild-task/MSBuildTask.cs` only
  (+ untracked progress doc).

## Implementation (DONE)

`tools/msbuild-task/MSBuildTask.cs`, 33 insertions / 1 deletion, BOM+CRLF
preserved:

- New `CoreToolName(toolName)`: maps `"ncc.exe"` → `"ncc-core.exe"`,
  everything else (incl. MONO `ncc.bat`/`ncc`) → `null` (no probe).
- `FindExecutable`: probes the core variant **first** in the same two
  candidate locations (CompilerPath, then task assembly dir); the legacy
  resolution (`ncc.exe`, bare name) below it is byte-for-byte unchanged.
- `GenerateFullPathToTool`: one `Log.LogMessage(..., MessageImportance.Normal)`
  (`Using Nemerle compiler: <path>`) because ToolTask's own tool-path header is
  detailed-verbosity-only — this is the sanctioned extra evidence line.

## A — task rebuild + Stage1 (DONE, exit 0)

- `cmd /c "Build-core-base.cmd" Debug Build Stage1` → **exit 0**,
  0 Warning(s) 0 Error(s) (`.tmp\phase34a-logs\stage1-build.log`). The NTasks
  target recompiled `Nemerle.MSBuild.Tasks.csproj` (csc, netstandard2.0) and
  deployed the new dll to `boot\` + `Stage1\` (`CoreToolName` present in both).
- Incremental `Build` skips unchanged stage compiles (Nemerle targets'
  CoreCompile Inputs effectively ignore stage refs), so evidence was taken
  from forced runs at /v:n (`stage1-vn.log`, `stage3-rebuild-validate-vn.log`):
  `Using Nemerle compiler:` lines show **Stage1 = `boot\ncc.exe`** (the
  `boot-dnlib` seed has no apphost → legacy fallback branch, runtime proof of
  acceptance C) and **Stage2 = `Stage1\ncc-core.exe`**,
  **Stage3 = `Stage2\ncc-core.exe`** — Core host preference active.

## B — PARITY FAILED AT Validate: fixed point 5/6 (STOP)

Full `Rebuild Stage3,Validate` at /v:n (the `Build` form skips everything
incremental). Stage2 and Stage3 fully recompiled on the Core host, all
compiles clean. Validate: **5 of 6 FC pairs identical** (incl.
`Nemerle.MSBuild.Tasks.dll`, `Nemerle.Compiler.dll`, `ncc-core.dll`);
`Nemerle.Macros.dll` differs ⇒ `error MSB3073` on the `fc` Exec
(`NemerleAll.nproj:351`); pefverify (`:357`, known-red) never ran because
`fc` aborted the target first.

Diagnosis — **not a functional failure; per-process string-hash randomization
on the Core host**:

- The only FC diff hunks are the generated macro-operator type names
  `Nemerle.*._N_operator<HASH>_<counter>Macro` (14 names; each counter appears
  twice, once per stage, with different `<HASH>`; attribute blob length 33 vs
  31 follows). Same types, same tmpname counters — only the digits differ.
- Producer: `ncc/frontend/hierarchy/MacroClassGen.n:724` —
  `Util.tmpname ($"operator$(x.GetHashCode())")` for operator names that
  aren't valid identifiers. `x.GetHashCode()` is `String.GetHashCode`.
- .NET Framework `String.GetHashCode` is deterministic across processes;
  .NET Core 3.0+ randomizes it per process. Runtime proof on this machine:
  `powershell.exe` (FX): `1214616415` twice; `pwsh` (Core): `-237791282` vs
  `1832480837`.
- Hence two separate `ncc-core.exe` processes (Stage1→Stage2 and
  Stage2→Stage3 compiles) embed different names; the FC gate compared raw IL
  text. Under the old Framework `ncc.exe` host the hash matched across
  processes, which is why the pre-3.4a chain reported FC 6/6.

Options for the orchestrator (NOT done here): make the tmpname hash stable in
compiler source (e.g. deterministic FNV-1a of the string) — a compiler fix
belonging to 3.5 per report row ordering; or normalize `_N_operator\d+` in
Validate like the 2.5 IL-diff did. Both dlls are functionally identical.

## C — fallback (DONE, static + runtime)

Code (`MSBuildTask.cs`): probes `ncc-core.exe` only when
`CoreToolName(toolName) != null` and the file exists; otherwise the untouched
legacy path returns `ncc.exe`:

```csharp
var path = Path.Combine(CompilerPath, toolName);
if (File.Exists(path))
    return path;
...
var ncc_file = Path.Combine(my_dir, toolName);
if (File.Exists(ncc_file))
{
    // The tool found in the same folder.
    return ncc_file;
}
```

Runtime corroboration: `boot\` has no `ncc-core.exe` ⇒ Stage1 resolved to
`boot\ncc.exe` exactly as before (log lines 295–1429 of stage1-vn.log).

## D — diff scope (DONE)

`git diff --stat`: `tools/msbuild-task/MSBuildTask.cs | 34 +++++++++++++++...`
(33 insertions, 1 deletion) — only file modified; progress doc untracked;
pre-existing untracked files untouched. NOT committed (per spec).

## Logs

- `.tmp\phase34a-logs\stage1-build.log` — A, exit 0.
- `.tmp\phase34a-logs\stage1-vn.log` — Stage1 Rebuild /v:n (boot\ncc.exe lines).
- `.tmp\phase34a-logs\stage3-validate.log` — B as-specified `Build` form
  (incremental no-op compile, FC 6/6 on stale pre-3.4a dumps, pefverify red).
- `.tmp\phase34a-logs\stage3-rebuild-validate-vn.log` — B real chain at /v:n:
  Core-host lines + FC 5/6 + fc MSB3073 (the deliverable diagnosis).

