# Progress 8.3g — `_NugetPack` standalone stage selection by freshness (R2-03)

Status: **DONE** (2026-10-31; gates A-D green, see §Evidence; review2 finding
R2-03: the standalone
`_NugetPack` NCurBin fallback in `NemerleAll.nproj` picked the *highest-numbered*
existing `$(NBin)\Stage{4..1}` directory while its comment claimed "newest built
stage" — stale Stage3/Stage4 dirs from an old run next to a freshly rebuilt
Stage1 made the pack silently ship stale payload).

## Fix

`NemerleAll.nproj`, `_NugetPack` target only. The four
`NCurBin … Exists(StageN)` fallback lines are replaced by a freshness scan:
for each candidate `$(NBin)\Stage{4,3,2,1}\ncc-core.exe` that exists, read
`$([System.IO.File]::GetLastWriteTimeUtc(…).Ticks)` into a scratch property and
keep the candidate whose tick count is strictly greater than the incumbent best
(pure MSBuild property functions, no tasks, no `Exec`). Candidates are scanned
highest-number-first and replacement requires a *strictly newer* timestamp, so
an exact tie still prefers the higher-numbered stage (old tie-break preserved).
When NCurBin is already set (DevBuild*/Installer* composites, or an explicit
`-p:NCurBin=`), the guarded tick reads never run and every comparison
degenerates to `0 > 0` — the selection is a no-op, as before. The friendly
`Error` for "no stage dir has ncc-core.exe" is unchanged. A high-importance
`Message` now prints the chosen stage directory so pack logs show what was
packaged. Comment rewritten to state the actual rule (newest LastWriteTimeUtc,
tie → higher number).

## Scope / constraints

- Only `NemerleAll.nproj` (`_NugetPack` target) + this progress doc. No
  compiler sources touched; `_NugetPack` is outside every default target
  (explicit invocation only, like CompilerTests), so the full Stage3/Validate
  + CompilerTests gate is not required for this change (see Regression below).
- File stays UTF-8 BOM + CRLF (verified before/after edit).

## Evidence

Logs under `.tmp\phase83g\`. Toolchain: MSBuild 18.11.0-1.26475.11 for .NET
Framework via `build.ps1`. Stage timestamps at session start (UTC): Stage1
04:51:57, Stage2 04:52:26, Stage3 04:52:56, Stage4 absent — Stage3 newest.

Note: the first Gate A attempt failed with `MSB4025 ... An XML comment cannot
contain '--'` — the draft comment used `--` as a dash; reworded, XML confirmed
well-formed (`xml.dom.minidom` parse OK) before re-running.

### A. Fresh state — `./build.ps1 -Targets _NugetPack` (`gateA.log`)

Exit 0 (`EXITCODE=0` last line), `Build succeeded`, both nupkgs created
(`Nemerle.10.0.56.nupkg`, `Nemerle.Compiler.10.0.56.nupkg`). New Message
(log line 7):

```
_NugetPack: packaging stage 'C:\RSDN\nemerle\bin\Debug\net-4.0\Stage3'
```

Stage3 is the genuinely newest stage → correct pick (old code also picked
Stage3 here, but only because it is the highest-numbered dir that exists —
Gate B is what discriminates).

### B. Stale scenario via touch, no rebuild

1. `(Get-Item bin\Debug\net-4.0\Stage1\ncc-core.exe).LastWriteTimeUtc =
   [DateTime]::UtcNow` → Stage1 05:02:54 (newest), Stage3 04:52:56.
2. `./build.ps1 -Targets _NugetPack` (`gateB-touch.log`), exit 0, Message:
   `_NugetPack: packaging stage 'C:\RSDN\nemerle\bin\Debug\net-4.0\Stage1'`
   → freshness beats stage number (the old code would have packed Stage3).
3. Restore: touched Stage3's `ncc-core.exe` to UtcNow (05:03:25) and re-ran
   (`gateB-restore.log`), exit 0, Message:
   `_NugetPack: packaging stage 'C:\RSDN\nemerle\bin\Debug\net-4.0\Stage3'`
   → natural order restored and selection follows.

No stage dir was deleted or rebuilt in this gate (timestamps only).

### C. Regression sanity — `./build.ps1 -Targets Stage1` (`gateC-stage1.log`)

Exit 0 (`EXITCODE=0`), tail:

```
  ncc-core -> C:\RSDN\nemerle\bin\Debug\net-4.0\Stage1\ncc-core.dll
Build succeeded. Nemerle installed to 'C:\RSDN\nemerle\bin\Debug\net-4.0' (when the Install target ran).
```

Full Stage3/Validate + CompilerTests gate deliberately NOT re-run: the change
is confined to the `_NugetPack` target of `NemerleAll.nproj`, which is outside
every default target and every stage/Validate/CompilerTests dependency chain
(verified: only callers are InstallerFast/Full, DevBuild* composites, and
explicit invocation) — no compiler source, no project-graph changes; and the
shipped package *content* correctness was verified in 6.5 (nuspec revival) and
7.4 (clean-clone fixed point on the boot-net10 seed). The Gate C build touched
Stage1's `ncc-core.exe` again, so Stage3 was re-touched to restore the
Stage3-newest state; a follow-up standalone pack (`gateD-recheck.log`, exit 0)
confirms `packaging stage '...\Stage3'`.

### D. Error path preserved (extra probe)

`msbuild -t:_NugetPack -p:NVer=net-4.0-83g-missing` (points NBin at a root
with no Stage* dirs): exit 1 with the unchanged friendly error —
`error : _NugetPack needs a built stage: no ...\Stage*\ncc-core.exe exists
and NCurBin is not set. ...` (`gateE-error.log`).

### E. `git diff --stat`

```
 NemerleAll.nproj | 39 ++++++++++++++++++++++++++++++++-------
 1 file changed, 32 insertions(+), 7 deletions(-)
```

plus this untracked progress doc. File stays UTF-8 BOM + CRLF (722 CRLF,
0 bare LF, verified after edit).
