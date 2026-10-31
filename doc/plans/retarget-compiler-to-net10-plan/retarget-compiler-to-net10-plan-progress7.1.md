# Progress 7.1 — cleanup: VS2017 scripts / registry hacks / dead Framework targets

Branch `retarget-compiler`. Plan: 7.1 «Удалить VS2017-скрипты/реестровые хаки и
мёртвые Framework-таргеты» + NB notes from checklist 3.4b / 3.2. Do NOT commit.

Canonical entry is `build.ps1` (Phase 5.2, vswhere-located; CI runs exactly
`./build.ps1 -Targets Stage3,Validate -Rebuild` and `./build.ps1 -Targets
Stage1,CompilerTests`). The VS2017 *registry* hack itself was already removed
from `Build-core-base.cmd` in Phase 1 and the file became a thin shim over
`build.ps1` in Phase 5.2 (audited again here: no registry/VsDevCmd reads left).
`NemerleAll.nproj` was already purged of gacutil/ngen/peverify/al.exe probes in
5.1 (grep confirms zero leftovers; the surviving `snippets/VS2010` strings are
the deliberate 5.5 `_GuardVsIntegration` design — do-not-touch).

## Decision table

Legend: **DEL** = `git rm`; **KEEP** = untouched; **EDIT** = targeted edit.

### Item 1 — dead ncc host project files (NB 3.4b)

| File | Referenced by (audit) | Decision |
|---|---|---|
| `ncc.nproj` (net461 ncc.exe host) | `NemerleAll.nproj`: comment only (3.4b removed the item); `ncc32/ncc64.nproj` (import, also deleted); `Nemerle-2005/2008/2010/2012.sln` project entries (stripped, see below). Out of the stage graph since 3.4b. | **DEL** |
| `ncc\App.config` | sole consumer `ncc.nproj:39` (`<None Include>`); `boot-dnlib\ncc.exe.config` comment *mentions* it (seed content, not touched); `NemerleAll.nproj:232` comment pointer (updated). Core host needs no app.config — runtimeconfig/deps carry the seed config (4.2). | **DEL** |
| `ncc32.nproj` | grep: only itself + `Reg-bins-2.cmd` ngen lines (deleted below) + WiX `CompilerAndTools.wxs` (guarded installer, see KEEP list). Not in NemerleAll graph (3.4b row 7). | **DEL** |
| `ncc64.nproj` | same as ncc32.nproj | **DEL** |
| `Nemerle-2005/2008/2010/2012.sln` | legacy VS2005–2012 dev solutions, referenced by nothing; other entries (Nemerle/Nemerle.Compiler/Nemerle.Macros/Linq/MSBuild.Tasks) still exist | **EDIT**: strip the 6-line `ncc`/`{0C558E38-…}` blocks (leftover references to the deleted file) |
| `.gitignore` `/ncc.psess`, `/ncc190725.vspx` | — | KEEP (task: may stay) |

### Item 2 — `Nemerle.Location/Nemerle.Location.nproj` (NB 3.2, 3.4b)

Not in the `NemerleAll.nproj` build graph, **but** it is a live `ProjectReference`
target of `Nemerle.Macros-dnlib.nproj:56`, `ncc-dnlib.nproj:74` and
`ncc/Nemerle.Compiler-dnlib.nproj:118`, and a project of
`Nemerle.Compiler-dnlib.sln` (the dnlib-line IDE subgraph; removing the .nproj
would break 4 files — retiring that subline is not a 7.1 decision; its `.n`
sources are additionally compiled into Nemerle.Compiler via
`Nemerle.Compiler.nproj: Compile Include="Nemerle.Location\**\*.n"`).

| Item | Decision |
|---|---|
| `Nemerle.Location.nproj` file | **KEEP** — recorded reason above |
| `:30 StartProgram=…\Stage1\ncc.exe` | **EDIT** → `ncc-core.exe` (checklist 3.4b: «перенацелить в Фазе 5/7» — ncc.exe no longer exists in stage dirs) |

### Item 3 — root `.cmd` scripts (18 audited)

| File | What it is | Referenced by | Decision |
|---|---|---|---|
| `build.ps1`, `build.sh` | canonical net10 entry (5.2) | CI, baseline doc | KEEP |
| `Build-core-base.cmd` | thin shim over build.ps1; **no registry hack left** (stripped in 1.1/5.2) | `doc/NET10-BUILD-BASELINE.md` (legacy .cmd entry), the 3 wrappers below | KEEP as-is |
| `Build-core.cmd`, `Build-core-2-stage.cmd`, `Build-core-with-tests.cmd` | 4-line wrappers → base → build.ps1; target sets still live and working | AGENTS.md (rewritten in 7.3); work fine | KEEP (already the «thin shim» option; deleting would break documented working entries before 7.3 rewrites AGENTS.md) |
| `Build-core-with-ide-plug-in.cmd` | `DevBuildQuick` → `_Integration` — hard-`<Error>`-guarded on this branch since 5.5 (VS2017-era VSSDK prerequisites; IDE ships on `master`) — can never complete here | nothing but AGENTS.md | **DEL** (dead entry point) |
| `CleanUp.cmd` | `rmdir bin./obj.` dev helper, not FX-specific | — | KEEP |
| `NemerleAllTest.cmd` | invokes targets `NccFast/NccFull/VSIntegration/Shell/RegProgFiles/RunTests` — **none exist** in NemerleAll.nproj — via `%SystemRoot%\Microsoft.NET\Framework\v3.5\msbuild.exe` | nothing | **DEL** |
| `GacReg.cmd`, `GacReg-4.0.cmd`, `GacReg-4.5.cmd` | gacutil register/unregister of FX GAC install (VS2005/7.0A/8.0A SDK paths) | nothing | **DEL** (gacutil retired from toolchain in 5.1) |
| `Reg-bins.cmd`, `Reg-bins-2.cmd`, `Reg-bins-VS2010/2012/2013/2015.cmd` | FX install: gacutil + ngen + **`reg.exe add HKLM/HKCU\...\VisualStudio\{9..14}.0\MSBuild\SafeImports` registry hacks** + Nemerle.VisualStudio.vsix; ngen ncc.exe/ncc32.exe/ncc64.exe (gone) | nothing | **DEL** (pure-FX «реестровые хаки») |
| `Reg-boot.cmd`, `Reg-boot-4.0.cmd` | ngen of legacy `boot\`/`boot-4.0\` FX seed binaries | nothing | **DEL** (scripts only; seed **folders** untouched — boot-dnlib stays until 7.4, boot-net10 is the seed) |

### Item 4 — NemerleAll.nproj Framework targets

`git grep -i "ngen|gacutil|peverify|vsdevcmd|devenv|vs2017|registry|HKLM|reg.exe|frameworkonly"` →
zero hits except the 5.5 guard comments/errors (`_GuardVsIntegration`,
`_Integration`, `_Installer`, `IdeEngineTests` stub, `Integration*/Installer*/DevBuild*`
composites): intentional, documented (NET10-BUILD-BASELINE §Windows-only) — **KEEP**.
`InitTools` ildasm discovery: still consumed by Validate (`fc` IL-diff, FC ×5 gate);
its own comment schedules its retirement together with a Validate replacement — **KEEP**.
`_Wpf`: deliberate test-fixture edge (5.5) — **KEEP**. No edits to any target.
Two stale **comments** updated: the 3.4b `ncc.nproj` note («kept on disk until
7.1» → deleted) and the `NPrepareBoot *.config` note (pointed at the now-deleted
`ncc\App.config`).

### Item 5 — other leftovers found while grepping

| Found | Decision |
|---|---|
| Mono/NAnt build path: root `Makefile`, `configure`, `Nemerle.build`, `Build-Mono.sh` (xbuild), `lib/Makefile`, `ncc/frontend/Makefile`, `macros/macros.build`, `ncc/ncc.build`, `testsuite/Makefile`+scripts, `tools/*`, `snippets/**/Makefile`, `misc/*.build`, packaging (`misc/packages/{msi,rpm,ebuild,unix-binary}`), `boot/Makefile` | **KEEP + record**: dead since 5.2 in practice, but (a) acceptance E forbids this diff from touching `ncc/frontend`, `lib/`, `macros/` (master-shared dirs), (b) it is a ~70-file mono-path removal entangled with 3.5f («make/NAnt-путь умирает») — belongs to its own atom / Phase 8 review, not to a cleanup gated on the same build. |
| `misc/packages/wix/Sources/CompilerAndTools.wxs` (ncc.exe/ncc32/ncc64 + ngen tasks) | KEEP: only reachable via `_Installer` → `_GuardVsIntegration` (5.5), needs WiX submodules; guarded, not silently dead. |
| `tools/msbuild-task/ncc-mono{,.bat}` (mono `ncc.exe` wrappers) | KEEP: copied by the still-live NTasks flow (`$(NBoot)\ncc[.bat]` for the mono path) — ambiguous, recorded. |
| `snippets/Nemerle.WPF/Nemerle.WPF.nproj:39` StartProgram → `%ProgramFiles(x86)%\Nemerle\ncc.exe` | KEEP: IDE-only F5 setting of the live `_Wpf` fixture (in-scope rule only re-pointed Nemerle.Location, per checklist NB); recorded. |
| `boot-dnlib/ncc.exe.config` comment «see ncc\App.config» | KEEP: seed folder content is do-not-touch; pointer becomes historical, noted here instead. |
| `ncc-core.nproj` comments mentioning the flat-dir FX `ncc.exe` | KEEP: the dnlib/CodeDom asset overwrites are still live (FX test harness binds the same files); comments stay accurate about the flat-dir rationale. |
| `Nemerle.Compiler.nuspec:20`, `NEWS` mentions | KEEP: prose/historical. |

## Execution order

1. `git rm` the 17 files (4 project files + 13 scripts).
2. Edits: 4 × `.sln` (strip `ncc` project), `Nemerle.Location.nproj` StartProgram,
   `NemerleAll.nproj` two comments.
3. Gates: A `Stage3,Validate -Rebuild` (FC ×5, 0 errors, exit 0), then
   B `Stage1,CompilerTests` (461/0/7nr + 167/0), C = A + B are the two CI entry
   commands + `Stage3\ncc-core.exe -version` smoke.
   Logs in `.tmp\phase71\`.

## Results (gates green; NOT committed per instruction)

Executed exactly as tabled: `git rm` ×17 files; edits ×6
(`Nemerle-2005/2008/2010/2012.sln` — ncc project blocks stripped;
`Nemerle.Location.nproj` StartProgram → `Stage1\ncc-core.exe`;
`NemerleAll.nproj` two comments). XML parses OK for both nproj edits; sln files
verified structurally consistent (every remaining project keeps its config
entries; zero `0C558E38`/`ncc.nproj` references left in them).

**A — bootstrap fixed point** (`./build.ps1 -Targets Stage3,Validate -Rebuild`,
log `.tmp\phase71\validate.log`): **exit 0**; `FC: no differences encountered`
**×5** (Nemerle.dll, Nemerle.Compiler.dll, Nemerle.Macros.dll,
Nemerle.MSBuild.Tasks.dll, ncc-core.dll — the post-3.4b FC ×5 norm);
lines matching `: error` → **0**; log ends `Build succeeded.` + `EXITCODE=0`.

**B — compiler regression suite** (`./build.ps1 -Targets Stage1,CompilerTests`,
log `.tmp\phase71\tests.log`): **exit 0**. Harness printed **`All tests
passed.` twice** (positives @ line 1715 `Time taken: 00:03:35.89`, negatives
@ 1906 `00:01:03.42`), plus the build-time `All 15 tests passed.` (Nemerle.Diff)
as usual; `: error` → 0; `Build succeeded.` Per-test accounting parsed from the
log (both `run…passed` and compile-only `…passed` line formats): positive
**468 files = 461 passed + 7 „not a test on core“ (nr) + 0 failed**; negative
**167 = 167 passed + 0 failed** — exact parity with the 6.4/6.5 norm.

**C — CI-proxy:** the two build.yml entry commands *are* gates A and B verbatim
(build.yml `Build Stage1 -> Stage2 -> Stage3 + Validate` step =
`./build.ps1 -Targets Stage3,Validate -Rebuild`; tests job step =
`./build.ps1 -Targets Stage1,CompilerTests` — both green above). Smoke:
`bin\Debug\net-4.0\Stage3\ncc-core.exe -version` →
`Nemerle Compiler (ncc) version 10.0.45.0 (git)` + `Copyright (c) …`,
**exit 0**.

**D — git status (nothing committed):** 17 staged deletions (`git rm`) +
6 modified files + this progress doc (untracked). `git diff HEAD --stat`:
23 files changed, 8 insertions(+), 791 deletions(-). Deleted:
`Build-core-with-ide-plug-in.cmd`, `GacReg.cmd`, `GacReg-4.0.cmd`,
`GacReg-4.5.cmd`, `NemerleAllTest.cmd`, `Reg-bins.cmd`, `Reg-bins-2.cmd`,
`Reg-bins-VS2010.cmd`, `Reg-bins-VS2012.cmd`, `Reg-bins-VS2013.cmd`,
`Reg-bins-VS2015.cmd`, `Reg-boot.cmd`, `Reg-boot-4.0.cmd`, `ncc.nproj`,
`ncc\App.config`, `ncc32.nproj`, `ncc64.nproj`.
Modified: `Nemerle-2005.sln` (−10), `Nemerle-2008/2010/2012.sln` (−6 each),
`Nemerle.Location/Nemerle.Location.nproj` (1 line), `NemerleAll.nproj`
(2 comments). Checklist untouched.

**E — master safety:** zero changed paths under `ncc/frontend/`, `lib/`, or
`macros/` (verified with `git diff HEAD --name-only` filtered — empty). The one
deleted file under `ncc/` is `ncc\App.config`, the branch-local FX-sidecar of
`ncc.nproj` added on this branch (assert-ui work); no language/typing/library
semantics touched, so nothing needs cross-branch porting and `master`'s
Framework line is unaffected.

Residual known-stale pointers (documented above, intentionally kept):
`AGENTS.md` still lists `Build-core-with-ide-plug-in.cmd` and describes the old
VS2017 base.cmd flow → rewritten by 7.3; `boot-dnlib\ncc.exe.config` comment
mentions the deleted `ncc\App.config` (seed folder, do-not-touch); mono/NAnt
make path + WiX/ebuild/rpm packaging keep their ncc.exe references until their
own atom (3.5f-linked, Phase 8 review).
