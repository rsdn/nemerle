# Progress 7.3 — docs: AGENTS.md / README / baseline doc under dotnet + net10

Branch `retarget-compiler`. Plan: 7.3 «Обновить AGENTS.md/README под dotnet+net10».
Docs-only atom: **no builds were run** (all canonical commands are quoted from
`doc/NET10-BUILD-BASELINE.md`, `.github/workflows/build.yml` and the phase
progress docs; the gates were last green in 7.1). Do NOT commit.

## Audit (verified against the repo before writing)

| Fact written into docs | Evidence |
|---|---|
| Canonical entry `build.ps1` (+ bash twin `build.sh`); vswhere-discovered classic MSBuild ≥ 18; `global.json` sdk 10.0.402 | `build.ps1` param block (`-Targets/-Rebuild/-MsBuildExe/-UseDotNetMsBuild/-DryRun`), `global.json` |
| VS2017 no longer required; old "VS2017 from registry or nothing" text obsolete | `Build-core-base.cmd` is a thin `build.ps1` wrapper: "no VsDevCmd, no VS registry" |
| `-UseDotNetMsBuild` exists but classic MSBuild Full is the default (dotnet msbuild hits MSB4803 `GetFrameworkSdkPath` in InitTools) | `build.ps1` header + `build.sh` header |
| `Build-core{,-2-stage,-with-tests}.cmd` kept as shims; `Build-core-with-ide-plug-in.cmd` deleted | `git ls-files Build-*.cmd` (4 files) + progress7.1 deletion list |
| Seed = `boot-net10/` (framework-dependent .NET 10, `ncc-core`); `NRootBoot` points there; `boot-dnlib/` = emergency fallback until 7.4; `boot-4.0/` stays the master seed | `NemerleAll.nproj:27` `<NRootBoot>$(NRoot)\boot-net10</NRootBoot>`, checklist 4.2–4.6/7.2, progress7.1 item 3 |
| Working compiler is `ncc-core.exe` (net10 apphost); FX `ncc.exe` host gone from the graph | 3.4b, `NemerleAll.nproj:72-73` ("net10 ncc-core.nproj is the only compiler host now") |
| Validate = ildasm + `fc` IL-diff ×5 assemblies; pefverify retired in 5.1 | progress5.1, `build.yml` build job comment |
| Harness `Nemerle.Compiler.Test` net10; Core norm 461 passed + 7 not-run / negatives 167; default refs from the compiler (6.2b); test exes via `dotnet exec` + generated runtimeconfig (`-r dotnet -rp exec`, 6.2c); `// NO-CORE` (6.2d); `BEGIN-OUTPUT-CORE` (6.4); `-known-failures` default-off, CI uses it with the now-empty allowlist | `Nemerle.Compiler.Test.nproj:16` (net10.0), `NemerleAll.nproj` CompilerTests Exec (line 416/418, `-r dotnet -rp exec`, `NKnownFailuresArg`), `testsuite/ci-known-failures-64.txt` (comments-only), checklist 6.2–6.4 |
| CI = GitHub Actions `build.yml` (build + tests jobs, the two canonical commands) | `.github/workflows/build.yml` |
| Packaging: `./build.ps1 -Targets _NugetPack` (`dotnet pack` shim, `tools\net10.0` + `lib\netstandard2.0`) | progress6.5, `NemerleAll.nproj` `_NugetPack` (revived) |
| Libraries stay netstandard2.0; only the toolchain is net10 (D2/D6) | checklist 3.2/6.5, `Directory.Build.props` comment |
| `global.json` + `Directory.Build.props` + `nuget.config` now EXIST (old AGENTS gotcha said the opposite) | files at repo root, progress5.3 |
| Full build still Windows-only (`build.sh` refuses off Windows); Mono/xbuild path dead (`NemerleAll-Mono.nproj` not in `git ls-files`) | `build.sh` header, git ls-files audit |

## Files changed

1. **`AGENTS.md`** — rewrote Build / Bootstrapping / Tests / Gotchas for the
   net10 reality; "Two branches" retarget bullet now says .NET 10 host +
   toolchain, dnlib backend, `boot-net10` seed; cross-branch sync section
   semantics untouched (only ".NET Core" → ".NET 10" precision in the
   divergent-runtime bullet); `master` description (FX + VS2017) kept as-is —
   it is accurate for `master`.
2. **`README.md`** — quick sample uses `ncc-core.exe`; Windows build section →
   `build.ps1` (installer/IDE targets are `master`-only on this branch); Linux
   section states honestly that full builds are Windows-only and the
   Mono/xbuild path is gone; IDE + repository-structure sections annotated.
3. **`README`** (tarball readme) — same: build-from-sources → `build.ps1`,
   seed folder `boot-net10/`, Mono/xbuild commands removed.
4. **`doc/NET10-BUILD-BASELINE.md`** — added `-Targets _NugetPack` to the
   canonical commands; fixed the stale "InitTools retirement lands with 6.3"
   and the 5.5 paragraph's `_NugetPack`-is-blocked statements (revived in 6.5;
   `DevBuild*` now blocked at the 5.5 `_Integration` guard); appended a 6.5/7.1
   re-verification note (packages produced, 7.1 deletions, current test norm
   461+7nr/167). Historical baseline/peverify paragraphs kept as the record.

## Not touched (audited, intentional)

- `NemerleAll.nproj`, `build.ps1`, `ncc/`, `lib/`, `macros/`, `snippets/`,
  `testsuite/` — out of scope for a docs atom.
- `configure` (`peverify=` autoconf var) and the Mono/NAnt make path — dead but
  intentionally kept until their own atom (progress7.1 item 5).
- `doc/plans/**` progress/report/checklist files mentioning deleted scripts —
  historical records, not developer-facing build docs.

## Acceptance

- A: `git diff --stat` — doc files only (AGENTS.md, README, README.md,
  doc/NET10-BUILD-BASELINE.md + this new progress doc).
- B: every command written exists verbatim in `build.ps1`/baseline/CI; the
  deleted-script names appear only in historical plans files after the edits
  (grep output quoted in the final report).
- C: `master` paragraph of the two-branches section left accurate for the FX /
  VS2017 line.

Status: DONE (not committed, per instruction)
