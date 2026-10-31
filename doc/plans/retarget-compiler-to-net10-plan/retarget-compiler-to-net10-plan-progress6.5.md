# 6.5 — NuGet packaging revival (`_NugetPack`, nuspec modernization)

Status: DONE (evidence below)

## Goal

`_NugetPack` was an `<Error>` stub since 5.4 (ExternalDependences\NuGet.exe
removed). `Nemerle.Compiler.nuspec` still globbed `ncc.???`/`ncc??.???` (ghosts
since 3.4b — the host is `ncc-core.exe`, .NET 10) and both nuspec files placed
netstandard2.0 assemblies under the wrong `lib\net\` moniker (checklist 3.2
`nuspec-pack-list`). Revive packaging on modern tooling with no machine feeds.

## Mechanism chosen

`dotnet pack` (SDK 10.0.402, NuGet's **NuspecFile** feature) driven by a tiny
SDK-style shim project *generated at build time* into `$(NObj)\Nuget` (nothing
extra checked in). Probed on this machine before wiring:

* `dotnet pack` of a `NuspecFile`+`NuspecProperties` shim restores offline (the
  shim has no PackageReferences; repo nuget.config `<clear/>`+nuget.org is not
  contacted) and honors `$prop$` substitution — including `$version$` inside
  `<dependency version="$version$"/>`.
* `WriteLinesToFile Lines="…%3b…"` writes a literal `;` on ONE line (probed
  against VS-18 MSBuild.exe: raw `;` from expanded properties splits lines and
  corrupts the XML; `\;` keeps the backslash; `%3b` is correct).
* CLI-side `-p:NuspecProperties=a=1%3bb=2` is unreliable through the dotnet
  driver (property arrives but token substitution silently drops pairs), so
  properties are baked into the generated shim, NOT passed on the command line.

## Package layout (chosen)

| payload | old folder | new folder | why |
|---|---|---|---|
| ncc-core.exe/.dll/.runtimeconfig.json/.deps.json, dnlib.dll, System.CodeDom.dll, Nemerle.dll, Nemerle.Compiler.dll, Nemerle.Macros.dll | (ncc.??? commented out, `lib\net\`) | `tools\net10.0\` | .NET 10 tool closure (same set as the `boot-net10` seed); `tools\` keeps exe out of compile refs |
| Nemerle.Compiler.dll, Nemerle.Macros.dll | `lib\net\` | `lib\netstandard2.0\` | old lib intent, correct TFM moniker (D2/D6: libs stay ns2.0) |
| Nemerle.dll (Nemerle package) + Nemerle.Peg.dll, Nemerle.Diff.dll | `lib\net\` | `lib\netstandard2.0\` | all three target netstandard2.0 |

Not packaged (old nuspec never shipped them): Nemerle.MSBuild.Tasks.dll,
Nemerle.MSBuild.targets, Nemerle.Sdk.props/targets (IDE/installer layer),
.pdb/.xml files.

## Target wiring

* `_NugetPack` (public, still outside default targets): standalone-safe `NCurBin`
  fallback (newest `$(NBin)\Stage{4..1}\ncc-core.exe`), friendly `<Error>` when
  no stage exists, then re-enters this .nproj via `<MSBuild>` with `NCurBin` as
  a GLOBAL property so `_PegAndCSharp`/`TestFramework` see `Nemerle=$(NCurBin)`
  even standalone; delegates to new `_NugetPackCore` (`DependsOnTargets` kept
  from the 5.4 stub).
* Version: unchanged `GetAssemblyIdentity` + regex on `$(NCurBin)\Nemerle.dll`
  (10.0.41.0), passed to both nuspec files as `version=`.
* Output: `$(NBin)\Nuget` (unchanged, `Install` already copies it).

## Evidence (2026-10-31)

### A. Package produced (`bin\Debug\net-4.0\Nuget\`, listed via Expand-Archive to .tmp\phase65\zip)

`Nemerle.Compiler.10.0.41.nupkg` (2 218 691 B):

```
Nemerle.Compiler.nuspec
lib\netstandard2.0\Nemerle.Compiler.dll
lib\netstandard2.0\Nemerle.Macros.dll
tools\net10.0\dnlib.dll
tools\net10.0\ncc-core.deps.json
tools\net10.0\ncc-core.dll
tools\net10.0\ncc-core.exe
tools\net10.0\ncc-core.runtimeconfig.json
tools\net10.0\Nemerle.Compiler.dll
tools\net10.0\Nemerle.dll
tools\net10.0\Nemerle.Macros.dll
tools\net10.0\System.CodeDom.dll
```

`Nemerle.10.0.41.nupkg` (118 044 B):

```
Nemerle.nuspec
lib\netstandard2.0\Nemerle.Diff.dll
lib\netstandard2.0\Nemerle.dll
lib\netstandard2.0\Nemerle.Peg.dll
```

No `ncc.???`/`ncc??.???` ghosts; packed Nemerle.Compiler.nuspec shows substituted
tokens (`<version>10.0.41</version>`, `<dependency id="Nemerle" version="10.0.41" />`).
Only warning: NU5125 `licenseUrl` deprecation (pre-existing metadata, kept as-is).

### B. Clean-consumer smoke (.tmp\phase65\consumer)

Nemerle.Compiler package extracted to `.tmp\phase65\consumer\pkg` (no repo paths):

```
> pkg\tools\net10.0\ncc-core.exe -version        → exit 0
  Nemerle Compiler (ncc) version 10.0.41.0 (git)
> ncc-core.exe -target:exe -out:hello-pkg.exe hello.n   (zero -r flags) → exit 0, hello-pkg.exe produced
```

### C. Target wired + regressions

* `./build.ps1 -Targets _NugetPack` → exit 0 (`.tmp\phase65\pack.log`:
  2× `Successfully created package`, `Build succeeded.`, 0 errors; standalone
  `NCurBin` fallback correctly picked `bin\Debug\net-4.0\Stage3`).
* `./build.ps1 -Targets Stage1` → exit 0 (`.tmp\phase65\stage1.log`,
  `ncc-core -> ...\Stage1\ncc-core.dll`, `Build succeeded.`). Full
  Rebuild+Validate is the next atom.

### D. Diff scope

`git diff --stat`: `Nemerle.Compiler.nuspec` +29/−…, `Nemerle.nuspec`,
`NemerleAll.nproj` only (+ this untracked progress doc). BOM/CRLF preserved
(nproj UTF-8-BOM CRLF, nuspecs UTF-8 no-BOM CRLF). Not committed.

## Notes for the orchestrator

* `_NugetPack` re-enters the .nproj (`<MSBuild>` task, global `NCurBin`) so
  `_PegAndCSharp`/`TestFramework` work standalone; `_NugetPackCore` keeps the
  original `DependsOnTargets`. Default targets untouched.
* The generated shim lives only in `$(NObj)\Nuget` (`Nemerle.Pack.csproj`);
  `bin\Debug\net-4.0\Nuget\*.nupkg` is picked up by `Install` unchanged (line 442).
* Checklist 3.2 `nuspec-pack-list` is now closed together with 6.5.

Status: DONE
