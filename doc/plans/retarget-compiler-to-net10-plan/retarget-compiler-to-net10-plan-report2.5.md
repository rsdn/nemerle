# Report — Phase 2.5 research: dnlib 3.3.0 → 4.5.0 bump (no code changed, no builds run)

Scope: plan items 2.5.1–2.5.7 (`doc/plans/retarget-compiler-to-net10-plan.md:107-115`).
Branch `retarget-compiler`. Paths relative to repo root.

**Method.** Code reading plus authoritative evidence instead of changelog archaeology:
(i) downloaded `dnlib.3.3.0.nupkg`/`dnlib.4.5.0.nupkg`, dumped via reflection the full public
surface (incl. inherited/interface members) of the exact asset `ncc` compiles against —
`lib/netstandard2.0/dnlib.dll`, per `Nemerle.Compiler.nproj:19` — and member-diffed it;
(ii) machine-checked all 127 repo-used members against 4.5.0; (iii) source-diffed tags
v3.3.0…v4.5.0; (iv) release notes + README v4.5.0. Scratch: `%TEMP%\opencode\dnlib450\`.

## Q1. dnlib API surface used by the compiler

`lib/`, `macros/`: zero dnlib (git grep); `snippets/`: binary match only. Live usage is all in
`ncc/backend/**` plus `ncc/frontend/ManagerClass.n:564`, through the single live reference
`Nemerle.Compiler.nproj:39` (plan's "line 37" is actually 39). Namespaces in use:

| dnlib namespace | What is used | Where |
|---|---|---|
| `dnlib.DotNet` | ModuleDef/`ModuleDefMD.Load`(IPEImage+opts), AssemblyDef/AssemblyRef, TypeDef/TypeRef/TypeSpec, FieldDef, MethodDef/MemberRef/MethodSpec, Event/PropertyDef, GenericParam, ImplMap+`PInvokeAttributes`, CustomAttribute/CAArgument, `*Attributes` enums, SigComparer/TypeEqualityComparer, `ToTypeSig`/`ToTypeDefOrRef`, ModuleContext/AssemblyResolver/`AddToCache`, StrongNameKey, `module.Resources` | TypesManager.n:113-131,517-520; LibraryReference.n:36-68,152-154; LibraryReferenceManager.n:52-57,393-398,518; TypeSpecCache.n:18,30; AttributeCompilerClass.n:139,225,487; ManagerClass.n:562-572 |
| `dnlib.DotNet.Emit` | CilBody + `CreateBody()`, Instruction, Local, ExceptionHandler/Block, PdbScope/PdbLocal | ILEmitter.n:132,2293-2435,2550-2602 |
| `dnlib.DotNet.Pdb` | PdbState (`UserEntryPoint`), PdbDocument + reflection into private `PdbScope.customDebugInfos` | MethodBuilder.n:72-73,265; TypeBuilder.n:105; TypesManager.n:410-441 (hack at :434) |
| `dnlib.DotNet.Writer` | ModuleWriterOptions (`WritePdb`/`PdbFileName`/`PdbStream`/`MetadataOptions`/`PEHeadersOptions`/`Cor20HeaderOptions`), `InitializeStrongNameSigning` | TypesManager.n:275-296 |
| `dnlib.DotNet.MD` / `dnlib.PE` | `module.Metadata` table reads; `PEImage.Create` | LibraryReference.n:36-68 |

Dead legacy refs (not in the `NemerleAll.nproj:58-62` build set): `ncc/packages.config:3` (3.2.0),
`ncc/Nemerle.Compiler-dnlib.nproj:42-45` HintPath, `ncc/Nemerle.Compiler.csproj`,
`ExternalDependences/dnlib.dll`. Only `Nemerle.Compiler.nproj:39` matters.

## Q2. Per-group status 3.3.0 → 4.5.0

Member diff of the two netstandard2.0 assets: **11 removals total**, all in
`dnlib.DotNet.Resources.*` (ResourceElementSet ctor/`CreateSerialized`/`WriteData`,
`ResourceDataCreator` delegate, `ResourceData.Output`/`GetWriter`) plus
`dnlib.DotNet.Writer.ByteArrayChunk..ctor(Byte[])` — zero uses in `ncc/` (git grep).
Checker over 127 repo-used members: **0 broken**. Enum diff: only
`GenericParamAttributes.SpecialConstraintMask` 0x1C→0x3C (new `AllowByRefLike`), unused in repo.
Plan 2.5.3's premise ("MetadataOptions extraction / writer-options restructure") is **false** —
those members already lived on `ModuleWriterOptionsBase` in 3.3.0; 2.5.2's "Importer/Resolver
signature changes" do not exist either. Per-group sub-answers:

- **(a) ModuleDef/ModuleDefMD** — identical ctor `(MetadataBase, ModuleCreationOptions)`
  (non-public) and identical public `Load` overload sets incl. `Load(IPEImage, ModuleCreationOptions)`
  (LibraryReference.n:40). No change.
- **(b) Importer** — all ctor overloads take `ModuleDef` in both; repo never instantiates it
  (only a comment mentions it, LibraryReference.n:54). No change.
- **(c) Writer options** — `WritePdb`, `PdbFileName`, `PdbStream`, `MetadataOptions`,
  `PEHeadersOptions`, `Cor20HeaderOptions`, `InitializeStrongNameSigning` all on
  `ModuleWriterOptionsBase` in both versions. No restructure.
- **(d) PEHeadersOptions / Cor20HeaderOptions / NativeModuleWriterOptions** — no renames/removals.
- **(e) dnlib.DotNet.Pdb** — additive only (`PdbDocument.MDToken` getter). Private field
  `customDebugInfos` still exists on `PdbScope` in 4.5.0 → TypesManager.n:434 reflection hack
  survives. `CustomDebugTypeInfo` exists in neither version.
- **(f) AssemblyResolver** — identical property sets in both (DefaultModuleContext,
  EnableFrameworkRedirect, EnableTypeDefCache, FindExactMatch, Pre/PostSearchPaths, UseGAC);
  `AddToCache` (LibraryReference.n:56) unchanged. Note: `EnableTypeDefFinder` and public
  `FileExists` (cited by plan 2.5.4) exist in **neither** version — written against a phantom API.
- **(g) Strong-name signing** — `StrongNameKey` ctors `(Byte[])/(String)/(Stream)/(BinaryReader)`
  and `InitializeStrongNameSigning(ModuleDef, StrongNameKey)` identical; `StrongNameSigner.cs`
  differs by 2 comment lines. No RSA-only migration → TypesManager.n:517-520 compiles unchanged.
  Plan 2.5.5 is wrong twice: API unchanged, and `misc/keys/*.snk` are real 596-byte keys, not stubs.
  `PInvokeAttributes.CallConvStdcall` (AttributeCompilerClass.n:225): present in both.

**Behavior changes (the real risk — not compile errors).** SigComparer.cs 530 lines changed
(4.0 #517, 4.1 #525), TypeSig/TypeSpec/TypeRef fixes (4.0 #479 "wrong TypeSig for TypeDef",
4.1 items), 4.0 #518 field-initial-value ECMA alignment, 3.4.0 InterfaceImpl row sorting,
Importer.cs 193 lines, AssemblyResolver.cs 64, ModuleContext.cs 33. Directly relevant to
`TypeSpecCache.n:18,30` (`SigComparer(0)`) and emitted metadata layout → **Stage2 `Validate`
IL-diff is the acceptance gate, not Stage1 success.** Non-impacting: `GetEntryPoint()` now honors
`Cor20HeaderOptions.EntryPoint` (null in Nemerle); `TablesHeapOptions.Log2Rid` only read when
rewriting an existing ModuleDefMD.

## Q3. LibraryReferenceManager.n reference resolution (plan 2.5.4)

Current mechanics (identical under both dnlib versions):
- `Manager.Init` (LibraryReferenceManager.n:52-57) makes one `ModuleContext` wrapping an
  `AssemblyResolver`; `UseGAC` set at ManagerClass.n:564; each reference opens as
  `PEImage`+`ModuleCreationOptions`+`ModuleDefMD.Load` (LibraryReference.n:36-40), then
  `AddToCache` (:56); deferred binding via `typeRef.Resolve()` (:518).
- Separate from dnlib: `CheckIfAssemblyShouldBeLoaded` (:119-141) and `GetAssembly` (:142-165)
  use **System.Reflection** (`Assembly.Load(new AssemblyName(...))`, `LoadFrom`,
  `Assembly.CodeBase`), plus a Mono-GAC hack (:128-140).

What must change when Nemerle itself runs on net10.0 (host problem, bump-independent):
1. `Assembly.Load("simple-ref-name")` won't bind reference assemblies on Core and
   `Assembly.CodeBase` is null/unsupported → this System.Reflection path must go dnlib-only
   (or be restricted to the default load context).
2. dnlib README v4.5.0: for .NET Core targets disable GAC loading and add Core reference-assembly
   search paths; `AssemblyResolver` probes the Windows GAC only on non-Mono and its per-module
   probe is the module's own base dir. Concretely: `UseGAC=false` on Core hosts and inject the
   `Microsoft.NETCore.App` ref-pack dirs into `PreSearchPaths` — API present in **both** versions.
3. Mono-GAC hack becomes dead code on Core; remove with the port.

dnlib API changes required by 3.3.0→4.5.0 in this file: **none**.

## Q4. PDB writer path and 4.5 requirements

Path: `-debug`→`EmitDebug`, `-pdbtype`→`PdbType` (CompilationOptions.n:421-431); `PdbType` is
never explicitly initialized, so default = enum member 0 = `Windows`. TypesManager.n:275-296 sets
`WritePdb`/`PdbFileName` or `PdbStream` (`CreatePdbStream` :400-408), then `Write(...)`. Scopes/
locals/sequence points come from ILEmitter.n:2293-2435, 2550-2602; Windows-PDB state-machine
marks at MethodBuilder.n:72-73.

- API: nothing on this path changed 3.3.0→4.5.0 (only additive `PdbDocument.MDToken`); the
  `customDebugInfos` reflection hack (TypesManager.n:434) still works — field present in 4.5.0.
- Windows PDB **writing is Windows-only** (README v4.5.0). dnlib prefers `Microsoft.DiaSymReader.Native`,
  falling back to `diasymreader.dll` (.NET-Framework-only, not loadable on .NET Core). Neither is
  referenced in the repo and dnlib doesn't pull either → a net10 host must add
  `<PackageReference Include="Microsoft.DiaSymReader.Native" />` (and ship the native DLL next to
  ncc) or default `-pdbtype` to `portable` — the one genuine Phase 2.5 work item, runtime-only.
- Portable PDB writing is fully managed and OS-independent in both versions. `PdbWriterOptions` exists in both.

## Q5. Bump strategy and exact edits

**Bump straight to 4.5.0; do not step via 3.6.0.** All 11 removals since 3.3.0 are in namespaces
the repo never touches; no version between 3.4.0 and 4.5.0 changed any of the 127 repo-used
signatures (3.6.0's surface already matches 4.5.0 for them), so a stepping build buys two extra
bootstraps and zero information. The genuine risk (Q2) is identical on both paths.

Edit set for a clean compile:
1. `Nemerle.Compiler.nproj:39` — `Version="3.3.0"` → `"4.5.0"`. Sole required edit; expect zero `ncc/**` changes.
2. Optional cleanup (dead, not built): `ncc/packages.config:3`, `ncc/Nemerle.Compiler-dnlib.nproj:43`,
   `ncc/Nemerle.Compiler.csproj`, `ExternalDependences/dnlib.dll`.

Package-asset facts: current closure (`bin/Debug/net-4.0/Stage1/Nemerle.Compiler.deps.json:40-64`)
= dnlib 3.3.0 + SRE packages 4.6.0; 4.5.0's netstandard2.0 asset pulls SRE 4.7.0 (3.6.0 already
did), its `lib/net6.0` asset has **no dependencies**. Host stays .NET Framework ≥4.7.2 (already
implied by `netstandard2.0`), `System.CodeDom 4.7.0` (:40) unaffected.

Bootstrap hazard: seed `boot-dnlib/dnlib.dll` and `ExternalDependences/dnlib.dll` are both 3.3.0.0.
Each stage dir gets its own NuGet dnlib.dll, so stage dirs stay self-consistent, but a seed
`Nemerle.Compiler.dll` (binds 3.3.0.0) must never run against a stage's 4.5.0.0 dnlib.dll —
assemblyVersion differs, no binding redirects exist.

## Summary

1. dnlib is used only under `ncc/backend/**` (+ `ManagerClass.n:564`); `lib/`/`macros/` are clean.
2. API diff of the exact compile asset (netstandard2.0), 3.3.0→4.5.0: 11 removals, none used;
   127 repo-used members checked: 0 broken; one unused enum value changed.
3. Plan premises 2.5.2/2.5.3/2.5.5 (Importer/resolver signature changes, writer-options
   restructure, RSA-only signing migration) are disproven by the API dumps — APIs unchanged.
4. 2.5.4's `EnableTypeDefFinder`/`FileExists` exist in no dnlib version; the real net10 work is
   `PreSearchPaths` injection + retiring the System.Reflection load path — bump-independent.
5. PDB writer path compiles unchanged; a net10 host needs `Microsoft.DiaSymReader.Native`
   added (or portable-PDB default) for the default Windows PDB type.
6. Real risk is silent SigComparer/typeSig/emit behavior changes (4.0 #479/#517/#518/#525,
   3.4.0 InterfaceImpl sorting) → gate the bump on Stage2 `Validate` IL-diff, not Stage1.
7. Recommended action: single edit `Nemerle.Compiler.nproj:39` → 4.5.0, then Stage1→Stage2→IL-diff.
8. **Verdict: clean-bump compile risk LOW; metadata/IL-parity and net10-runtime risk MEDIUM.**
