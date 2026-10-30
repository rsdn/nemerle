# Report 3.3a — System.CodeDom: reproduce, root cause, decision (bump vs keep vs remove)

Date: 2026-10-31. Branch `retarget-compiler`. Research-only; no repo sources touched, no builds.
Scratch: `.tmp\phase33a\` (backups, probes, fetched sources). All `bin\` mutations restored (verified below).

## Q1 — Reproduction (full evidence)

Harness dir `bin\Debug\net-4.0\Tests\positive\`, facade `System.CodeDom.dll` = 25,192 B backed up to
`.tmp\phase33a\System.CodeDom.dll.facade.bak`.

1. Baseline (facade): `.\Nemerle.Compiler.Test.exe -output:. "-p:-nowarn:10003 -def:NET_4_0;DEBUG" "C:\RSDN\nemerle\testsuite\positive\codedom.n"`
   → `codedom.n: run.......................................................passed` (`All tests passed.`)
2. Swap: `Copy-Item %USERPROFILE%\.nuget\packages\system.codedom\4.7.0\lib\netstandard2.0\System.CodeDom.dll .` (181,832 B, identity `System.CodeDom, Version=4.0.3.0, PublicKeyToken=cc7b13ffcd2ddd51`). Same recipe →
   `codedom.n: run...failed` / `Test finished with exit code -1073741819` (0xC0000005). No output/log files written by the harness (only the regenerated `codedom.exe`).
3. Complete exception (compiled test run directly, `.\codedom.exe`, cwd = harness dir, full-impl still in place):

   ```
   Unhandled Exception: System.NullReferenceException: Object reference not set to an instance of an object.
      at System.CodeDom.Compiler.CodeGenerator.GenerateCommentStatements(CodeCommentStatementCollection e)
      at System.CodeDom.Compiler.CodeGenerator.GenerateType(CodeTypeDeclaration e)
      at System.CodeDom.Compiler.CodeGenerator.System.CodeDom.Compiler.ICodeGenerator.GenerateCodeFromType(CodeTypeDeclaration e, TextWriter w, CodeGeneratorOptions o)
      at Test.Bug13284()
      at Test.Main()
   EXIT: -1073741819
   ```
4. Restore facade (25,192 B) + re-run recipe → `codedom.n: run.......................................................passed` (`All tests passed. Time taken: 00:00:01.2327773`). Probe binaries removed from the dir; nothing else under `bin\` altered.

Note: 4.7.0 full-impl → NRE; 10.0.14 full-impl (Phase 2 journal, checklist.md:32,:104) → ArgumentNullException in the same method. Same fault, different guard — see Q2.

## Q2 — Root cause: (c) load/context artifact (mixed type identity), NOT an upstream regression

What the test does (`testsuite\positive\codedom.n:13-24`): builds a `CodeTypeDeclaration("Foo")` (IsEnum) with a
`CodeMemberField` + `CodeAttributeDeclaration("Baz", array[])` and calls `Nemerle.Compiler.NemerleCodeProvider.GenerateCodeFromType`
— i.e. Nemerle's provider (`ncc\frontend\codedom\NemerleCodeProvider.n:16` `: CodeDomProvider`; `NemerleCodeGenerator.n:31` `: CodeGenerator`),
whose **base class comes from the System.CodeDom assembly** that Nemerle.Compiler.dll references:
`Nemerle.Compiler.dll → REF: System.CodeDom, Version=4.0.3.0`. But the freshly compiled `codedom.exe` assembly refs are only:

```
mscorlib, Version=4.0.0.0   System, Version=4.0.0.0   Nemerle.Compiler, Version=10.0.19.0
```

— no System.CodeDom ref at all: the harness's in-process compile resolves the test's `CodeTypeDeclaration`,
`CodeGeneratorOptions` etc. to the **in-box FX types in GAC System.dll** (a default reference), not to the package.
This is independent of which System.CodeDom.dll sits in the dir (refs were identical with the full-impl present).

Empirical 2x2 (probes in `.tmp\phase33a\probe\`, compiled with FX csc, run from harness dir):

| caller-side CodeDom types | System.CodeDom.dll in dir | result |
|---|---|---|
| in-box (as codedom.exe) | 4.7.0 **facade** (net461) | **SUCCESS** — probe prints `param codeType: CodeTypeDeclaration @System v4.0.0.0 [in-box]`, `... == test type? SAME`, generates `public enum Foo {}` (facade type-forwards everything to in-box → one unified type world) |
| in-box (as codedom.n) | 4.7.0 **full-impl** | **FAIL** — harness NRE above; reflection probe: base chain `NemerleCodeProvider → CodeDomProvider @System.CodeDom v4.0.3.0 (appdir)`, `... == test type? DIFFERENT`, `System.ArgumentException: Object of type 'System.CodeDom.CodeTypeDeclaration' cannot be converted to type 'System.CodeDom.CodeTypeDeclaration'` |
| package (Probe2, extern-alias bound to full-impl) | 4.7.0 full-impl | **SUCCESS**: `test-side CodeTypeDeclaration loaded from: ...\System.CodeDom.dll` + `SUCCESS (package-typed test side): [public enum Foo |{| [Baz]| Bar|}|]` |

Source-level confirmation that the full impl itself is fine and `e` cannot legitimately be null:
4.7.0 (= corefx v3.1.0) `CodeGenerator.GenerateType` calls `GenerateCommentStatements(e.Comments)` (fetched `CodeGenerator.corefx31.cs:61`,
method at :1438-1444 is `foreach (CodeCommentStatement comment in e)`), and `CodeTypeMember.Comments` is
`public CodeCommentStatementCollection Comments { get; } = new CodeCommentStatementCollection();` (`CodeTypeMember.corefx31.cs:30`) —
never null **on an instance the package constructed**. So the NRE proves the receiver was an *in-box-constructed*
object executed by package-typed code: identical simple name, different runtime types; the virtual dispatch reaches
the package `CodeGenerator`, which reads `Comments` through its own field layout and gets null → NRE at line 1440.
Current 10.x added `ArgumentNullException.ThrowIfNull(e)` at the top of `GenerateCommentStatements` (source.dot.net
`System.CodeDom/.../CodeGenerator.cs`) — which is exactly why the 10.0.14 bump reported ArgumentNullException
(checklist Deviations 2026-10-30) instead of NRE for the same fault. Web search found no dotnet/runtime issue for this;
consistent with "not upstream": FX in-box (`dotnetframework.org` referencesource `CompMod/System/CodeDOM/Compiler/CodeGenerator.cs`)
has the same unguarded `foreach` and works because the facade unifies both sides onto the in-box types.

**Classification: (c)** — harness/load-context artifact of the flat FX stage dir (mixed in-box vs package identity for the
same type names). The facade is the *only* asset shape that keeps the FX world unified; the full-impl asset splits it.

## Q3 — Decision

Runtime version matrix (who loads what):
- **FX harness / ncc.exe (net461)**: facade (net461 asset) → type-forwards to in-box `System.dll` implementation. This is the only executing CodeDom consumer today.
- **ncc-core.exe (net10)**: never loads/executes CodeDom (report3.1c-3.1d Q1(c): refs lazy; `main.n` has no CodeDom refs); the flat-dir overwrite (`ncc-core.nproj:58-68`) keeps the facade in the shared stage dir regardless.
- **Downstream net10 consumers of `Nemerle.Compiler.nupkg`** (`NemerleAll.nproj:560-561` pack `Nemerle.nuspec`/`Nemerle.Compiler.nuspec`): restore pulls the pinned package's net10/netstandard2.0 **full-impl** asset as the single identity for both their code and Nemerle.Compiler — unified world, works (row 3 of the matrix). The 3.1c regression is flat-FX-dir-specific and does **not** affect them; pinning 4.7.0 only ages them, it does not break them.

Options:
- **(i) Bump to current — infeasible for the FX line.** Current stable is 10.0.14 (NuGet index; 11.0 previews exist). Packages ≥ 7.0.0 dropped `lib/net461` (only net462), so the net461 projects fall back to the netstandard2.0 full-impl → reproduces Q1 for any "актуальный" version; `ncc-core.nproj:66` (`$(PkgSystem_CodeDom)\lib\net461\...`) would also break outright. No config can re-unify package identity with in-box identity on .NET Framework (there is no in-box assembly named `System.CodeDom` to redirect to); "fixing" it means changing how the Nemerle harness resolves test references — out of proportion. (6.0.0 is the last version shipping a net461 facade; bumping only to 6.0.0 satisfies neither "актуальный" nor any need.)
- **(ii) KEEP 4.7.0 + document deviation — RECOMMENDED.** The facade is what actually runs on the only live CodeDom host (FX harness via forwarding); ncc-core never executes it; downstream consumers get a correct (if old) package and are unaffected by this artifact. The plan's "актуальный" (plan lines 122, 194) is unachievable for the net461 target by design of the package (≥7.0.0 has no net461 asset).
- **(iii) Remove the CodeDom surface — right long-term, defer.** Plan table line 133 (`ncc/frontend/codedom/*`, M). Touches: `ncc/frontend/codedom/` (5 files: Provider, Generator, Compiler, MemberAttributeConverter, test.xml), consumer `testsuite/positive/codedom.n` (delete), `tools/snippetcompiler-addin/src/NemerleLanguage.n:51` (legacy), VS-IDE consumers under `snippets/VS2010/` (IDE integration is master-branch functionality). No `ncc` CLI option uses it (no `codegen` refs in `ncc/main.n`). Removal also clears AOT blocker 3.7(в) and item 6.4; schedule with the Phase 4/6 surface cleanup, not as a blocker for 3.x.

**Recommendation: (ii) now, (iii) as a later atom.** 3.3 closes as "keep 4.7.0 pin (deviation documented)", CodeDom removal filed against Phase 6.

## Q4 — Spec for atom 3.3b (option ii)

Edits (all comment/doc-level; zero code or version change):
1. `Nemerle.Compiler.nproj:40` — keep `<PackageReference Include="System.CodeDom" Version="4.7.0" />`; add an XML comment above it: `<!-- pinned: 7.0.0+ dropped lib/net461 facade; net461 stage must carry the forwarding facade (mixed in-box/package identity breaks codedom.n — see report3.3a). Bump only together with removing ncc/frontend/codedom. -->`
2. `ncc-core.nproj:56` — keep `Version="4.7.0"` (must stay equal to Nemerle.Compiler.nproj; existing comment at :49-55 stays accurate).
3. `doc/plans/retarget-compiler-to-net10-checklist.md` — mark 3.3a done; add Deviations line: "3.3 System.CodeDom stays 4.7.0 — 'актуальный' impossible on net461 (7.0.0+ has no net461 facade; full-impl splits type identity vs in-box System.dll and breaks codedom.n). Root cause + matrix: report3.3a. Closure path: CodeDom surface removal (plan line 133) in Phase 6." Update plan table line 133 action cell to "keep 4.7.0; remove surface (Phase 6)" and note 194 acceptance "System.CodeDom актуален" as deviation-approved.

Acceptance (run by 3.3b executor; no gate should change vs today):
- `cmd /c "`"C:\RSDN\nemerle\Build-core-base.cmd`" Debug Build "Stage3,Validate""` → FC ×5 + sole known-red pefverify MSB3073.
- `cmd /c "`"C:\RSDN\nemerle\Build-core-base.cmd`" Debug Build "Stage1,CompilerTests""` → 468+167/0 (codedom.n included).
- Sanity: `bin\Debug\net-4.0\Stage1\System.CodeDom.dll` size 25,192 B (facade overwrite, 3.1c mechanism intact).

Fallback: if CompilerTests shows codedom.n failing with the facade present, the 3.1c Q1(a) AfterTargets overwrite silently stopped working (e.g. restore graph change) — re-apply/verify the `ncc-core.nproj:58-68` target before touching versions; do **not** attempt to "fix" codedom.n by switching the copied asset to the full-impl (Q1 reproduction proves it is the wrong asset for FX).

## Cleanup verification

- `System.CodeDom.dll` in `Tests\positive` restored to 25,192 B and codedom.n re-passed after restore (quote in Q1.4).
- `Probe*.exe`, `Probe2*.exe` deleted from the harness dir; harness-regenerated `codedom.exe` is normal output of every recipe run.
- `git status --porcelain` at close: only the pre-existing `M doc/plans/retarget-compiler-to-net10-checklist.md`, `?? AGENTS.md` (both from the tasking session, untouched) + this new report file.
