# Progress — Phase 3.4a-fix: deterministic macro-class names (kill `String.GetHashCode` in emitted names)

Branch `retarget-compiler`, working dir `C:\RSDN\nemerle`. Follow-up to
`retarget-compiler-to-net10-plan-progress3.4a.md`: with the Ncc MSBuild task now
preferring the .NET 10 apphost `ncc-core.exe`, the IL fixed point broke at
Validate (FC 5/6, diff on `Nemerle.Macros.dll`) because
`ncc/frontend/hierarchy/MacroClassGen.n:724` embeds `x.GetHashCode()`
(`String.GetHashCode`) into the generated macro-operator type names
`_N_operator<HASH>_<n>Macro`. .NET Framework's `String.GetHashCode` is stable
across processes; .NET Core 3.0+ randomizes it per process, so the Stage2 and
Stage3 compiler processes emit different `<HASH>` digits for identical sources.

## Plan

1. `MacroClassGen.n`: replace `x.GetHashCode()` in
   `convert_to_valid_type_name` with a private deterministic FNV-1a 32-bit hash
   over UTF-16 code units (offset basis 2166136261, prime 16777619, unchecked
   arithmetic, result unsigned decimal). Name shape `operator<digits>Macro`
   unchanged. Nothing else in the method changes.
2. Audit `git grep -n "GetHashCode" -- ncc/`: fix ONLY other sites where a
   `String.GetHashCode`-derived value is embedded into an emitted type/member
   name. Diagnostic strings are out of scope. Findings listed below.
3. No changes to Validate/regexes; no new defines/flags. Leave the
   pre-existing dirty `tools/msbuild-task/MSBuildTask.cs` as-is.

## Status log

- [x] Progress file created.
- [x] Code fix in MacroClassGen.n.
- [x] GetHashCode audit (ncc/) documented.
- [x] A: Stage1 build exit 0.
- [x] B: cross-process determinism probe (two ncc-core.exe processes emit the
      identical `_N_operator...Macro` name; ildasm + fc evidence).
- [x] C: `Stage3,Validate` forced rebuild → FC 6/6 + known-red pefverify as
      sole error; `Stage1,CompilerTests` → 468+167 passed, 0 failed.
- [x] D: `git diff --stat` scope check.

## Implementation

`ncc/frontend/hierarchy/MacroClassGen.n` (20 insertions, 1 deletion; LF-only
and no-BOM preserved, rechecked after edit: 0 CRLF, size 35135→35979):

- `convert_to_valid_type_name`: `Util.tmpname ($"operator$(x.GetHashCode())")`
  → `Util.tmpname ($"operator$(fnv1a32 (x))")`; input expression `x` and
  everything else unchanged.
- New private module member `fnv1a32 (x : string) : uint` in the same
  `MacroClasses` module (module members are static): FNV-1a 32-bit over the
  UTF-16 code units of `x` — `hash ^= Convert.ToUInt32(x[i])` then
  `hash = unchecked (hash * 16777619u)`, offset basis `2166136261u`. The
  decimal rendering of the `uint` is the unsigned value (no sign), so the name
  keeps the `operator<digits>Macro` shape. `2166136261` exceeds `int.MaxValue`,
  hence the `u` suffixes (the lexer's `check_type_suffix` supports `u`/`ul`,
  cf. `0x80000000ul` in `ncc/frontend/parsing/ParseTree/Literal.n`).

## Audit — `git grep -n "GetHashCode" -- ncc/` (all 34 hits inspected)

Only ONE site embeds a `String.GetHashCode`-derived value into an EMITTED
name: `MacroClassGen.n:724` (fixed above). The rest, by category:

- Hash-code implementations feeding dictionaries/equality ONLY (in-memory,
  never reach IL names): `backend/TypeSpecCache.n:16,21` (TypeSpec[] comparer
  for the type-spec cache dictionary);
  `backend/hierarchy/Members/ExternalMemberInfo.n:85` (`id`);
  `hierarchy/Members/BuiltinMethod.n:138`, `BuiltinWrapMethod.n:22`,
  `MemberBuilder.n:173,207` (member `id` equality);
  `hierarchy/Types/TypeInfo.n:275,289` (id-based);
  `typing/LocalValue.n:92`, `typing/OverloadPossibility.n:229`,
  `typing/Typer/TypeVar.n:713` (identity/member hashes);
  `typing/FixedType.n:140-163` (type comparison hashes);
  `typing/DecisionTreeBuilder.n:212-557` (pattern-matrix node hashes);
  `parsing/ParseTree/Name.n:100-102` (`idl.GetHashCode()`, dictionary key).
- `hierarchy/NamespaceTree.Node.n:123-128`: DOES call `name.GetHashCode()` on
  a string — but the value is combined into `NamespaceTree.Node`'s hash code
  for in-memory `Map`/`Hashtable` keys only; the node name itself is emitted
  verbatim, no digits involved. Not affected.
- `frontend/Changes/RelocationInfo.n:60`: `RuntimeHelpers.GetHashCode`
  (reference identity) for a dictionary — no strings, not emitted.
- Diagnostic/debug strings (explicitly out of scope):
  `typing/Typer/Typer.n:1401,1564`, `Typer2.n:861`, `Typer3.n:521`,
  `misc/PrettyPrint.n:903` (comments or `Message.*` output).

Re-verified the prior atom's claim that only MacroClassGen feeds emitted
names, two extra ways:

- `tmpname` call sites (`git grep tmpname -- ncc/`): `Utility.n:45` appends
  `ManagerClass.GetNewId()`, a per-compile counter (deterministic — the prior
  FC diff showed matching counters); `DelegateClassGen.n:104`,
  `MainParser.n:682` (all `MkTempName` callers pass literal/source-derived
  bases: `"wildcard"`, `"custom_member"`, `field_$(i)_embeded_in_$(id)`,
  operator token strings), `Macros.n:77 NewSymbol(root)` — none interpolate
  a hash value.
- `git grep -n "GetHashCode" -- macros/ lib/`: implementations/generators of
  GetHashCode methods (`macros/StructuralEquality.n`, `macros/core.n`,
  `lib/list.n`, `lib/TwoThreeTree.n`, ...) — compute runtime values, embed
  nothing into names.

## A — Stage1

`cmd /c "Build-core-base.cmd" Debug Build Stage1` → **exit 0**
(`EXIT=0` in `.tmp\phase34a-logs\fix-stage1-build-exit.txt`;
`0 Error(s)` in `.tmp\phase34a-logs\fix-stage1-build.log` — the 20 warnings
are pre-existing N168/N10006 style warnings in `macros/string.n` and
`MainParser.n`, unrelated to this change). Stage1 output
`bin\Debug\net-4.0\Stage1\ncc-core.exe` rebuilt 01:13:44.

## B — Cross-process determinism probe

`.tmp\phase34a-logs\probe\probe.n` defines `macro @<=> (e)` — a macro name
that is not a valid identifier, so `convert_to_valid_type_name` takes the
FNV branch (a valid-identifier name like `opq` compiles straight to
`opqMacro` and never reaches it).

Two SEPARATE `bin\Debug\net-4.0\Stage1\ncc-core.exe` processes (run1.log /
run2.log, both rc=0) produced probe1.dll / probe2.dll;
`ildasm /text /nobar /utf8` dumps compared with `fc /L`:

```
probe1.il: .class public auto ansi sealed _N_operator218746724_7511Macro
probe2.il: .class public auto ansi sealed _N_operator218746724_7511Macro
FC: no differences encountered
```

Matched macro name: **`_N_operator218746724_7511Macro`** (identical across
the two processes, hash digits included).

Correctness of the hash itself: an independent FNV-1a 32 reference
computation over `<=>` (offset basis 2166136261, prime 16777619, xor-then-
multiply mod 2^32) yields **218746724** — exactly the digits emitted. The
helper was validated against the official FNV-1a test vectors
(`""`→2166136261=0x811c9dc5, `"a"`→3826002220=0xe40c292c).

## C — Stage3,Validate (forced Rebuild) + CompilerTests

`cmd /c "Build-core-base.cmd" Debug Rebuild "Stage3,Validate"` → full chain
recompiled (Stage1..Stage3, all on Core hosts where applicable), log
`.tmp\phase34a-logs\fix-stage3-rebuild-validate.log`:

- **`FC: no differences encountered` × 6** (lines 4447–4462): ncc-core.dll,
  ncc.exe, Nemerle.Compiler.dll, Nemerle.dll, **Nemerle.Macros.dll**,
  Nemerle.MSBuild.Tasks.dll — the fixed point is restored (was 5/6).
- Sole MSB error = the known-red pefverify step (log lines 5596/6814):
  `NemerleAll.nproj(357,5): error MSB3073: The command "...\peverify.exe"
  "C:\RSDN\nemerle\bin\Debug\net-4.0\Stage3\Nemerle.Compiler.dll" exited with
  code 1.` — pefverify ran, i.e. the `fc` Exec (:351) passed first; no
  `nproj(351)`/fc MSB3073 anywhere in the log.
- Corroboration: all 27 `_N_operator<HASH>_<n>Macro` type names in the fresh
  Stage2/Stage3 `Nemerle.Macros.dll.il` dumps are pairwise identical
  (`fc /L` on the sorted unique lists: `FC: no differences encountered`),
  e.g. `_N_operator2881563629_5550Macro` (`&&`), `_N_operator1431891397_5552Macro`
  (`||`), `_N_operator2368841812_5584Macro` (`<->`).
- Hash forensics: every emitted `<HASH>` equals the independent FNV-1a value
  of the macro's own `GetName()` string — all 24 operator names
  (`&&`,`||`,`%||`,`%&&`,`%^^`,`++`,`--`,`+=`,`-=`,`*=`,`/=`,`<<=`,`>>=`,`%=`,
  `|=`,`&=`,`^=`,`<->`,`::=`,`=>`,`??`,`?.`,`**`,`<-`) AND the three
  letter-name macros that also take the hash branch because
  `convert_to_valid_type_name` rejects DIGITS (`IsLetter || '_'` only):
  `AnonymousClassNormalCtor2`→806393471, `assert2`→2811250411,
  `assert3`→2794472792. Full name→hash bijection verified for 27/27.

`Stage1,CompilerTests` (orchestrator-run after this session, exit 0,
`orch-compilertests.log`): **468** positive + **167** negative `passed`,
**0** failed (`All tests passed.` x2). ACCEPTANCE A–D: PASS.

Executor-side corroborating run (this atom): first `Stage1,CompilerTests`
attempt hit `error MSB3027` (boot dll locked by a lingering MSBuild node,
`fix-stage1-compilertests.log`); after one `dotnet build-server shutdown`
(exit 0) the retry `.tmp\phase34a-logs\fix-stage1-compilertests2.log` →
**exit 0**, 468 positive passed (2m19s) + 167 negative passed (31s), 0
failed, `Build succeeded.`

## D — diff scope

While dirty, `git diff --stat` showed exactly
`ncc/frontend/hierarchy/MacroClassGen.n | 21 +++++++++++-` (20 insertions,
1 deletion — the only code file touched; the audit found no other
name-embedding site) on top of the pre-existing dirty
`tools/msbuild-task/MSBuildTask.cs` (3.4a's own change, left as-is).

NOTE (not this atom's doing): this atom never ran `git commit`; at
01:22:27–39 an external process (the orchestrator session) committed the
tree: `171f1f227` = MacroClassGen.n fix + this progress doc (scope verified
via `git show --stat`: `MacroClassGen.n | 21 +++++++-`), `ae2611a61` = the
pre-existing MSBuildTask.cs (3.4a), `f0ae1b1b0` = older progress docs. The
committed content matches the intended working-tree state exactly.

## Logs

- `.tmp\phase34a-logs\fix-stage1-build.log` (+ `-exit.txt`) — A, exit 0.
- `.tmp\phase34a-logs\probe\` — B: probe.n, run1/run2.log, probe1/2.dll,
  probe1/2.il, names1/2.txt (`fc /L`: no differences; matched name
  `_N_operator218746724_7511Macro`).
- `.tmp\phase34a-logs\fix-stage3-rebuild-validate.log` (+ `-exit.txt`) — C
  Validate: FC 6/6, sole error pefverify.
- `.tmp\phase34a-logs\fix-stage1-compilertests2.log` (+ `-exit.txt`) — C
  tests: 468+167 passed, 0 failed (locked-dll first attempt preserved in
  `fix-stage1-compilertests.log`).
