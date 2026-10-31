# Master cherry-pick guidance (`retarget-compiler` → `master`)

Per-commit portability review of the net10 retarget work for the **`master`**
branch (.NET Framework line). Produced for checklist atom 8.3d from review
findings **R1-03**/**R1-04**
(`doc/plans/retarget-compiler-to-net10-plan/retarget-compiler-to-net10-plan-report-review1.md`
+ `...-report-review2.md`).

- **Range reviewed:** `17d3cf47c..HEAD` on `retarget-compiler` — **57 commits
  at the time of writing** (a moving-`HEAD` range; the classification below is
  per-sha and stays valid, but recount with `git log --oneline 17d3cf47c..HEAD`
  — later commits are doc/checklist-only unless their shas appear above).
- **Shared surface** (per `AGENTS.md` "Cross-branch sync"): `ncc/frontend/`,
  `ncc/backend/hierarchy/` + `ncc/backend/typing/` (this checkout's location of
  the shared `hierarchy/`/`typing/`), other shared `ncc/` files, `lib/`,
  `macros/`, `testsuite/`.
- **Intentionally divergent, never port:** the dnlib emit backend, the
  `Nemerle.Location/` abstraction, and the host runtime (.NET Framework on
  master vs .NET 10 here) — same observable semantics, different IL/host.

## Sync rule reminder

`AGENTS.md`: the two branches must keep **compiling the same Nemerle source**,
so any language/semantics fix in the shared surface (frontend lexer/parser,
`hierarchy/`, `typing/`, macro mechanics, public APIs of `lib/`/`macros/`)
belongs on **both** branches (cherry-pick/merge via the `github`/`kl` remotes);
backend- and host-runtime-specific work stays local. Each branch bootstraps
from its own seed, so drift poisons the shared bootstrap source too.

## Mandatory pre-condition (R1-03)

Every gate on `retarget-compiler` since 3.1b is **Core-only** (FC fixed point +
Core CompilerTests). **No FX bootstrap or FX suite run has validated any of
these commits on this branch.** Therefore, before trusting *any* candidate
below on `master`, run on an FX host: master-side **Stage1→Stage2 bootstrap**
plus the **FX CompilerTests suite** with the cherry-pick applied, and record
the run. The reentrancy/ICE arguments for several fixes are comment-reasoned
for FX, not FX-execution-proven.

## Port mechanics — master's pre-split layout (R4-01)

**`master` still has the monolithic pre-split compiler layout.** The
frontend/backend/typing reorganization landed on this branch line *before*
`17d3cf47c` and is out of this review's range, so the files the candidates
touch **do not exist at those paths on master** (verified 2026-10-31 with
`git ls-tree master -- <path>` / `git grep -l <pattern> master -- ncc/`):

| file changed on `retarget-compiler` | target file on `master` | verification probe |
|---|---|---|
| `ncc/backend/hierarchy/InternalTypeClass.n` (`e9741864a`, `5a7dadbf1`(a)) | `ncc/external/InternalTypes.n` — the `InternalTypeClass` class and its `InitSystemTypes` live there (`ncc/passes.n` holds the other `InitSystemTypes` hit) | `git ls-tree master -- ncc/external/InternalTypes.n` → exists; `git grep -l "class InternalTypeClass" master -- ncc/` → `ncc/external/InternalTypes.n` |
| `ncc/backend/hierarchy/LibraryReference.n` (`f35161ae9`) | `ncc/external/LibraryReference.n` — the `LibraryReference` class | `git ls-tree master -- ncc/external/LibraryReference.n` → exists; `git grep -n "class LibraryReference" master -- ncc/` → includes `ncc/external/LibraryReference.n` |
| `ncc/frontend/CompilationOptions.n` (`5a7dadbf1`(b), hunk skipped anyway) | `ncc/CompilationOptions.n` | `git ls-tree master -- ncc/CompilationOptions.n` → exists |
| `ncc/frontend/hierarchy/MacroClassGen.n` (`171f1f227`) | `ncc/hierarchy/MacroClassGen.n` (same filename, no `frontend/` level) | `git ls-tree master -- ncc/hierarchy/MacroClassGen.n` → exists; FNV anchor `Util.tmpname ($"operator$(x.GetHashCode())")` present at line 749 |

**Do NOT `git cherry-pick` the three code candidates** (`e9741864a`,
`5a7dadbf1`, `f35161ae9`). Merge-base simulations (`git merge-tree
--write-tree --merge-base=<sha>^ master <sha>`) end in **modify/delete
conflicts** on the retarget-side files ("deleted in master … version left in
tree"). Resolving the conflict the way git's hint invites — keeping the file —
is the dangerous outcome: master's `Nemerle.Compiler.nproj` uses **explicit
Compile globs** (`ncc\hierarchy\*.n`, `ncc\external\*.n`; single-`*` does not
span directories), so a retarget-layout file left at `ncc/backend/...` is
**never compiled — a silent no-op port with a green FX build**, and the FX
gate below cannot catch it because the reorders are FX-behavior-neutral by
design. Port each hunk **by hand into the monolithic master file** per the
mapping table, then apply the FX gate.

**`171f1f227` (FNV determinism) is the one mechanically cherry-pickable
candidate**: rename detection maps it into `ncc/hierarchy/MacroClassGen.n`
(anchor verified above; merge-tree simulation applied cleanly — only the
progress-doc addition conflicts, cosmetically).

Per-candidate manual port procedure (source → target, then FX gate):

- `e9741864a` — hand-edit the target `ncc/external/InternalTypes.n` (master's
  monolithic home of the `InternalTypeClass` class): move the
  `Void`/`Object`/`ValueType` lookups to the top of `InitSystemTypes` exactly
  as the retarget diff (`git show e9741864a -- ncc/backend/hierarchy/InternalTypeClass.n`) does.
- `5a7dadbf1`(a) — same target file: move the `is_right`/`single` helpers and
  the `Delegate_Combine`/`Remove` lookups after the `InitSystemTypes` chain.
  Hunk (b) targets `ncc/CompilationOptions.n` but is inert on FX — skip it.
- `f35161ae9` — hand-edit the target `ncc/external/LibraryReference.n`
  (master's home of the `LibraryReference` class): keep the control-flow fix
  (guard dropped, split on `'='`) and re-implement the key parse with
  `StrongNameKeyPair` (master has no dnlib), per the row below.
- `171f1f227` — `git cherry-pick` (rename-detected) or hand-apply the FNV hunk
  to `ncc/hierarchy/MacroClassGen.n`.

## Candidates — commits touching the shared surface

11 of the 57 commits touch the shared surface. Five carry language/semantics
changes relevant to master; six are Core-host-only work inside shared files
(NO-PORT, listed for completeness).

| sha | what it changes | master-relevant? | FX validation status | notes |
|---|---|---|---|---|
| `f35161ae9` | `ncc/backend/hierarchy/LibraryReference.n`: repairs the **doubly-dead IVT `PublicKey` branch** (guard required `StrongAssemblyKeyName` empty while `snKey()` opened that very file; inner split on `','` could never match `PublicKey=`) — guard dropped, split on `'='`; `StrongNameKeyPair`→dnlib key parse, `SHA1Managed`→`SHA1.Create()` | **yes — behavior fix** | Core-only validated — run the FX bootstrap + FX suite BEFORE trusting on master (R1-03) | **NOT semantics-neutral on FX (R1-04):** keyed `InternalsVisibleTo` becomes honored on FX too. Strictly permissive (only adds `IsFriend=true` on a real key match). Suite-neutral: no `PublicKey=` IVT anywhere in testsuite (all usages name-only). Label the cherry-pick "behavior fix: keyed IVT now honored". **Adapt code:** master has no dnlib — re-implement `publicKey()` with `StrongNameKeyPair` (works on FX); keep the control-flow fix. **Manual port into `ncc/external/LibraryReference.n` — do not cherry-pick** (modify/delete → silent no-op, see "Port mechanics"). |
| `e9741864a` | `ncc/backend/hierarchy/InternalTypeClass.n` (**InitSystemTypes fix**): move the `Void`/`Object`/`ValueType` lookups to the top of `InitSystemTypes` so reflective primitive lookups can't ICE on `TyCodec.minimizeInterfaces` asserts | yes (defensive reorder) | Core-only validated — needs an **FX bootstrap run** as part of the master port (R1-03) | ICE trigger (generic interfaces on primitives) exists only in modern ref assemblies; not reproducible with FX 4.x mscorlib — but the reorder keeps every relative order and `AddBuiltins` site otherwise intact, so it is safe and keeps the sources in sync. **Manual port into `ncc/external/InternalTypes.n` — do not cherry-pick** (see "Port mechanics"). |
| `5a7dadbf1` | two independent changes: (a) **InternalTypeClass reorder** — `is_right`/`single` helpers + `Delegate_Combine`/`Remove` lookups moved after the `InitSystemTypes` chain they reenter; (b) `ncc/frontend/CompilationOptions.n` host-gated `PdbType` default (Portable on Core, Windows on FX) | **partial** — port (a); (b) is dead code on master | Core-only validated — (a) needs the **FX bootstrap run** (R1-03) | (a) is the same class of fix as `e9741864a`: position-only, FX semantics unchanged ("FX used to reach the old tail assignment without ever hitting that reentrancy"). (b): the FX default is already `Windows`; the gate never fires on master — skip that hunk (or land it inert). **Manual port of (a) into `ncc/external/InternalTypes.n` — do not cherry-pick** (see "Port mechanics"). |
| `171f1f227` | `ncc/frontend/hierarchy/MacroClassGen.n` (**FNV determinism**): deterministic FNV-1a 32 hash instead of `String.GetHashCode()` in generated macro-class names `_N_operator<HASH>_<n>Macro` | yes (determinism hardening) | Core-only validated — run the **FX suite** before trusting on master (R1-03) | On FX `String.GetHashCode` is process-stable today, but only as an implementation detail; FNV keeps the shared source identical on both branches. Ported FX builds get different (still opaque, compiler-generated) temp-name digits — no language surface changes; expected zero suite impact, still run it. **Mechanically cherry-pickable** — rename-detected into `ncc/hierarchy/MacroClassGen.n` (see "Port mechanics"). |
| `1231f73af` | testsuite data: (a) fixture rename `Issue-git-0297.cs` → `Issue-git-0297-cs.cs` (content-identical `git mv`); (b) seven `BEGIN-OUTPUT-CORE`/`END-OUTPUT-CORE` expectation blocks in `assert.n`, `notnullorempty.n`, `Issue-git-0053.n`, `Issue-git-0274.n`, `basic-value-types.n`, `overloading.n`, `linq-2-ExprTree.n` | **partial / conditional** | n/a — the drifts are Core runtime behavior (exception-message format, `Expression.ToString`, IEEE-shortest `ToString`); FX expectations untouched | (a) matters to master **only if master ever adopts the same-basename pair**: with `Issue-git-0297.n` and `Issue-git-0297.cs` side by side, both emit assembly name `Issue-git-0297` and on Core the fixture's `.dll` shadowed the test `.exe` under simple-name binding — the FX/AppDomain loader never hit this. The rename is harmless to master (name referenced nowhere). (b) is master-safe (markers anchored `^...$`, FX harness ignores them) but pointless on master alone; needed only if the test files are ever unified. |
| `9ecc5c8a2` | `// NO-CORE` marker: harness parsing (`snippets/…/NccTest*.n`) + marker comments added to 7 FX-only-surface tests (`form.n`, `access-checks.n`, `security.n`, `Issue-git-0507.n`, `codedom.n`, `resource.n`, `serialize.n`) | no | FX path unchanged **by design** — FX hosts ignore the marker entirely | All tests keep running on master (they are FX-*only* content). The marker is an inert comment for master's harness; do **not** port the harness plumbing (net10, see NO-PORT). Relevant only to unified test data. |
| `544a17cdf` | `ncc/frontend/ManagerClass.n` (6.2b): default framework-reference auto-load loop on Core hosts, behind a `System.Private.CoreLib` host probe | no | FX gate false → **byte-identical FX behavior** | Host-gated Core branch: the added loop is dead code on master (FX keeps its `-r`/SDK reference story). NO-PORT. |
| `da2b5cdee` | `ncc/frontend/ManagerClass.n` (8.3f, R2-01/02): warn on unloadable default framework refs + precedence-note comment | no | all lines inside the same CoreLib gate → FX byte-identical | Follow-up inside `544a17cdf`'s host-gated block. NO-PORT. |
| `8561fda03` | `macros/Resource.n` (3.5e): explicit "Framework-only" error for the resx `Resource` macro on Core hosts | no | .NET Framework path untouched (FX still reads resx end-to-end) | Host-gated diagnostic; the full Core fix is deferred (checklist 7.5). NO-PORT. |
| `252bc791d` | `ncc/backend/hierarchy/LibraryReferenceManager.n` (3.5c): Core-safe `GetAssemblyLocation` helper + `TryLoadFromLibPath` (`-lib` macro-load fallback with swallowed probe failures) | no | FX behavior preserved (`CodeBase` non-null on FX → identical result; probe wording verified identical on boot FX `ncc.exe`) | Defensive Core-host fix in a shared file; master never runs on a Core host. NO-PORT (host-runtime category). |
| `2e031d283` | `ncc/main.n` (3.5a): `Uri(Assembly.CodeBase)` → `AppContext.BaseDirectory` fallback at startup | no | Core-host startup crash fix; `CodeBase` works on FX | Host-runtime fix, not language-affecting. NO-PORT. |

### Port order recommendation

1. `e9741864a` → `5a7dadbf1` (InternalTypeClass hunk only) → `171f1f227`
   (init-order + determinism chain; land together, then bootstrap). The first
   two are **manual ports into `ncc/external/InternalTypes.n`** (cherry-pick
   yields the modify/delete no-op trap); only `171f1f227` may be cherry-picked.
2. `f35161ae9` as a manual port into `ncc/external/LibraryReference.n` with
   the `StrongNameKeyPair` re-implementation on FX (dnlib is not available
   there) and the R1-04 commit-note.
3. `1231f73af`'s rename only if/when the fixture pair moves to master.
Then one combined FX Stage1→Stage2 + FX suite run (R1-03) covering all landed
candidates, recorded on the master-side checklist. **The FX gate is mandatory
precisely because a hand-port can silently miss** (a monolithic target file
edited nowhere, or a stray retarget-path file never compiled): after the run,
also grep master's build log for the ported symbols to confirm they were
actually compiled.

## NO-PORT — remaining commits in `17d3cf47c..HEAD` (46)

All remaining commits touch only branch-specific tooling, plan documents, the
bootstrap seeds, the net10 harness, or the intentionally divergent backend.
None carries a shared-surface change master needs.

| group | commits (sha) | why no port |
|---|---|---|
| Plan/checklist/progress/review docs (doc/ only) | `8e0ab9688`, `15d353a60`, `305118d30`, `e461d3f75`, `53a9f5ba0`, `3228ff59e`, `78e848cb0`, `c30ef6c25`, `699669d00`, `1d2ee0f2a`, `1219c702b`, `ec4400e44`, `f0ae1b1b0`, `57d5856f6`, `12dbc039d`, `6936fd656`, `a42c896d0` (17) | Retarget-plan documentation; no code. (The CodeDom root-cause and netstandard2.0-load reports describe findings but change no shared source.) |
| Build orchestration, MSBuild tooling, packaging, CI, repo hygiene | `9527eef14`, `58338392d`, `faf2fef44`, `00cee342b`, `27a5e50a7`, `141c005c8`, `892d0f553`, `6b0f0d5db`, `a60ddff19`, `f59b10aea`, `9d4ef9490`, `c3af18bad`, `ab6dc93b0`, `2d6fe489f`, `46d4962d5`, `ae2611a61`, `61a4592ae`, `996a1486b`, `18ad28a72`, `f70d5aaa1`, `0fbbfb2ab`, `952a0a84d`, `1cb57f94b`, `f7021ed4d` (24) | Branch-specific toolchain: `build.ps1`/`build.sh`, `NemerleAll.nproj` net10 stages/Validate, NuGet on the net10 payload, VS2017/FX tooling removal (master *keeps* that tooling), net10 TFMs/apphosts, `global.json`/`Directory.Build.props`/`nuget.config`, CI workflow, `.gitignore`. The `27a5e50a7` testsuite-file touch is the CI known-failures allowlist (CI plumbing, content comments-only). |
| Bootstrap seeds | `2ff4e2fb4`, `a6c0a4cec` (2) | Binary reseeds per branch (`boot-net10`/`boot-dnlib`); master bootstraps from `boot-4.0` and must never consume a dnlib/Core seed. |
| CompilerTests harness net10 plumbing (6.x) | `ed7b901ae` (6.1 net10 apphost), `11b76c76a` (6.2c `dotnet exec` launch) (2) | Harness runs on master's FX host via the old AppDomain path; the Core launch/TFM plumbing is host work, not suite semantics. (`9ecc5c8a2`'s harness part is likewise net10 plumbing; its testsuite part is covered in the candidates table.) |
| dnlib dependency bump (intentionally divergent backend) | `42d1a188a` (1) | dnlib exists only on this branch (replaces SRE); master's emit path is `System.Reflection.Emit` per AGENTS.md. |

## Coverage

57 commits in `17d3cf47c..HEAD` **at the time of writing** = **11**
shared-surface candidates
(`f35161ae9`, `e9741864a`, `5a7dadbf1`, `171f1f227`, `1231f73af`, `9ecc5c8a2`,
`544a17cdf`, `da2b5cdee`, `8561fda03`, `252bc791d`, `2e031d283`) + **46**
NO-PORT (17 + 24 + 2 + 2 + 1 above). Every sha in the range *as of writing* is
accounted for; the range moves with `HEAD` — recount with
`git log --oneline 17d3cf47c..HEAD` and expect only doc/checklist commits
above the newest sha listed here.
