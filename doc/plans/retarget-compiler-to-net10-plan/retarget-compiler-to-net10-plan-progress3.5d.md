# Progress 3.5d — Core-host-safe strong-name scan in LibraryReference.n

Scope: make the compiler's strong-name / `InternalsVisibleTo` scan work when
`ncc` RUNS on a .NET (Core) host. Target file:
`ncc\backend\hierarchy\LibraryReference.n`. Do NOT touch emit-time signing
(dnlib `InitializeStrongNameSigning`) or `LibraryReferenceManager.n` (3.5c).

## Intentions

1. Sites (report3.1-3.5 item 10):
   - `:84` `SHA1Managed()` — mandated change to `SHA1.Create()` (SYSLIB0020 on
     modern ref sets; works today on netstandard2.0 but must go).
   - `:72-76 def snKey() : SR.StrongNameKeyPair`, consumed at `:112-113` via
     `snKey().PublicKey` — ALL `StrongNameKeyPair` members throw
     `PlatformNotSupportedException` on .NET Core ⇒ the IVT scan breaks on the
     Core host.
2. Fix: read the public key bytes without `StrongNameKeyPair`. Options:
   `System.Reflection.AssemblyName.GetPublicKey()` (netstandard2.0-safe, does
   not throw) or dnlib's key tooling. The key file opened by `snKey()` is the
   same file the emit path signs with (`Options.StrongAssemblyKeyName`), so the
   minimal Core-safe read is: parse the .snk (full key blob) and take its
   public-key part. Inspect exact consumption (`toHexString(...)` hex compare,
   `generatePublicKeyToken(...)` last-8-bytes-of-SHA1 reversed) and adapt
   minimally. Preserve prior semantics for keyless assemblies (null ⇒ branch
   does not mark IsFriend).
3. Verify the exact reach condition of the `:109-115` branch
   (`string.IsNullOrEmpty(StrongAssemblyKeyName)` guard vs `snKey()` needing
   the key file — check `CompilationOptions.StrongAssemblyKeyName` semantics
   and history) and build the B-repro accordingly.
4. Acceptance:
   - A. Stage1 builds (exit 0):
     `cmd /c "`"C:\RSDN\nemerle\Build-core-base.cmd`" Debug Build Stage1"`.
   - B. Core-host probe in `.tmp\phase35d\`: `Lib.n` (signed with
     `misc\keys\Nemerle.Compiler.snk`, internal member + IVT to
     `Consumer, PublicKey=<hex>`) + `Consumer.n` (same key, `-r:Lib.dll`)
     compiles clean on `ncc-core.exe`; same pair compiles with boot FX
     `ncc.exe` (parity); negative control: Consumer signed with a DIFFERENT
     snk ⇒ compile FAILS with the internal-inaccessibility diagnostic (scan
     still executes on Core). Quote all three outcomes.
   - C. Full gates: Rebuild "Stage3,Validate" (FC ×5, sole error the known-red
     pefverify MSB3073); Build "Stage1,CompilerTests" ⇒ 468+167/0. Logs in
     `.tmp\phase35d\`.
   - D. `git diff --stat`: only `ncc\backend\hierarchy\LibraryReference.n`
     (+ this progress doc). No commit.

## Results

### Headline

The site is Core-ported AND repaired. The `InternalsVisibleTo` PublicKey branch was
**not only host-unsafe but fully dead code on every host** (inherited from `master`):
the branch guard required `string.IsNullOrEmpty(StrongAssemblyKeyName)` while
`snKey()` *needed* that key file (would have thrown `ArgumentNullException` from
`File.Open(null)`), and the inner key parse split `pKey` on `','` — a field produced
by splitting the IVT string on `','` can never itself contain `','`, so the 2-element
patterns `["publickey", key]` / `["publickeytoken", key]` could never match.
Measured pre-fix: same-key and wrong-key probes behaved identically (`unbound name
'Foo'`, exit 1) on both the Core-host Stage1 `ncc-core.exe` and the FX boot `ncc.exe`
— the key comparison never executed (`snKey().PublicKey`, hence no PNSE, was
unreachable). Both bugs live in the target file, so they were repaired there
(guard removed — the null-`publicKey()` check subsumes it; inner split `','` → `'='`,
which is exactly the `PublicKey=<hex>` / `PublicKeyToken=<hex>` IVT syntax the
pattern names prove intended).

### Code changes (single file: `ncc/backend/hierarchy/LibraryReference.n`, +30/−11)

1. `def snKey() : SR.StrongNameKeyPair` (all `StrongNameKeyPair` members throw PNSE
   on Core) → `def publicKey() : array[byte]`: reads
   `Options.StrongAssemblyKeyName` bytes and returns
   `dnlib.DotNet.StrongNameKey(bytes).PublicKey` (declared `System.Byte[]` in the
   referenced dnlib 4.5.0; verified equal to the public-key blob the same compiler
   writes into signed assemblies). No key file / unparseable key ⇒ `null`, the
   preserved "keyless ⇒ no friend" semantics (and the branch no longer crashes on
   `File.Open(null)` as the old code would have if ever reached).
2. Both `snKey().PublicKey` use-sites now bind `def pubKey = publicKey()` once and
   skip (`when (pubKey != null)`) when null.
3. Reachability repair (see Headline): guard dropped, inner split on `'='`.
4. `SHA1Managed()` → `System.Security.Cryptography.SHA1.Create()` (mandated;
   `generatePublicKeyToken` logic — last 8 SHA1 bytes reversed — unchanged).
5. Name-only IVT branch (line `| [asmName] when currentAssemblyName == asmName`)
   untouched — that is the branch all existing testsuite friend tests use.
   Emit-time signing (dnlib) and `LibraryReferenceManager.n` untouched.

### A — Stage1

`Build-core-base.cmd Debug Build Stage1` → `0 Error(s)`, `EXIT=0`,
"Nemerle installed to bin\Debug\net-4.0" (`.tmp\phase35d\stage1.log`).

### B — Core-host probe (`.tmp\phase35d\run-probe.ps1`, sources generated with key
hex derived live from the snk via dnlib)

Setup: `Lib.n` = `class Foo { mutable internal Bar : int; }` +
`[assembly: InternalsVisibleTo("Consumer, PublicKey=<hex of Nemerle.Compiler.snk>")]`,
signed `-keyfile:misc\keys\Nemerle.Compiler.snk`; `Consumer.n` uses `Foo`/`Bar`,
`-out:Consumer.dll`, `-r:Lib.dll`.

Pre-fix (unmodified Stage1 `ncc-core.exe`, .NET 10) — `probe-prefix-core.log`:

```
=== [2] Consumer.n signed with SAME key (Nemerle.Compiler.snk) - expect SUCCESS ===
...Consumer.n(5,15,5,18): error: unbound name `Foo'
SAMEKEY_EXIT=1
```

Pre-fix (FX boot `bin\Debug\net-4.0\boot\ncc.exe`) — `probe-prefix-fx.log`: IDENTICAL
failure (`SAMEKEY_EXIT=1`, wrong-key identical too) ⇒ the old seed has the same dead
scan; the PublicKey branch is unreachable in it (see Headline). Consequence for the
"boot parity" criterion: the boot binary cannot compile the same-key pair (its scan
is the dead one being fixed here; current tree builds no FX `ncc.exe` to re-test
with). Parity is therefore evidenced by pre-fix Stage1(Core) ≡ boot(FX) — both dead —
and post-fix correctness on the Core host below.

Post-fix (rebuilt Stage1 `ncc-core.exe`) — `probe-post-core.log`:

```
=== [2] Consumer.n signed with SAME key (Nemerle.Compiler.snk) - expect SUCCESS ===
...Consumer.n(1,1,9,2): warning: N10003: ...  (benign)
SAMEKEY_EXIT=0
=== [3] Consumer.n signed with DIFFERENT key (Nemerle.Evaluation.snk) — expect FAILURE ===
...Consumer.n(5,15,5,18): error: unbound name `Foo'
BADKEY_EXIT=1
```

[3] proves the scan executes and actually compares keys (not a silent pass).

`PublicKeyToken=` variant (exercises the `SHA1.Create()` site — the only path through
`generatePublicKeyToken`), IVT `PublicKeyToken=5291d186334f6101` (SHA1-derived token
of `Nemerle.Compiler.snk`): `TOKEN_SAMEKEY_EXIT=0`, `TOKEN_BADKEY_EXIT=1` (wrong key).

No-keyfile edge (old code would have crashed at `File.Open(null)` if ever reached):
Consumer unsigned ⇒ clean `error: unbound name 'Foo'`, `NOKEY_EXIT=1`, no PNSE/ICE.

### C — Full gates (logs in `.tmp\phase35d\`)

* `Rebuild "Stage3,Validate"` → `no differences encountered` count = **5** (FC ×5);
  unique `: error ` line = the known-red
  `NemerleAll.nproj(359,5): error MSB3073: … pefverify.exe … Stage3\Nemerle.Compiler.dll … exited with code 1`
  (printed twice by MSBuild; pefverify's own "1126/1127 Error(s)" output lines are its
  known complaint on that dll). (`stage3-validate.log`)
* `Build "Stage1,CompilerTests"` → positive **468** passed, negative **167** passed
  (635 `passed` lines; 0 `FAILED`), two × `All tests passed.`, `0 Error(s)`, EXIT=0.
  Sole `(?i)failed` hit is the known-harmless NuGet line "Failed to load prune package
  data …". (`stage1-tests.log`)

### D — Diff scope

`git diff --stat`: `ncc/backend/hierarchy/LibraryReference.n | 30 +++---…` only,
plus this untracked progress doc. `AGENTS.md` was already untracked at start.
Not committed (per instructions).

