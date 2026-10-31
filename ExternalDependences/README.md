# Why SDK and WiX folder are empty ?

These folders are git submodules. The submodule folders (vssdk*, wix*) and the
WiX installer under `misc/packages/wix` are deliberately isolated Windows/.NET
Framework-only installer/IDE-integration dependencies; the Core compiler build
(`build.ps1` targets Stage*/Validate/CompilerTests/Install) never loads them
(net10 plan 5.4).

Run this command from the Nemerle root directory:

```
git submodule update --init
```
