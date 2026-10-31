# What Is It

[![Join the chat at https://gitter.im/rsdn/nemerle](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/rsdn/nemerle?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Nemerle is a high-level statically-typed programming language for the .NET platform. It offers functional, object-oriented and imperative features. It has a simple C#-like syntax and a powerful meta-programming system.

Features that come from the functional land are variants, pattern matching, type inference and parameter polymorphism (aka generics). The meta-programming system allows great compiler extensibility, embedding domain specific languages, partial evaluation and aspect-oriented programming.

To find out more, please visit: http://nemerle.org/

# Quick sample

## Hello world

Create _hello.n_:
```nemerle
using System.Console;

WriteLine("Hello world")
```
Compile and run (on the `retarget-compiler` branch the compiler is the .NET 10
`ncc-core`, e.g. the one installed to `bin\Debug\net-4.0` by a build below; on
an `msi`-installed `master`-line release this is `ncc.exe`):
```bat
bin\Debug\net-4.0\ncc-core.exe hello.n /out:hello.exe
hello.exe
```
Will output
```bat
Hello world
```
# Install

## Windows

  Install latest msi package from http://nemerle.org/

## Linux, Mono

  Download latest binary package from http://nemerle.org and export Nemerle=/path/to/binaries/extracted

# How to build


Clone with all submodules: git clone --recursive git://github.com/rsdn/nemerle.git
If you have a clone already: git pull --recurse-submodules

## Windows

  The canonical entry point is `build.ps1` (it drives `NemerleAll.nproj` with a
  classic MSBuild >= 18 discovered via vswhere; **no Visual Studio 2017 and no
  registry probing needed**):

  ```powershell
  ./build.ps1                                    # Stage1 + extras + Install
  ./build.ps1 -Targets Stage3,Validate -Rebuild  # full bootstrap + IL fixed point
  ./build.ps1 -Targets Stage1,CompilerTests      # compiler regression suite
  ./build.ps1 -Targets _NugetPack                # NuGet packages
  ```

  Verified commands and details: `doc/NET10-BUILD-BASELINE.md`, `AGENTS.md`.
  The working compiler lands in `bin/Debug/net-4.0/ncc-core.exe` (.NET 10).

  _Note: MSI installer and Visual Studio integration targets
  (`DevBuild*`/`_Installer`/`_Integration`) are guarded off on this branch;
  the shipping IDE integration builds from `master` (which needs VSSDK and
  administrative rights)._

## Linux

  Full builds are Windows-only on this branch for now: `build.sh` (the bash
  twin of `build.ps1`) deliberately refuses to start off Windows because the
  MSBuild host, Validate tooling and stage machinery are Windows-only. The old
  Mono/xbuild bootstrap path (`NemerleAll-Mono.nproj`) is gone; see the net10
  plan under `doc/plans/` for the state of cross-platform work.
  

# What about IDE?

  * On this branch (`retarget-compiler`) there is no shipping IDE integration;
    the VS IDE sources live (Windows-only, guarded) under `snippets/VS2010/`.
  * On `master`: Visual Studio 2008/2010/2012/2013-preview integration installed
    by Nemerle installer
  * Nemerle Studio is a free IDE based on Visual Studio Shell (Isolated mode) installed by Nemerle installer if VS Shell was installed
  * Sharp Develop 3.0 addin can be builded manually. See snippets/sharpdevelop/ReadMe.txt 
  * See Vim, Emacs, Kate and other editors syntax support in the 'misc' folder

# Repository structure

  * Nemerle compiler sources (ncc/, host project ncc-core.nproj → ncc-core.exe on .NET 10),
  * Nemerle Documentation (doc/),
  * standard Nemerle library (lib/),
  * standard Nemerle macros (macros/),
  * some examples of Nemerle programs (snippets/),
  * a few useful tools (e.g. synatx highlighting modes) (misc/),
  * binary Nemerle compiler needed to compile itself (boot-net10/, plus the
    boot-dnlib/ fallback and the master-line boot-4.0/).
  * Nemerle realted tools (e.g. relector addin) (tools/)

The Visual Studio 2008 integration (VsIntegration/) lives on the `master`
branch only.

# Contacts

  * [Gitter](https://gitter.im/rsdn/nemerle) - chat for interactive discussions
  * Nemerle forum: http://groups.google.com/group/nemerle-en
  * Nemerle Russian forum: http://rsdn.ru/forum/nemerle/
