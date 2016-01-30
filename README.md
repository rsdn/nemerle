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
Compile and run
```bat
"C:\Program Files\Nemerle\ncc.exe" hello.n /out:hello.exe
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

  * For Development:
  
  [Nemerle build process (for Nemerle developers)](Nemerle build process (for Nemerle developers))

  * For Installer:
  
  Run BuildInstallerFull(fx-version).cmd depending on required .NET version. Installer will be placed in bin/Release/net-(fx-version)/Installer.
  
  _Note: You can also use BuildInstallerFast(fx-version).cmd to build installer without running tests._

  _Note: For building Visual Studio bindings you need VSSDK and administrative rights._

## Linux

  Nemerle can bootstrap itself on Mono.
  
  * Generic line:
  
  xbuild NemerleAll-Mono.nproj /p:TargetFrameworkVersion=v(3.5 or 4.0 or 4.5 or 4.5.1) /p:Configuration=Release(or Debug) /t:Stage4(1 - 4) /tv:4.0(Needed for framework 4.0 and above)   
  
  * Release 3.5:
  
  xbuild NemerleAll-Mono.nproj /p:TargetFrameworkVersion=v3.5 /p:Configuration=Release /t:Stage4  
  
  * Debug 4.0:
  
  xbuild NemerleAll-Mono.nproj /p:TargetFrameworkVersion=v4.0 /p:Configuration=Debug /t:Stage4 /tv:4.0
  

# What about IDE?

  * Visual Studio 2008/2010/2012/2013-preview integration installed by Nemerle installer
  * Nemerle Studio is a free IDE based on Visual Studio Shell (Isolated mode) installed by Nemerle installer if VS Shell was installed
  * Sharp Develop 3.0 addin can be builded manually. See snippets/sharpdevelop/ReadMe.txt 
  * See Vim, Emacs, Kate and other editors syntax support in the 'misc' folder

# Repository structure

  * Nemerle compiler sources (ncc/),
  * Nemerle Documentation (doc/),
  * standard Nemerle library (lib/),
  * standard Nemerle macros (macros/),
  * some examples of Nemerle programs (snippets/),
  * a few useful tools (e.g. synatx highlighting modes) (misc/),
  * binary Nemerle compiler needed to compile itself (boot/, boot-4.0/).
  * Nemerle realted tools (e.g. relector addin) (tools/)
  * Visual Studio 2008 integration (VsIntegration/)

# Contacts

  * Nemerle forum: http://groups.google.com/group/nemerle-en
  * Nemerle Russian forum: http://rsdn.ru/forum/nemerle/
