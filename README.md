# What Is It

Nemerle is a high-level statically-typed programming language for the .NET platform. It offers functional, object-oriented and imperative features. It has a simple C#-like syntax and a powerful meta-programming system.

Features that come from the functional land are variants, pattern matching, type inference and parameter polymorphism (aka generics). The meta-programming system allows great compiler extensibility, embedding domain specific languages, partial evaluation and aspect-oriented programming.

To find out more, please visit: http://nemerle.org/

# Quick sample

## Hello world

Create _hello.n_:

    using System.Console;

    WriteLine("Hello world")

Compile and run

    "C:\Program Files\Nemerle\ncc.exe" hello.n /out:hello.exe
    hello.exe

Will output

    Hello world

# Install

## Windows

  Install latest msi package from http://nemerle.org/

## Linux, Mono

  Download latest binary package from http://nemerle.org and export Nemerle=/path/to/binaries/extracted

# How to build

## Windows

  Run DevBuildQuick.cmd or DevBuildQuick-4.cmd depend on required netfx version. Compiler output will be placed at bin/Debug/net-{fx-version}/Stage1.

  _Note: for building Visual Studio bindings you need VSSDK and administrative rights._

## Linux

  Nemerle can't bootstrap itself on Mono, but can be used as binaries.

# What about IDE?

  * Visual Studio 2008 integration installed by Nemerle installer
  * Visual Studio 2010 integration is not released, but works and can be builded manually, see snippets/VS2010/readme.txt
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
