" Vim syntax file
"
" Copyright (c) 2003 The University of Wroclaw.
" All rights reserved.
"
" Redistribution and use in source and binary forms, with or without
" modification, are permitted provided that the following conditions
" are met:
"    1. Redistributions of source code must retain the above copyright
"       notice, this list of conditions and the following disclaimer.
"    2. Redistributions in binary form must reproduce the above copyright
"       notice, this list of conditions and the following disclaimer in the
"       documentation and/or other materials provided with the distribution.
"    3. The name of the University may not be used to endorse or promote
"       products derived from this software without specific prior
"       written permission.
" 
" THIS SOFTWARE IS PROVIDED BY THE UNIVERSITY ``AS IS'' AND ANY EXPRESS OR
" IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
" OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN
" NO EVENT SHALL THE UNIVERSITY BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
" SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
" TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
" PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
" LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
" NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
" SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
"
" Language:    Nemerle
" Maintainer:  Alexey Badalov <don_reba@inbox.ru>
" Last Change: 2008-11-12
" Version:     1.1

" Adapted from the gont.vim file.

" To use this file, copy it to ~/.vim/syntax/ and add the following to
" ~/.vim/filetype.vim (create one if you don't already have it):
"
"	augroup filetypedetect
"		autocmd BufNewfile,BufRead *.n setfiletype nemerle
"	augroup END
"
" Additionally, since ncc assumes utf-8 encoding of input files, you 
" can add the following to your ~/.vimrc file:
"
"	augroup nemerle
"		au!
"		autocmd BufNewfile,BufReadPre *.n
"			\ set fencs=ucs-bom,utf-8,iso-8859-2 fenc=utf-8
"	augroup END
"
"
" Have fun :)

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn keyword nemerleType int bool string void option list char float object
syn keyword nemerleType double long ulong uint ushort short byte sbyte

syn keyword nemerleModifier abstract extern internal new private protected
syn keyword nemerleModifier public sealed virtual static override partial async

syn keyword nemerleTopDecl class interface syntax module namespace type implements
syn keyword nemerleTopDecl extends struct using macro enum def value mutable 

syn keyword nemerleKeyword and as catch finally fun match out params ref throw delegate
syn keyword nemerleKeyword try typeof variant when where array is get set yield event

" macros
syn keyword nemerleKeyword if else while for unless lock do repeat until checked
syn keyword nemerleKeyword regexp foreach assert ignore this base unchecked

syn keyword nemerleConst null true false 

syn match nemerleSpec "[()\]\[]\|\*\["

syn match nemerleNumber "[0-9_]\+\((\.[0-9_]*)\)\?\([eE]\([+-]\)\?[0-9_]*\)\?"
syn match nemerleNumber "0[xX][0-9a-fA-F_]\+"
syn match nemerleNumber "0[bB][01_]\+"
syn match nemerleNumber "0[oO][0-7_]\+"

syn match nemerleTyVar "'[a-zA-Z_][a-zA-Z_0-9]*"
syn match nemerleChar "'\(.\|\\.\|\\x\x\x\)'"


syn match nemerleIdentifier "[a-z_][a-zA-Z_0-9]*"

syn match nemerleTyArg "[<>]"

" comments
syn keyword nemerleTodo contained XXX TODO FIXME
syn cluster nemerleCommentGroup contains=nemerleTodo
syn match nemerleComment "//.*$" contains=@nemerleCommentGroup
syn region nemerleComment start="/\*" end="\*/" contains=@nemerleCommentGroup fold

syn match nemerleString +@"\([^"]\|""\)*"+

syn match  nemerleSpecial display contained "\\\(x\x\x\|.\|$\)"
syn region nemerleString  start=+[$L]\="+ skip=+\\\\\|\\"+ end=+"+ contains=nemerleSpecial

" xml markup inside '///' comments
" borrowed from cs.vim
syn cluster xmlRegionHook   add=csXmlCommentLeader
syn cluster xmlCdataHook    add=csXmlCommentLeader
syn cluster xmlStartTagHook add=csXmlCommentLeader
syn keyword csXmlTag        contained Libraries Packages Types Excluded ExcludedTypeName ExcludedLibraryName
syn keyword csXmlTag        contained ExcludedBucketName TypeExcluded Type TypeKind TypeSignature AssemblyInfo
syn keyword csXmlTag        contained AssemblyName AssemblyPublicKey AssemblyVersion AssemblyCulture Base
syn keyword csXmlTag        contained BaseTypeName Interfaces Interface InterfaceName Attributes Attribute
syn keyword csXmlTag        contained AttributeName Members Member MemberSignature MemberType MemberValue
syn keyword csXmlTag        contained ReturnValue ReturnType Parameters Parameter MemberOfPackage
syn keyword csXmlTag        contained ThreadingSafetyStatement Docs devdoc example overload remarks returns summary
syn keyword csXmlTag        contained threadsafe value internalonly nodoc exception param permission platnote
syn keyword csXmlTag        contained seealso b c i pre sub sup block code note paramref see subscript superscript
syn keyword csXmlTag        contained list listheader item term description altcompliant altmember

syn cluster xmlTagHook add=csXmlTag

syn match   csXmlCommentLeader +\/\/\/+    contained
syn match   csXmlComment       +\/\/\/.*$+ contains=csXmlCommentLeader,@csXml
syntax include @csXml <sfile>:p:h/xml.vim
hi def link xmlRegion Comment

syn match nemerlePreCondit "#\(define\|elif\|else\|endif\|endregion\|error\|if\|line\|pragma\|undef\)"

syn match nemerleRegionName "#region\s\+.*"hs=s+8 contained
syn match nemerleRegionHead "#region" contains=nemerleRegionName

syn region nemerleRegion start="#region" end="#endregion" transparent keepend extend fold
syn region nemerleBlock start="{" end="}" transparent fold

" synchronization
syn sync fromstart

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_nemerle_syntax_inits")
  if version < 508
    let did_lisp_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif


HiLink nemerleChar       Character
HiLink nemerleComment    Comment
HiLink nemerleConst      Constant
HiLink nemerleKeyword    Statement
HiLink nemerleModifier   Type
HiLink nemerleNumber     Number
HiLink nemerlePreCondit  PreCondit
HiLink nemerleRegionHead PreCondit
HiLink nemerleRegionName Comment
HiLink nemerleSpec       Special
HiLink nemerleString     String
HiLink nemerleTodo       Todo
HiLink nemerleTopDecl    Statement
HiLink nemerleTyArg      Type
HiLink nemerleType       Type
HiLink nemerleTyVar      Special

HiLink csXmlCommentLeader Comment
HiLink csXmlComment       Comment
HiLink csXmlTag           Statement

" HiLink nemerleErrInParen Error
" HiLink nemerleErrInBracket Error
  delcommand HiLink
endif

let b:current_syntax = "nemerle"

if version < 700
	setlocal errorformat ^=
	\%f:%l:%c:\ %*[[0-9;m]%trror%*[[0-9;m]:\ %m,
	\%f:%l:%c:\ %*[[0-9;m]%tarning%*[[0-9;m]:\ %m,
	\%f:%l:%c:\ %*[[0-9;m]%tint%*[[0-9;m]:\ %m,
	\%f:%l:%c:%*[0-9]:%*[0-9]:\ %*[[0-9;m]%tint%*[[0-9;m]:\ %m,
	\%f:%l:%c:%*[0-9]:%*[0-9]:\ %*[[0-9;m]%trror%*[[0-9;m]:\ %m,
	\%f:%l:%c:%*[0-9]:%*[0-9]:\ %*[[0-9;m]%tarning%*[[0-9;m]:\ %m
else
	setlocal errorformat ^=
	\%f:%l:%c:%*[0-9]:%*[0-9]:\ %*[[0-9;m]%tint%*[[0-9;m]:\ %m,
	\%f:%l:%c:%*[0-9]:%*[0-9]:\ %*[[0-9;m]%trror%*[[0-9;m]:\ %m,
	\%f:%l:%c:%*[0-9]:%*[0-9]:\ %*[[0-9;m]%tarning%*[[0-9;m]:\ %m,
	\%f:%l:%c:\ %*[[0-9;m]%trror%*[[0-9;m]:\ %m,
	\%f:%l:%c:\ %*[[0-9;m]%tarning%*[[0-9;m]:\ %m,
	\%f:%l:%c:\ %*[[0-9;m]%tint%*[[0-9;m]:\ %m
endif
" vim: nowrap

