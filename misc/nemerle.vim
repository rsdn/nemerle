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
" Maintainer:  Michal Moskal <malekith@pld-linux.org>
" Last Change: $LastChangedDate$
" Version:     1.00
" Revision:    $Rev$

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
" copy this to your ~/.vimrc to enable outline mode (folding regions) on <F8>
" map <F8> <ESC>:call NemOutlineToggle()<CR>
" imap <F8> <ESC>:call NemOutlineToggle()<CR>a
"
" Have fun :)

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" based on http://vim.sourceforge.net/tips/tip.php?tip_id=333
function! NemOutlineToggle()
  let OldLine = line(".")
  let OldCol = virtcol(".")

  if (! exists ("b:outline_mode"))
     let b:outline_mode = 0
     let b:OldMarker = &foldmarker
  endif

  if (b:outline_mode == 0)
    let b:outline_mode = 1
    set foldmethod=marker
    set foldmarker=#region,#endregion
    " set foldcolumn=4
    set foldcolumn=0
  else
    let b:outline_mode = 0
    set foldmethod=marker
    let &foldmarker=b:OldMarker
    set foldcolumn=0
  endif
  
  execute "normal! ".OldLine."G"
  execute "normal! ".OldCol."|"
  unlet OldLine
  unlet OldCol
  " execute "normal! zv"
endfunction

syn keyword nemerleType int bool string void option list char float object
syn keyword nemerleType double long ulong uint ushort short byte sbyte

syn keyword nemerleModifier abstract extern internal new private protected
syn keyword nemerleModifier public sealed virtual static override partial

syn keyword nemerleTopDecl class interface syntax module namespace type implements
syn keyword nemerleTopDecl extends struct using macro enum

syn keyword nemerleKeyword and as catch def finally fun match out params ref throw
syn keyword nemerleKeyword try typeof variant when where array mutable is

" macros
syn keyword nemerleKeyword if else while for unless lock do repeat until
syn keyword nemerleKeyword regexp foreach assert ignore

syn keyword nemerleConst null true false this base

syn match	nemerleSpec	"[()\]\[]\|\*\["
syn match	nemerleNumber "0[xX][0-9a-fA-F]\+"
syn match	nemerleNumber "0[bB][01]\+"
syn match	nemerleNumber "0[oO][0-7]\+"
syn match	nemerleNumber "[0-9]\+\((\.[0-9]*)\)\?\([eE]\([+-]\)\?[0-9]*\)\?"

syn match	nemerleTyVar	"'[a-zA-Z_][a-zA-Z_0-9]*"
syn match	nemerleChar	"'\(.\|\\.\|\\x\x\x\)'"

syn match	nemerlePreCondit "^[ 	]*#\(region\|endregio\|if\|else\|endif\).*$"

" syn match	nemerleUIdentifier	"[A-Z][a-zA-Z_0-9]*"
syn match	nemerleIdentifier	"[a-z_][a-zA-Z_0-9]*"

syn match	nemerleTyArg	"[<>]"

" Comments
syn keyword nemerleTodo	contained	XXX TODO FIXME
syn cluster nemerleCommentGroup	contains=nemerleTodo
syn match nemerleComment	"//.*$"	contains=@nemerleCommentGroup
syn region nemerleComment start="/\*" end="\*/" contains=@nemerleCommentGroup

syn match	nemerleSpecial	display contained "\\\(x\x\x\|.\|$\)"
syn region	nemerleString	start=+L\="+ skip=+\\\\\|\\"+ end=+"+ contains=nemerleSpecial

" synchronization
syn sync lines=500

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

"  HiLink nemerleIdentifier	Identifier
"  HiLink nemerleUIdentifier	Constant
  HiLink nemerleComment	Comment
  HiLink nemerleKeyword	Statement
  HiLink nemerleTopDecl	Statement
  HiLink nemerleString	String
  HiLink nemerleChar	Character
  HiLink nemerleModifier	Type
  HiLink nemerleType		Type
  HiLink nemerleSpec	Special
  HiLink nemerleTyVar	Special
  HiLink nemerleNumber	Number
  HiLink nemerleConst	Constant
  HiLink nemerleTodo	Todo
  HiLink nemerlePreCondit	PreCondit
  
  HiLink nemerleTyArg	Type

" HiLink	nemerleErrInParen	Error
" HiLink	nemerleErrInBracket	Error
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
