" Vim filetype plugin file
" Language:	Nemerle
" Maintainer:	Alexey Badalov <abadalov@mail.ru>
" Last Change:	Wed 01/10/2007 

if exists("b:did_ftplugin")
	finish
endif
let b:did_ftplugin = 1

" folding
setlocal foldmethod=syntax

" compiler
compiler ncc

" program execution
if b:target =~# 'winexe'
	noremap <silent> <buffer> <F5> :!start "%<.exe"<CR>
else
	noremap <silent> <buffer> <F5> :!start cmd /c "%<.exe" & pause<CR>
endif
