" Vim compiler file
" Compiler:         Nemerle Compiler
" Maintainer:       Alexey Badalov <don_reba@inbox.ru>
" Latest Revision:  2007-03-16

if exists("current_compiler")
  finish
endif
let current_compiler = "ncc"

if exists(":CompilerSet") != 2		" older Vim always used :setlocal
  command -nargs=* CompilerSet setlocal <args>
endif

" error format
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

function! s:ExtractValue(line, name)
	return substitute(strpart(a:line, strlen('// ' . a:name . ':')), '^\s*\|\s*$', '', 'g')
endfunction

function! s:ReadHeader()
	let b:references = ""
	let b:target     = "exe"
	let b:out        = ""

	let header      = 1
	let lineNumber  = 1
	let lastLineNum = line('$')

	while header && lineNumber <= lastLineNum
		let line = getline(lineNumber)

		if line =~# "^// REFERENCE:"
			let b:references .= '-r "' . s:ExtractValue(line, 'REFERENCE') . '" '
		elseif line =~# "^// TARGET:"
			let b:target = s:ExtractValue(line, 'TARGET')
		elseif line =~# "^// OUT:"
			let b:out = s:ExtractValue(line, 'OUT')
		elseif line !~# "^//"
			let header = 0
		endif

		let lineNumber += 1
	endwhile

	if b:out == ""
		if b:target =~ 'library'
			let b:out = '"%<.dll"'
		else
			let b:out = '"%<.exe"'
		endif
	endif
endfunction

call s:ReadHeader()

execute 'CompilerSet makeprg=' . escape('ncc -no-color -nologo -out:' . b:out . ' -target:' . b:target . ' "%" ' . b:references . '$*', ' "')
