" Vim indent file
" Language:	Nemerle
" Maintainer:	Piotr Kalinowski <pitkali@interia.pl>
" Last Change:	2005 May 03

" Instructions:
" Put this file under ~/.vim/indent/nemerle.vim and ensure the following lines are
" present in ~/.vim/filetype.vim:
" 
" augroup filetypedetect
"    autocmd BufNewfile,BufRead *.n setfiletype nemerle
" augroup END
"
" Any comments and suggestions are welcome.

" Load only once
if exists("b:did_indent")
    finish
endif
let b:did_indent = 1

setlocal cinkeys-=:
setlocal indentkeys& indentkeys-=: indentkeys+=<Bar>,0=requires,0=ensures,0=invariant
setlocal indentexpr=GetNemerleIndent()

" Define function only once.
if exists("*GetNemerleIndent")
    finish
endif

" Find previous line, skipping blanks and comments
function! GetPrevious(lnum)
    " Skip comments
    let prev = prevnonblank(a:lnum)
    while prev > 0
	" C++ comments
	if getline(prev) =~ '^\s*//'
	    let prev = prev - 1
	elseif getline(prev) =~ '\*/\s*$' && getline(prev) !~ '\S*\s*/\*.*\*/\s*$' " C comment
	    let prev = prev - 1
	    if getline (prev + 1) !~ '/\*'
		while prev > 0 && getline(prev) !~ '/\*'
		    let prev = prevnonblank(prev - 1)
		endwhile
		let prev = prev - 1
	    endif
	else
	    break
	endif
	let prev = prevnonblank(prev)
    endwhile
    
    return prev
endfunction
    

function GetNemerleIndent()
    " Start with indentation as in C. We differ only in few cases
    let theIndent = cindent(v:lnum)

    let prev = GetPrevious(v:lnum - 1)
    let cur_line = getline(v:lnum)

    " If by any chance:
    " * we hit begining of the file
    " * we are at the begining of the block
    " * we are at the end of the block
    " - trust cindent
    if prev == 0 || cur_line =~ '^\s*{' || cur_line =~ '^\s*}'
	return theIndent
    endif

    let prev_line = getline(prev)
    let ind = indent(prev)

    " Attributes
    if prev_line =~ '^\s*\[.*\]\s*\(\s*\|//.*\|/\*.*\*/\s*\)$'
	return ind
    endif

    " Foreach
    if prev_line =~ 'foreach (.*)\s*\(\s*\|//.*\|/\*.*\*/\s*\)$'
	return ind + &sw
    endif

    " Design by contract macros
    if cur_line =~ '^\s*\(requires\>\|ensures\>\|invariant\>\)'
	return ind
    endif

    " Here we shall handle the colon. There are exactly two scenarios we need
    " to handle - function signature not ending with semicolon or opening
    " brace, and a field with no attributes.
    if cur_line =~ '^\s*\S*\s*:.*;' || prev_line =~ '^\s*\S*\s*:.*;' " field
	if prev_line !~ '\(;\|}\)\s*\(\s*\|//.*\|/\*.*\*/\s*\)$' " previous line is not end of expression
	    return ind + &sw
	else
	    return ind
	endif
    endif

    " funtion signature
    if prev_line =~ '(.*)\s*:' && prev_line !~ '\(;\|{\)\s*\(\s*\|//.*\|/\*.*\*/\s*\)$'
	return ind + &sw
    endif

    " Current is a pattern
    if cur_line =~ '^\s*|'
	if prev_line =~ '{\s*\(\s*\|//.*\|/\*.*\*/\s*\)$'
	    return ind + &sw
	endif
	let depth = 1
	if prev_line =~ '}\s*\(\s*\|//.*\|/\*.*\*/\s*\)$'
	    let depth = depth + 1
	endif
	while prev > 0 && getline(prev) !~ '^\s*|' || depth > 1
	    let prev = GetPrevious(prev - 1)
	    if getline(prev) =~ '}'
		let depth = depth + 1
	    endif
	    if getline(prev) =~ '{'
		let depth = depth - 1
	    endif
	endwhile
	if depth == 0
	    return theIndent
	else
	    return indent(prev)
	endif
    endif

    " Now matching. Here we need to follow operation of cindent to know what
    " to fix. Basically, we need to get previous non-continuation line.
    if prev_line =~ '\(;\|}\)\s*\(\s*\|//.*\|/\*.*\*/\s*\)$' 
	" now we know that Current line is non-continuation
	let prev = GetPrevious(prev - 1)
	let depth = 1
	if prev_line =~ '}\s*$'
	    let depth = 2
	endif
	while prev > 0 && getline(prev) !~ '\(;\|{\|}\)\s*\(\s*\|//.*\|/\*.*\*/\s*\)$' || depth > 1
	    if getline(prev) =~ '}'
		let depth = depth + 1
	    endif
	    if getline(prev) =~ '{'
		let depth = depth - 1
	    endif
	    let prev = GetPrevious(prev - 1)
	endwhile
	let prev = prev + 1 "TODO: obviously - it doesn't have to be next line...
	let prev_nonc = getline(prev)

	" if previous is a match pattern
	if prev_nonc =~ '^\s*|'
	    return theIndent + &sw
	endif
    endif

    return theIndent
endfunction " GetNemerleIndent

