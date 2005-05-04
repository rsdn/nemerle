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
	elseif getline(prev) =~ '\*/\s*$' " C comment
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

    " Design by contract macros
    if cur_line =~ '^\s*\(requires\>\|ensures\>\|invariant\>\)'
	return ind
    endif

    if prev_line =~ ':' && prev_line !~ '{'
	" If the previous line contains a colon but no {, cindent fails
	" to correctly determine indentation. A colon is function or field
	" declaration. Next line should have the same indentation unless
	" previous line doesn't end with semicolon (function signature).
	let theIndent = ind
	if prev_line !~ ';\s*$'
	    let theIndent = theIndent + &sw
	endif
	return theIndent
    endif

    " following handles lines inside a match clause
    if prev_line =~ '\s*|'
	if cur_line !~ '\s*|'
	    " if current line is not match pattern, whereas previous is
	    let theIndent = ind + &sw " indent it
	endif
    else
	if cur_line =~ '\s*|' " if previous line is not a pattern, but current is
	    if prev_line !~ '{' " and current line is not the first pattern in match clause
		let theIndent = ind - &sw " decrease indentation
	    endif
	else
	    " this is the normal line, but if, by any chance, we are
	    " inside a match clause, we need to ensure that the
	    " indentation is like the previous line.
	    while prev > 0
		if getline(prev) =~ '\s*|'
		    " yep - we are inside a match
		    let theIndent = ind
		    break
		elseif getline(prev) =~ '{'
		    " if we reach that without coming across a match
		    " pattern, we can safely leave the things as they are.
		    " Even if the block, begining of which we've reached,
		    " is inside a match itself, cindent does good job
		    " here.
		    break
		endif
		let prev = GetPrevious(prev - 1)
	    endwhile
	endif
    endif

    return theIndent
endfunction " GetNemerleIndent

