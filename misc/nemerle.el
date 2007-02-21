;;; nemerle.el -- major mode for editing nemerle programs

;; Copyright (C) 2003-2005 The University of Wroclaw
;; All rights reserved.

;; Author: Jacek Sliwerski (rzyjontko) <rzyj@o2.pl>
;; Maintainer: Jacek Sliwerski (rzyjontko) <rzyj@o2.pl>
;; Created: 5 Oct 2003
;; Version: 0.2
;; Keywords: nemerle, mode, languages
;; Website: http://nemerle.org


;; This file is not part of GNU Emacs.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;    1. Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;    2. Redistributions in binary form must reproduce the above copyright
;;       notice, this list of conditions and the following disclaimer in the
;;       documentation and/or other materials provided with the distribution.
;;    3. The name of the University may not be used to endorse or promote
;;       products derived from this software without specific prior
;;       written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE UNIVERSITY ``AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN
;; NO EVENT SHALL THE UNIVERSITY BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;; TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.



;;; Commentary:

;; A major mode for editing nemerle source files.  It provides syntax
;; hilighting, proper indentation, and many other features.
;; To install the nemerle mode, put the following lines into your
;; ~/.emacs file:

;; (setq load-path (cons "/path/to/dir/where/this/file/resides" load-path))
;; (autoload 'nemerle-mode "nemerle.el"
;;   "Major mode for editing nemerle programs." t)
;; (setq auto-mode-alist (cons '("\\.n$" . nemerle-mode) auto-mode-alist))

;; If you'd like to have every line indented right after new line put
;; these lines into your ~/.emacs files.

;; (defun my-nemerle-mode-hook ()
;;   (define-key nemerle-mode-map "\C-m" 'newline-and-indent))
;; (add-hook 'nemerle-mode-hook 'my-nemerle-mode-hook)

;; You may use variables nemerle-basic-offset and nemerle-match-case-offset
;; to customize indentation levels.

;; By default indentation based syntax is turned on. It is switched off
;; inside any parens anyway so it should be ok. If you think otherwise
;; you can use following in your ~/.emacs file:

;; (setq nemerle-indentation-based-syntax nil)



;;; Change Log:

;; 2006-12-27 Piotr Kalinowski <pitkali@gmail.com>
;;   * Added indent-expr-to-paren variable, so now the user can
;;     choose behaviour when breaking the expression inside
;;     parens.

;; 2006-03-06 Piotr Kalinowski <pitkali@gmail.com>
;;   * I have corrected if-else structures handling
;;     and in-match detection.
;;   * Added support for indentation based syntax
;;     - try .. catch and if .. else alignment
;;     - tab iterates over different indentation possibilities
;;     - backspace if pressed inside indentation deletes
;;       until previous indentation level
;;     - \C-c. to shift region right, \C-c, to shift left
;;     - \C-c\C-j to indent single line, use region-indent
;;       to indent multiple lines
;;     - inside any parens indentation based syntax
;;       modifications are turned off
;;     - support for explicit switching syntax type with
;;       \C-c\C-i and state feedback in mode line.

;; 2005-05-10 rzyjontko <rzyj@o2.pl>
;;   * final fixes of indentation engine and comment handling

;; 2005-05-04 rzyjontko <rzyj@o2.pl>
;;   * indentation engine fixes

;; 2005-04-29 rzyjontko <rzyj@o2.pl>
;;   * changes possible due to new syntax:
;;     - rewrote indenting engine
;;     - adapted coloring scheme

;; 2004-04-27 rzyjontko <rzyj@o2.pl>
;;   * further coloring improvements
;;   * fixed syntax table

;; 2004-01-24 rzyjontko <rzyj@o2.pl>
;;   * fixed coloring

;; 2004-01-23 rzyjontko <rzyj@o2.pl>
;;   * indent to open parenthesis

;; 2004-01-21 rzyjontko <rzyj@o2.pl>
;;   * improved indentation
;;   * changed syntax table
;;   * disabled tab-indent
;;   * switched to new grammar
;;   * electric-bar and electric-brace

;; 2003-11-17 rzyjontko <rzyj@o2.pl>
;;   * updated copyright disclaimer
;;   * basic indentation engine

;; 2003-10-09 rzyjontko <rzyj@o2.pl>
;;   * nemerle mode automatically sets file coding system to utf-8
;;   * syntax table changes
;;   * more colours
;;   * indentation framework

;; 2003-10-05 rzyjontko <rzyj@o2.pl>
;;   * initial version



;;; Known Bugs

;; There is a problem with single quote character, as it is also used 
;; in type variables and may not be treated just like in C.  There is
;; only one situation when you will suffer from this, namely if you try
;; to insert the following sequence: '"'.  In this case, everything after
;; the double quote will be coloured as a string.  However there exists
;; a very simple workaround for this: simply type '\"' instead.  The same
;; problem applies, when trying to indent line with '[' or '('.  Use the
;; same workaround i.e.: replace them with '\[' and '\(' respectively.


;;; Todo:

;; - further indentation improvements
;; - imenu (with ncc execution)
;; - make _ be a special symbol (write matcher functions)



;;; Code:

(provide 'nemerle-mode)


(defvar nemerle-mode-map nil
  "The keymap used in nemerle-mode.")

(defvar nemerle-font-lock-keywords nil
  "Font lock definitions for nemerle-mode.")

(defvar nemerle-mode-syntax-table nil
  "The syntax table used in nemerle-mode.")

(defvar nemerle-mode-hook nil
  "This hook is run when nemerle-mode is loaded, or a new nemerle-mode
buffer created.  This is a good place to put your customizations.")

(defvar nemerle-basic-offset 4
  "Indentation of blocks in nemerle.")

(defvar nemerle-match-case-offset 2
  "Indentation of match case bodies.")

(defvar nemerle-indentation-based-syntax t
  "Whether we are using indentation based syntax. On by default, because
it'll get turned off inside any parens anyway.")

(defvar nemerle-indent-expr-to-paren t
  "If a line is broken inside parenthesised expression and this is set to t,
next line will be aligned to the opening paren.")

(unless nemerle-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'comment-region)
    (define-key map "|" 'nemerle-electric-bar)
    (define-key map "{" 'nemerle-electric-brace-begin)
    (define-key map "}" 'nemerle-electric-brace-end)
    (define-key map "*" 'nemerle-electric-star)
    (define-key map "e" 'nemerle-electric-e)
    (define-key map "h" 'nemerle-electric-h)
    (define-key map "\C-c\C-i" 'nemerle-toggle-indentation-syntax)
    (define-key map "\C-c\C-j" 'nemerle-indent-line)
    (define-key map (kbd "TAB") 'nemerle-electric-tab)
    (define-key map (kbd "<backspace>") 'nemerle-electric-delete)
    (define-key map "\C-c." 'nemerle-shift-region-right)
    (define-key map "\C-c," 'nemerle-shift-region-left)
    (setq nemerle-mode-map map)))

(unless nemerle-font-lock-keywords
  (setq nemerle-font-lock-keywords
	(list
	 ;; character constants
	 '("'[^\\']'\\|'\\\\.'" 0 font-lock-string-face)

	 ;; keywords
	 ;; some keywords are introduced later in more complex regular 
	 ;; expressions such that we can mark their argument with colour
	 ;; these are:
	 ;;    class, interface, module, namespace, using, variant
	 ;; 'void' and 'array' are also keywords but we treat them
	 ;; as type names
	 '("\\<\\(_\\|abstract\\|and\\|as\\|base\\|catch\\|def\\|delegate\\|enum\\|event\\|true\\|false\\|finally\\|fun\\|implements\\|internal\\|is\\|macro\\|match\\|matches\\|mutable\\|new\\|null\\|out\\|override\\|params\\|private\\|protected\\|public\\|ref\\|sealed\\|static\\|struct\\|syntax\\|this\\|throw\\|try\\|type\\|typeof\\|virtual\\|volatile\\|when\\|where\\|partial\\)\\>"
	   0 font-lock-keyword-face)

	 ;; these aren't really keywords but we set them so
	 '("\\<\\(do\\|else\\|for\\|if\\|regexp\\|unless\\|while\\|when\\|in\\|foreach\\)\\>"
	   0 font-lock-keyword-face)
	 '("=>" 0 font-lock-keyword-face)
	 '("\\<\\(foreach\\)\\s *(.*:\\s *\\(\\w*\\)\\s *\\(\\<in\\>\\)"
	   (1 font-lock-keyword-face) 
	   (2 font-lock-type-face) 
	   (3 font-lock-keyword-face))
	 
	 '("\\<\\(variant\\|class\\|interface\\|module\\|namespace\\|using\\)\\s +\\(\\(\\w\\|\\.\\)*\\)"
	   (1 font-lock-keyword-face) (2 font-lock-function-name-face))
	 
	 ;; types
	 '("->" 0 font-lock-type-face)
	 '("\\<\\(void\\|int\\|uint\\|char\\|float\\|double\\|decimal\\|byte\\|sbyte\\|short\\|ushort\\|long\\|ulong\\|bool\\|string\\|object\\|list\\|option\\|array\\)\\>"
	   0 font-lock-type-face)
	 
	 ;; constants
         '("\\<[0-9]+\\>" 0 font-lock-constant-face))))


(unless nemerle-mode-syntax-table
  (setq nemerle-mode-syntax-table (copy-syntax-table c-mode-syntax-table))
  (modify-syntax-entry ?_  "_"  nemerle-mode-syntax-table)
  (modify-syntax-entry ?\\ "\\" nemerle-mode-syntax-table)
  (modify-syntax-entry ?+  "."  nemerle-mode-syntax-table)
  (modify-syntax-entry ?-  "."  nemerle-mode-syntax-table)
  (modify-syntax-entry ?=  "."  nemerle-mode-syntax-table)
  (modify-syntax-entry ?%  "."  nemerle-mode-syntax-table)
  (modify-syntax-entry ?<  "."  nemerle-mode-syntax-table)
  (modify-syntax-entry ?>  "."  nemerle-mode-syntax-table)
  (modify-syntax-entry ?&  "."  nemerle-mode-syntax-table)
  (modify-syntax-entry ?|  "."  nemerle-mode-syntax-table)
  (modify-syntax-entry ?\' "."  nemerle-mode-syntax-table)
  (cond
   ;; XEmacs 21
   ((not (boundp 'c-emacs-features))
    (modify-syntax-entry ?/  ". 1456" nemerle-mode-syntax-table)
    (modify-syntax-entry ?*  ". 23"   nemerle-mode-syntax-table))
   ;; XEmacs 19 & 20
   ((memq '8-bit c-emacs-features)
    (modify-syntax-entry ?/  ". 1456" nemerle-mode-syntax-table)
    (modify-syntax-entry ?*  ". 23"   nemerle-mode-syntax-table))
   ;; Emacs 19 & 20
   ((memq '1-bit c-emacs-features)
    (modify-syntax-entry ?/  ". 124b" nemerle-mode-syntax-table)
    (modify-syntax-entry ?*  ". 23"   nemerle-mode-syntax-table))
   ;; incompatible
   (t (error "Nemerle Mode is incompatible with this version of Emacs"))
   )
  (modify-syntax-entry ?\n "> b"  nemerle-mode-syntax-table)
  ;; Give CR the same syntax as newline, for selective-display
  (modify-syntax-entry ?\^m "> b" nemerle-mode-syntax-table))


(defun nemerle-toggle-indentation-syntax (arg)
  "Toggle the variable controlling type of the syntax used."
  (interactive "p")
  (cond (nemerle-indentation-based-syntax
	 (setq nemerle-indentation-based-syntax nil)
	 (setq mode-name "Nemerle"))
	(t
	 (setq nemerle-indentation-based-syntax t)
	 (setq mode-name "Nemerle Indent.")))
  (force-mode-line-update))


(defun nemerle-go-up-one-level ()
  "Find an innermost surrounding parenthesis (or brace, or whatever)
and return the corresponding character.  Return 0 if the point is in
the topmost block."
  (let* ((here (point))
	 (beg-buf (point-min))
	 (state (parse-partial-sexp here beg-buf)))
    (cond ((> (nth 0 state) 0)
	   (goto-char (nth 1 state))
	   (char-after))
	  (t
	   (goto-char (point-min))
	   0))))


(defun nemerle-is-not-indentation-exception ()
  "Returns t if indentation-based syntax should be applied."
  (let* ((here (point))
	 (beg-buf (point-min))
	 (state-one (parse-partial-sexp here beg-buf))
	 (state-two (parse-partial-sexp here beg-buf -1)))
    (goto-char here)
    (cond ((or (> (nth 0 state-one) 0)
	       (nth 3 state-two))
	   nil)
	  (t
	   t))))



(defun nemerle-skip-sexps (end)
  "Skip all blocks of code (delimited with braces) until END.
Returns t if inside a comment."
  (let ((last-brace-pos (point))
	(last-pos (point)))	       ; this is used to force advance
    (while (< (point) end)
      (forward-char 1)
      (parse-partial-sexp (point) (point-max) 1)
      (backward-char 1)
      (when (< (point) end)
	(forward-list 1)
	(backward-char 1))
      (if (and (< (point) end) (looking-at "}"))
	  (setq last-brace-pos (point)))
      (while (<= (point) last-pos)    ; this is where we force advance
	(forward-char 1))
      (setq last-pos (point)))
    (goto-char last-brace-pos)))


(defun nemerle-up-and-skip ()
  "Do NOT use it.  For testing purposes only!"
  (interactive)
  (beginning-of-line)
  (let ((end (point)))
    (nemerle-go-up-one-level)
    (nemerle-skip-sexps end)))


(defun nemerle-in-comment ()
  "Return t if the point is somewhere in the comment."
  (let ((state (parse-partial-sexp (point-min) (point) -1)))
    (nth 4 state)))


(defun nemerle-in-string ()
  "Return t if the point is somewhere in the string."
  (let ((state (parse-partial-sexp (point-min) (point) -1)))
    (nth 3 state)))


(defun nemerle-in-match ()
  "Return t if the point is somewhere in the match statement."
  (save-excursion
    (let ((end (point))
	  (line 'none)
	  (in-match-case nil)
	  (at-end nil)
	  (brace (nemerle-go-up-one-level))
	  (begin (point)))
      (if (not (eq brace ?{))
	  nil
	(while (and (not in-match-case) (not at-end) (<= (point) end))
	  (setq line (nemerle-analyze-line))
	  (if (and (eq line 'match-case) (eq (nth 0 (parse-partial-sexp (point) begin)) 1))
		 (setq in-match-case t))
	  (if (or (> (forward-line 1) 0) (eq (point) end))
	      (setq at-end t))
	  (beginning-of-line))
	  in-match-case))))


(defun nemerle-on-empty-line ()
  "Return t if the point is on an empty line."
  (save-excursion
    (beginning-of-line)
    (looking-at "[ \t]*$")))


(defun nemerle-analyze-line ()
  "Analyze the current line."
  (save-excursion
    (beginning-of-line)
    (cond ((nemerle-in-comment)
	   (if (looking-at "[ \t]*\\*")
	       'star-comment
	     'comment))
	  ((nemerle-in-string)
	   'in-string)
	  ((looking-at "[ \t]*{")
	   'begin-brace)
	  ((looking-at "[ \t]*}")
	   'end-brace)
	  ((looking-at "[ \t]*|")
	   'match-case)
	  ((looking-at "[ \t]*else\\b")
	   'else-clause)
	  ((or (looking-at "[ \t]*catch\\b")
	       (looking-at "[ \t]*finally\\b"))
	   'catch-clause)
	  (t
	   'none))))


(defun nemerle-analyze-block (end result)
  "Analyze the block from current point position until END.  Return
the relative offset + result for the block."
  (goto-char end)
  (if (nemerle-in-match)
      (setq result (+ result nemerle-match-case-offset)))
  result)


;; Go backwards, skipping comments, but no further than begin.
;; After calling this  function you end up at most at the beginning
;; of the line containing begin.
(defun nemerle-go-backwards (begin &optional indent-syntax)
  ;; checks if currrent line is a comment line
  ;; such situation will be noticed either by nemerle in comment in case
  ;; of being in the middle of multi line comment or two regexps checking
  ;; for the beginning of a comment.
  (if indent-syntax
      (defun should-be-skipped ()
	(or (looking-at "[ \t]*/\\*") (nemerle-in-comment) (nemerle-on-empty-line) ))
    (defun should-be-skipped ()
      (or (looking-at "[ \t]*//") (looking-at "[ \t]*/\\*") (nemerle-in-comment)
	  (nemerle-on-empty-line))))

  (let ((lvl (nth 0 (parse-partial-sexp (point) begin))))
    (while (and (eq (forward-line -1) 0) (< begin (point))
		(should-be-skipped))
      nil)
    (if (save-excursion (beginning-of-line) (looking-at "[ \t]*}"))
	nil
      (cond ((and (< begin (point))
		  (> (nth 0 (parse-partial-sexp (point) begin)) lvl))
	     (nemerle-go-up-one-level)
	     (beginning-of-line)
	     (while (and (< begin (point))
			 (> (nth 0 (parse-partial-sexp (point) begin)) lvl))
	       (nemerle-go-up-one-level)
	       (beginning-of-line))
	     (goto-char (max begin (point))))
	    (t
	     nil)))
    (beginning-of-line)))
		      	 
 
(defun nemerle-check-if (end line offset)
  "Checks if we have a case of if/else/whatever with one-line body. In such
case braces may be missing and then syntax parser won't indicate the need to
nest next line. This function will."
  ;; check previous line for the if case and return offset
  (let ((begin (point))
	(add_offset 0) ; additional offset we could add
	(done nil))
    (unless (eq line 'begin-brace) ; don't indent begin-brace any more
      (setq add_offset nemerle-basic-offset))
    
    (goto-char end)
    (nemerle-go-backwards begin)
    (if (> begin (point)) ; gone to far
	(setq done t))
    (goto-char (max (+ begin 1) (point)))
    (let ((here (point))
	  (type 'none))
       (setq type (cond
		   ((looking-at ".*\\b\\(if\\|for\\|foreach\\|while\\|when\\|unless\\)[ \t]*(")
		    'if)
		   ((looking-at "[ \t]*else\\b")
		    'else)
		   (t
		    'none)))
       (goto-char begin)
       (if (eq type 'else)
	   ;; skip to corresponding if - we should have same indentation
	   (setq here (nemerle-find-if (point) here)))
       (if (eq type 'none)
	   offset
	 ;; we have the if-case, so increase offset and check another line
	 ;; in case of nested if-cases
	 (setq offset (+ offset add_offset))
	 (if done
	     offset
	   (nemerle-check-if here 'none offset))))))
	 

(defun nemerle-find-if (begin end)
  "Find if clause corresponding to else clause at position END. Return
its position or END if no appropriate clause was found."
  (save-excursion
    (let ((depth 1)
	  (retval nil)
	  (too-far nil))
      ;; depth is number of unmatched else's. We start with value of
      ;; of 1 - the else line at end, and proceed until it reaches 0
      (goto-char end)
      (while (and (not (eq depth 0)) (not too-far))
	(nemerle-go-backwards begin)
	(if (or (> begin (point)) (eq (point-min) (point))) ;; oops - too far
	    (setq too-far t))
	(goto-char (max (+ 1 begin) (point)))
	(if (looking-at "[ \t]*else\\b")
	    (setq depth (+ depth 1))
	  (if (looking-at ".*\\bif[ \t]*(")
	      (setq depth (- depth 1)))))
      (if (eq depth 0)
	  (setq retval (point))
	(setq retval end))
      retval)))
      

(defun nemerle-get-offset (end line)
  "Return the relative offset for the block from the current point
position until END, where the last line has syntactic meaning given 
by LINE."
  (cond ((eq line 'end-brace)
	 0)
	((eq line 'comment)
	 (nemerle-analyze-block end 2))
	((eq line 'star-comment)
	 ;; handle star comments inside if-clause without braces
	 (nemerle-analyze-block end (+ 1 (nemerle-check-if end line 0))))
	((eq line 'match-case)
	 (nemerle-analyze-block end (- nemerle-match-case-offset)))
	((eq line 'else-clause)
	 ;; handle proper indentation of else clause
	 (nemerle-analyze-block end (nemerle-check-if
				     (nemerle-find-if (point) end)
				     line 0)))
	(t
	 ;; handle if-clause without braces
	 (nemerle-analyze-block end (nemerle-check-if end line 0)))))


(defun nemerle-get-nested (end line)
  "Return the relative offset for the line LINE nested in the block
of code.  Analyze code from the current point position until END."
  (if (eq line 'end-brace)
      0
    (+ nemerle-basic-offset (nemerle-get-offset end line))))


(defun nemerle-looking-at-function ()
  (or (looking-at
	   "[ \t]*\\(\\(public\\|internal\\|private\\|override\\|virtual\\|static\\|protected\\)[ \t]*\\)+[^ \t]+[ \t]*(")
      (looking-at "[ \t]*[^ \t]+[ \t]*(.*)[ \t]*:")
      (looking-at "[ \t]*def[ \t]*[^ \t]+[ \t]*([^{]*$")))
	 

(defun nemerle-if-add-indent (line)
  "Try to infer where indentation-based syntax need to increase indent."
  (cond ((or
	  (looking-at ".*\\b\\(if\\|for\\|foreach\\|while\\|when\\|unless\\)[ \t]*(")
	  (looking-at "[ \t]*namespace\\b")
	  (looking-at ".*\\b\\(class\\|struct\\|module\\|try\\)\\b")
	  (nemerle-looking-at-function)
	  (looking-at "[ \t]*else\\b")
	  (looking-at ".*\\bmatch[ \t]*(")
	  (looking-at "[ \t]*catch\\b")
	  (looking-at "[ \t]*finally\b"))
	 nemerle-basic-offset)
	((looking-at "[ \t]*|")
	 nemerle-match-case-offset)
	(t
	 0)))


(defun nemerle-calculate-dedent (line)
  "Try to infer, if we need to decrease indentation in indentation-based syntax."
  (cond ((eq line 'star-comment)
	 -1)
	(t
	 0)))


(defun nemerle-find-try-clause ()
  "Finds and moves to the try clause."
  (let ((depth 1)
	(too-far nil))
      ;; depth is number of unmatched else's. We start with value of
      ;; of 1 - the else line at end, and proceed until it reaches 0
      (while (and (not (eq depth 0)) (not too-far))
	(nemerle-go-backwards (point-min))
	(if (eq (point-min) (point)) ;; oops - too far
	    (setq too-far t))
	(if (looking-at "[ \t]*catch\\b")
	    (setq depth (+ depth 1))
	  (if (looking-at ".*\\btry\\b")
	      (setq depth (- depth 1)))))))


(defun nemerle-find-match-clause (current)
  "Find matching match clause."
  (nemerle-go-backwards (point-min))
  (let ((indent (current-indentation))
	(previous current))
    (while (and (or (>= indent previous)
		    (and (not (looking-at ".*\\bmatch[ \t]*("))
			 (not (looking-at "[ \t]*catch\\b"))
			 (not (nemerle-looking-at-function))))
		(not (eq (point) (point-min))))
      (setq previous (min previous indent))
      (nemerle-go-backwards (point-min))
      (setq indent (current-indentation)))))



(defun nemerle-calculate-indentation-of-line (line)
  "Return the absolute indentation for the line at the current point,
where its syntactic meaning is given by LINE, and may not be IN-STRING."
  (save-excursion 
    (beginning-of-line)
    (cond ((and nemerle-indentation-based-syntax
		(nemerle-is-not-indentation-exception))
	   (cond ((eq line 'else-clause)
		  (goto-char (nemerle-find-if (point-min) (point)))
		  (current-indentation))
		 ((eq line 'match-case)
		  (nemerle-find-match-clause (current-indentation))
		  (+ (current-indentation) nemerle-basic-offset))
		 ((eq line 'catch-clause)
		  (nemerle-find-try-clause)
		  (current-indentation))
		 (t
		  (nemerle-go-backwards (point-min) t)
		  (if (or (eq line 'begin-brace)
			  (looking-at "[ \t]*//")
			  (looking-at ".*\\\\[ \t]*$"))
		      (current-indentation)
		    (- (+ (current-indentation) (nemerle-if-add-indent line))
		       (nemerle-calculate-dedent line))))))
	  (t
	   (let ((end (point))
		 (paren-char (nemerle-go-up-one-level))
		 (top-indentation (current-indentation))
		 (paren-column (- (point) 
				  (save-excursion (beginning-of-line) (point)))))
	     (nemerle-skip-sexps end)
	     (cond ((eq paren-char ?{)
		    (+ top-indentation (nemerle-get-nested end line)))
		   ((eq paren-char 0)
		    (nemerle-get-offset end line))
		   (t
		    (if nemerle-indent-expr-to-paren
			(1+ paren-column)
		      (+ (+ top-indentation nemerle-basic-offset) (nemerle-get-nested end line))))))))))


(defun nemerle-calculate-indentation ()
  "Return the absolute indentation for the line at the current point."
  (let ((line (nemerle-analyze-line)))
    (if (eq line 'in-string)
	(current-indentation)
      (nemerle-calculate-indentation-of-line line))))


(defun nemerle-indent-to (level)
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (if (< level (current-indentation))
	(delete-horizontal-space))
    (indent-to level))
  (if (< (current-column) (current-indentation))
      (skip-chars-forward " \t")))


(defun nemerle-indent-line ()
  "Indent current line of nemerle code."
  (interactive)
  (let ((level (nemerle-calculate-indentation)))
    (nemerle-indent-to level)))


(defun nemerle-change-indentation (shift)
  (let ((level (+ (current-indentation) shift)))
    (if (< level 0)
	(setq level 0))
    (nemerle-indent-to level)))


(defun nemerle-electric-tab ()
  "If indentation-based syntax is turned on and the point is not
inside any parens and it is not only brace opening or closing,
increase indentation level. Otherwise indent line."
  (interactive)
  (beginning-of-line)
  (cond ((and nemerle-indentation-based-syntax
	   (not (looking-at "[ \t]*\\({\\|}\\)"))
	   (nemerle-is-not-indentation-exception))
	 (let ((current (current-indentation))
	       (preceding 0)
	       (suggested (nemerle-calculate-indentation)))
	   (save-excursion
	     (nemerle-go-backwards (point-min))
	     (setq preceding (current-indentation)))
	   (if (> current preceding)
	       (nemerle-indent-to (min suggested (max 0 (- preceding nemerle-basic-offset))))
	     (if (eq current preceding)
		 (nemerle-indent-to
		  (if (<= suggested current)
		      (+ current nemerle-basic-offset)
		    suggested))
	       (nemerle-indent-to preceding)))))
	(t
	 (nemerle-indent-line))))


(defun nemerle-electric-delete (arg)
  "Normally behaves like backspace, but if the point is at first
non-blank character on line and indentation based syntax should be
applied, delete characters up to line begin or next indentation level,
whichever comes first."
  (interactive "p")
  (let ((here (point))
	(paren-column (backward-to-indentation 0))
	(count arg))
    (if (and nemerle-indentation-based-syntax
	     (eq (point) here)
	     (nemerle-is-not-indentation-exception))
	(setq count (max (min nemerle-basic-offset paren-column) 1)))
    (goto-char here)
    (backward-delete-char-untabify count nil)))


(defun nemerle-shift-region-right (start end)
  "Shift selected region to the right by indentation level."
  (interactive "r")
  (nemerle-shift-region start end nemerle-basic-offset))


(defun nemerle-shift-region-left (start end)
  "Shift selected region to the left by indentation level."
  (interactive "r")
  (nemerle-shift-region start end (- nemerle-basic-offset)))


(defun nemerle-shift-region (start end shift)
  "Shift given region."
  (goto-char start)
  (nemerle-change-indentation shift)
  (while (and (eq (forward-line 1) 0) (<= (point) end))
    (nemerle-change-indentation shift))
  (goto-char end))
      

(defun nemerle-electric-bar (arg)
  "Insert a bar.

Also, the line is re-indented unless a numeric ARG is supplied."
  (interactive "p")
  (if (or (and arg (> arg 1)) (not (nemerle-on-empty-line)))
      (self-insert-command (or arg 1))
    (if (or (and nemerle-indentation-based-syntax
		 (nemerle-is-not-indentation-exception))
	    (nemerle-in-match))
	(let ((level (nemerle-calculate-indentation-of-line 'match-case)))
	  (nemerle-indent-to level))
      (let ((level (nemerle-calculate-indentation-of-line 'none)))
	(nemerle-indent-to level)))
    (self-insert-command 1)))


(defun nemerle-electric-e (arg)
  "Insert letter e and force reindent if it's else-clause."
  (interactive "p")
  (self-insert-command (or arg 1))
   (save-excursion
     (beginning-of-line)
     (cond ((looking-at "[ \t]*else\\b")
 	   (let ((level (nemerle-calculate-indentation-of-line 'else-clause)))
 	     (nemerle-indent-to level))))))


(defun nemerle-electric-h (arg)
  "Insert letter e and force reindent if it's else-clause."
  (interactive "p")
  (self-insert-command (or arg 1))
   (save-excursion
     (beginning-of-line)
     (cond ((looking-at "[ \t]*catch\\b")
 	   (let ((level (nemerle-calculate-indentation-of-line 'catch-clause)))
 	     (nemerle-indent-to level))))))


;; Common part of brace handling. Functions for specific brace (begin, end)
;; need just to pass appropriate MEANING param.
(defun nemerle-electric-brace (arg meaning)
  "Insert a brace.

Also, the line is re-indented unless a numeric ARG is supplied.
MEANING is the symbol denoting syntax meaning of current line:
begin-brace or end-brace."
  (if (or (and arg (> arg 1)) (not (nemerle-on-empty-line)))
      (self-insert-command (or arg 1))
    (let ((level (nemerle-calculate-indentation-of-line meaning)))
      (nemerle-indent-to level))
    (self-insert-command 1)))


(defun nemerle-electric-brace-begin (arg)
  "Insert a brace.

Also, the line is re-indented unless a numeric ARG is supplied."
  (interactive "p")
  (nemerle-electric-brace arg 'begin-brace))


(defun nemerle-electric-brace-end (arg)
  "Insert a brace.

Also, the line is re-indented unless a numeric ARG is supplied."
  (interactive "p")
  (nemerle-electric-brace arg 'end-brace))


(defun nemerle-electric-star (arg)
  "Insert an asterisk.

Also, the line is re-indented if inside a comment."
  (interactive "p")
  (if (or (and arg (> arg 1)) (not (nemerle-in-comment)) 
	  (not (nemerle-on-empty-line)))
      (self-insert-command (or arg 1))
    (let ((level (nemerle-calculate-indentation-of-line 'star-comment)))
      (nemerle-indent-to level))
    (self-insert-command 1)))


(defun nemerle-comment-indent ()
  (interactive)
  nil)


(defun nemerle-mode ()
  "Major mode for editing nemerle source files.

Mode map
========
\\{nemerle-mode-map}"

  (interactive)
  (kill-all-local-variables)

  (if nemerle-indentation-based-syntax
      (setq mode-name "Nemerle Indent.")
    (setq mode-name "Nemerle"))
  (setq major-mode 'nemerle-mode)

  (use-local-map nemerle-mode-map)
  (set-syntax-table nemerle-mode-syntax-table)

  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(nemerle-font-lock-keywords nil nil
			     ((?_ . "w") (?. . "w") (?\' . "."))))
  
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'nemerle-indent-line)

  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'nemerle-comment-indent)

  (make-local-variable 'comment-start)
  (setq comment-start "// ")

  (make-local-variable 'comment-end)
  (setq comment-end "")

  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "/\\*+[ \t]*\\|//+ *")

  (setq buffer-file-coding-system 'utf-8)
  (setq indent-tabs-mode nil)
  
  (run-hooks 'nemerle-mode-hook))

;;; nemerle.el ends here
