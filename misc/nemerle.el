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



;;; Change Log:

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

(unless nemerle-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'comment-region)
    (define-key map "|" 'nemerle-electric-bar)
    (define-key map "}" 'nemerle-electric-brace)
    (define-key map "*" 'nemerle-electric-star)
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
	  (brace (nemerle-go-up-one-level)))
      (if (not (eq brace ?{))
	  nil
	(beginning-of-line)
	(while (and (not at-end) (<= (point) end))
	  (setq line (nemerle-analyze-line))
	  (if (eq line 'match-case)
	      (setq in-match-case t))
	  (if (> (forward-line 1) 0)
	      (setq at-end t)))
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
	  ((looking-at "[ \t]*}")
	   'end-brace)
	  ((looking-at "[ \t]*|")
	   'match-case)
	  (t
	   'none))))


(defun nemerle-analyze-block (end result)
  "Analyze the block from current point position until END.  Return
the relative offset + result for the block."
  (beginning-of-line)
  (let ((line 'none)
	(in-match-case nil)
	(at-end nil))
    (while (and (not at-end) (<= (point) end))
      (setq line (nemerle-analyze-line))
      (cond ((eq line 'match-case)
	     (setq in-match-case t))
	    (t
	     nil))
      (if (> (forward-line 1) 0)
	  (setq at-end t)))
    (if in-match-case
	(setq result (+ result nemerle-match-case-offset)))
    result))


(defun nemerle-get-offset (end line)
  "Return the relative offset for the block from the current point
position until END, where the last line has syntactic meaning given 
by LINE."
  (cond ((eq line 'end-brace)
	 0)
	((eq line 'comment)
	 (nemerle-analyze-block end 2))
	((eq line 'star-comment)
	 (nemerle-analyze-block end 1))
	((eq line 'match-case)
	 (nemerle-analyze-block end (- nemerle-match-case-offset)))
	(t
	 (nemerle-analyze-block end 0))))


(defun nemerle-get-nested (end line)
  "Retrun the relative offset for the line LINE nested in the block
of code.  Analyze code from the current point position until END."
  (if (eq line 'end-brace)
      0
    (+ nemerle-basic-offset (nemerle-get-offset end line))))


(defun nemerle-calculate-indentation-of-line (line)
  "Return the absolute indentation for the line at the current point,
where its syntactic meaning is given by LINE, and may not be IN-STRING."
  (save-excursion 
    (beginning-of-line)
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
	     (1+ paren-column))))))


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


(defun nemerle-electric-bar (arg)
  "Insert a bar.

Also, the line is re-indented unless a numeric ARG is supplied."
  (interactive "p")
  (if (or (and arg (> arg 1)) (not (nemerle-on-empty-line)))
      (self-insert-command (or arg 1))
    (if (nemerle-in-match)
	(let ((level (nemerle-calculate-indentation-of-line 'match-case)))
	  (nemerle-indent-to level))
      (let ((level (nemerle-calculate-indentation-of-line 'none)))
	(nemerle-indent-to level)))
    (self-insert-command 1)))
	

(defun nemerle-electric-brace (arg)
  "Insert a brace.

Also, the line is re-indented unless a numeric ARG is supplied."
  (interactive "p")
  (if (or (and arg (> arg 1)) (not (nemerle-on-empty-line)))
      (self-insert-command (or arg 1))
    (let ((level (nemerle-calculate-indentation-of-line 'end-brace)))
      (nemerle-indent-to level))
    (self-insert-command 1)))


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
  
  (setq mode-name "Nemerle")
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
  (setq comment-start "/* ")

  (make-local-variable 'comment-end)
  (setq comment-end " */")

  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "/\\*+[ \t]*\\|//+ *")

  (setq buffer-file-coding-system 'utf-8)
  (setq indent-tabs-mode nil)
  
  (run-hooks 'nemerle-mode-hook))

;;; nemerle.el ends here
