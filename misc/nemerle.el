;;; nemerle.el -- major mode for editing nemerle programs

;; Copyright (C) 2003, 2004 The University of Wroclaw
;; All rights reserved.

;; Author: Jacek Sliwerski (rzyjontko) <rzyj@o2.pl>
;; Maintainer: Jacek Sliwerski (rzyjontko) <rzyj@o2.pl>
;; Created: 5 Oct 2003
;; Version: 0.1
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
;;   (setq nemerle-basic-offset 2)
;;   (define-key nemerle-mode-map "\C-m" 'newline-and-indent))
;; (add-hook 'nemerle-mode-hook 'my-nemerle-mode-hook)



;;; Change Log:

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

(unless nemerle-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'comment-region)
    (define-key map "|" 'nemerle-electric-bar)
    (define-key map "}" 'nemerle-electric-brace)
    (setq nemerle-mode-map map)))

(unless nemerle-font-lock-keywords
  (setq nemerle-font-lock-keywords
	(list
	 ;; strings
	 '("[^']\\(\"[^\"]*\"\\)" 1 font-lock-string-face)
	 '("'[^\\']'\\|'\\\\.'" 0 font-lock-string-face)

	 ;; one-line comments
	 '("//.*" 0 font-lock-comment-face)
	 
	 ;; keywords
	 '("\\<\\(_\\|abstract\\|and\\|as\\|base\\|catch\\|const\\|def\\|delegate\\|enum\\|extends\\|extern\\|finally\\|fun\\|implements\\|interface\\|internal\\|is\\|macro\\|match\\|matches\\|mutable\\|new\\|out\\|override\\|params\\|private\\|protected\\|public\\|ref\\|sealed\\|static\\|struct\\|syntax\\|this\\|throw\\|try\\|tymatch\\|type\\|typeof\\|virtual\\|where\\|partial\\)\\>"
	   0 font-lock-keyword-face)
	 ;; these aren't really keywords but we set them so
	 '("\\<\\(do\\|else\\|for\\|if\\|regexp\\|unless\\|while\\|when\\|in\\|foreach\\)\\>"
	   0 font-lock-keyword-face)
	 '("=>\\||" 0 font-lock-keyword-face)

	 '("\\<\\(foreach\\)\\s *(.*:\\s *\\(\\w*\\)\\s *\\(\\<in\\>\\)"
	   (1 font-lock-keyword-face) (2 font-lock-type-face) (3 font-lock-keyword-face))
	 
	 '("\\<\\(variant\\|class\\|interface\\|module\\|namespace\\|using\\)\\s +\\(\\(\\w\\|\\.\\)*\\)"
	   (1 font-lock-keyword-face) (2 font-lock-function-name-face))
	 
	 ;; types
	 '("\\<list\\s *<[^>]*[^\\-]>+" 0 font-lock-type-face t)
	 '("\\<option\\s *<[^>]*>+" 0 font-lock-type-face t)
	 '("\\<array\\s *<[^>]*>+" 0 font-lock-type-face t)
	 '("->" 0 font-lock-type-face)
	 '("\\<\\(void\\|int\\|uint\\|char\\|float\\|double\\|decimal\\|byte\\|sbyte\\|short\\|ushort\\|long\\|ulong\\|bool\\|string\\|object\\)\\>"
	   0 font-lock-type-face)
	 
	 ;; constants
         '("\\<[0-9]+\\>" 0 font-lock-constant-face)
	 '("\\<Nil\\>" 0 font-lock-constant-face)
	 '("\\<\\(false\\|true\\|null\\)\\>" 0 font-lock-constant-face))))


(unless nemerle-mode-syntax-table
  (setq nemerle-mode-syntax-table (copy-syntax-table c-mode-syntax-table))
  (modify-syntax-entry ?< "(>" nemerle-mode-syntax-table)
  (modify-syntax-entry ?> ")<" nemerle-mode-syntax-table))


(defun nemerle-syntax ()
  (save-excursion
    (beginning-of-line)
    (cond ((looking-at "[ \t]*\\<try\\>")
	   'try)
	  ((looking-at "[ \t]*\\<catch\\>")
	   'catch)
	  ((looking-at "[ \t]*|")
	   'match-case)
	  ((looking-at "[ \t]*$")
	   'empty)
	  ((looking-at "[ \t]*\\<if\\>")
	   'if)
	  ((looking-at "[ \t]*\\<else\\>")
	   'else)
	  ((looking-at "[^\n{]*}")
	   'block-end)
	  ((looking-at "[^\n]*{[^\n}]*$")
	   'block-beg)
	  ((looking-at "[ \t]*\\<when\\>")
	   'if)
	  ((looking-at "[ \t]*\\<unless\\>")
	   'if)
	  (t 
	   'other))))


(defun nemerle-prev-line ()
  (save-excursion
    (beginning-of-line)
    (if (bobp)
	0
      (let ((there (point)))
	(skip-chars-backward " \t\n")
	(beginning-of-line)
	(let* ((here (point))
	       (syntax (nemerle-syntax))
	       (indent (current-indentation))
	       (state (parse-partial-sexp here there)))
	  (cond ((and (< (nth 0 state) 0) (eq ?\) (nth 2 state)))
		 (goto-char (scan-lists (nth 2 state) -1 1))
		 (cons (current-indentation) (nemerle-syntax)))
	        ((null (nth 1 state))
		 (cons indent syntax))
		((eq (char-after (nth 1 state)) ?\()
		 (cons (- (nth 1 state) here) 'open-paren))
		(t
		 (cons indent syntax))))))))


(defun nemerle-calculate-indentation ()
  (let* ((prev-info (nemerle-prev-line))
	 (prev-indent (car prev-info))
	 (prev-syntax (cdr prev-info))
	 (cur-syntax (nemerle-syntax)))
    (cond ((eq prev-syntax 'open-paren)
	   (1+ prev-indent))
	  ((eq prev-syntax 'match-case)	; match-case
	   (cond ((eq cur-syntax 'match-case)
		  prev-indent)
		 ((eq cur-syntax 'block-end)
		  (- prev-indent nemerle-basic-offset))
		 (t
		  (+ prev-indent 2))))
	  ((eq prev-syntax 'try)	; try
	   (cond ((eq cur-syntax 'block-beg)
		  prev-indent)
		 ((eq cur-syntax 'catch)
		  prev-indent)
		 (t (+ prev-indent nemerle-basic-offset))))
	  ((eq prev-syntax 'catch)
	   (+ prev-indent nemerle-basic-offset))
	  ((eq prev-syntax 'block-beg)	; beginning of block
	   (+ prev-indent nemerle-basic-offset))
	  ((eq prev-syntax 'block-end) 	; end of block
	   (cond ((eq cur-syntax 'block-end)
		  (- prev-indent nemerle-basic-offset))
		 (t
		  prev-indent)))
	  ((eq prev-syntax 'if)		; if
	   (+ prev-indent nemerle-basic-offset))
	  ((eq prev-syntax 'else)	; else
	   (+ prev-indent nemerle-basic-offset))
	  (t
	    (cond ((eq cur-syntax 'block-end)
		   (- prev-indent nemerle-basic-offset))
		  ((eq cur-syntax 'else)
		   (- prev-indent nemerle-basic-offset))
		  ((eq cur-syntax 'catch)
		   (- prev-indent nemerle-basic-offset))
		  (t
		   prev-indent))))))


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

Also, the line is re-indented unless a numeric ARG is supplied 
or there are some non-blank symbols on the line."
  (interactive "p")
  (if (or (not (eq (nemerle-syntax) 'empty)) (and arg (> arg 1)))
      (self-insert-command (or arg 1))
    (message "ok")
    (let* ((prev-info (nemerle-prev-line))
	   (prev-indent (car prev-info))
	   (prev-syntax (cdr prev-info))
	   (level prev-indent))
      (if (eq prev-syntax 'block-beg)
	  (setq level (+ prev-indent nemerle-basic-offset)))
      (nemerle-indent-to level)
      (insert-char ?| 1))))
	

(defun nemerle-electric-brace (arg)
  "Insert a brace.

Also, the line is re-indented unless a numeric ARG is supplied
or there are some non-blank symbols on the line."
  (interactive "p")
  (if (or (not (eq (nemerle-syntax) 'empty)) (and arg (> arg 1)))
      (self-insert-command (or arg 1))
    (let* ((prev-info (nemerle-prev-line))
	   (prev-indent (car prev-info))
	   (prev-syntax (cdr prev-info))
	   (level prev-indent))
      (nemerle-indent-to (- prev-indent nemerle-basic-offset))
      (insert-char ?} 1))))


(defun nemerle-comment-indent ()
  "Indent current line of nemerle comment."
  (interactive)
  0)


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
			     ((?_ . "w") (?. . "w") (?\/ . ". 14b")
			      (?* . ". 23") (?\" . ".") (?\' . "."))))
  
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'nemerle-indent-line)

  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'c-comment-indent)

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
