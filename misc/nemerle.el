;;; nemerle.el -- major mode for editing nemerle programs

;; Copyright (C) 2003, 2004 The University of Wroclaw
;; All rights reserved.

;; Author: Jacek Sliwerski (rzyjontko) <rzyj@plusnet.pl>
;; Maintainer: Jacek Sliwerski (rzyjontko) <rzyj@plusnet.pl>
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

;; 2004-01-21 rzyjontko <rzyj@plusnet.pl>
;;   * improved indentation
;;   * changed syntax table
;;   * disabled tab-indent
;;   * switched to new grammar

;; 2003-11-17 rzyjontko <rzyj@plusnet.pl>
;;   * updated copyright disclaimer
;;   * basic indentation engine

;; 2003-10-09 rzyjontko <rzyj@plusnet.pl>
;;   * nemerle mode automatically sets file coding system to utf-8
;;   * syntax table changes
;;   * more colours
;;   * indentation framework

;; 2003-10-05 rzyjontko <rzyj@plusnet.pl>
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
    (setq nemerle-mode-map map)))

(unless nemerle-font-lock-keywords
  (setq nemerle-font-lock-keywords
	(list
	 ;; one-line comments
	 '("//.*" 0 font-lock-comment-face)
	 ;; old-style multiline comments
	 '("(\\*+\\(\\([^\\*]\\)\\|\\(\\*[^)]\\)\\)+\\*+)" 
	   0 font-lock-comment-face)
	 ;; new-style multiline comments
	 '("/\\*+\\(\\([^\\*]\\)\\|\\(\\*[^/]\\)\\)+\\*+/"
	   0 font-lock-comment-face)
	 
	 ;; strings
	 '("'[^\\']'\\|'\\\\.'" 0 font-lock-string-face)
	 '("\"[^\"]*\"" 0 font-lock-string-face)

	 ;; keywords
	 '("\\<\\(_\\|abstract\\|and\\|as\\|base\\|const\\|def\\|else\\|enum\\|extends\\|extern\\|finally\\|fun\\|implements\\|in\\|interface\\|internal\\|macro\\|match\\|mkarray\\|mutable\\|new\\|null\\|out\\|override\\|params\\|private\\|protected\\|public\\|raise\\|ref\\|sealed\\|static\\|struct\\|syntax\\|this\\|try\\|tymatch\\|type\\|typeof\\|virtual\\|volatile\\|when\\|where\\|with\\)\\>"
	   0 font-lock-keyword-face)
	 ;; these aren't really keywords but we set them so
	 '("\\<\\(do\\|for\\|if\\|regexp\\|repeat\\|then\\|unless\\|until\\|using\\|while\\)\\>"
	   0 font-lock-keyword-face)

	 '("\\<\\(open\\|variant\\|class\\|module\\|namespace\\)\\s +\\(\\(\\w\\|\\.\\)*\\)"
	   (1 font-lock-keyword-face) (2 font-lock-function-name-face))
	 
	 ;; types
	 '("\\<\\(void\\|int\\|char\\|float\\|bool\\|string\\|object\\)\\>"
	   0 font-lock-type-face)
	 '("\\<list\\s *([^)]*)" 0 font-lock-type-face t)
	 '("\\<option\\s *([^)]*)" 0 font-lock-type-face t)
	 '("\\<array\\s *([^)]*)" 0 font-lock-type-face t)
	 '("'\\w+" 0 font-lock-type-face t)
	 
	 ;; constants
         '("\\[\\]" 0 font-lock-constant-face)
	 '("\\<\\(false\\|true\\)\\>" 0 font-lock-constant-face))))



(unless nemerle-mode-syntax-table
  (setq nemerle-mode-syntax-table (copy-syntax-table c-mode-syntax-table))
  
  (modify-syntax-entry ?\( "()1"  nemerle-mode-syntax-table)
  (modify-syntax-entry ?\) ")(4"  nemerle-mode-syntax-table)
  (modify-syntax-entry ?\* ". 23" nemerle-mode-syntax-table)
  (modify-syntax-entry ?\/ ". 14" nemerle-mode-syntax-table)
  (modify-syntax-entry ?_  "w"    nemerle-mode-syntax-table)
  (modify-syntax-entry ?\' "_"    nemerle-mode-syntax-table))



(defun nemerle-syntax ()
  "Returns syntactic information."
  (save-excursion
    (beginning-of-line)
    (cond ((looking-at "[^\n]*}")
	   'block-end)
	  ((looking-at "[^\n]*{")
	   'block-beg)
	  ((looking-at "[ \t]*\\<try\\>")
	   'try)
	  ((looking-at "[ \t]*\\<with\\>")
	   'with)
	  ((looking-at "[ \t]*|")
	   'match-case)
	  ((looking-at "[ \t]*$")
	   'empty)
	  ((looking-at "[ \t]*\\<if\\>")
	   'if)
	  ((looking-at "[ \t]*\\<else\\>")
	   'else)
	  (t 
	   'other))))


(defun nemerle-prev-line ()
  "Calculates indentation level for the previous line, and checks
its syntatctic information."
  (save-excursion
    (beginning-of-line)
    (if (bobp)
	0
      (skip-chars-backward " \t\n")
      (beginning-of-line)
      (let ((syntax (nemerle-syntax))
	    (indent (current-indentation)))
	(cons indent syntax)))))


(defun nemerle-calculate-indentation ()
  "Calculates indentation level for the current line."
  (let* ((prev-info (nemerle-prev-line))
	 (prev-indent (car prev-info))
	 (prev-syntax (cdr prev-info))
	 (cur-syntax (nemerle-syntax)))
    (cond ((eq prev-syntax 'match-case)	; match-case
	   (cond ((eq cur-syntax 'match-case)
		  prev-indent)
		 ((eq cur-syntax 'block-end)
		  (- prev-indent nemerle-basic-offset))
		 (t
		  (+ prev-indent 2))))
	  ((eq prev-syntax 'try)	; try
	   (cond ((eq cur-syntax 'block-beg)
		  prev-indent)
		 ((eq cur-syntax 'with)
		  prev-indent)
		 (t (+ prev-indent nemerle-basic-offset))))
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
		  (t
		   prev-indent))))))


(defun nemerle-indent-line ()
  "Indent current line of nemerle code."
  (interactive)
  (let ((level (nemerle-calculate-indentation)))
    (save-excursion
      (beginning-of-line)
      (skip-chars-forward " \t")
      (if (< level (current-indentation))
	  (delete-horizontal-space))
      (indent-to level))
    (if (< (current-column) (current-indentation))
	(skip-chars-forward " \t"))))
		   


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
  (setq font-lock-defaults '(nemerle-font-lock-keywords nil t))
  
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
