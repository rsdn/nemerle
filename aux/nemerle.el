;;; nemerle.el -- major mode for editing nemerle programs

;; Copyright (C) 2003 University of Wroclaw

;; Author: Jacek Sliwerski (rzyjontko) <rzyj@plusnet.pl>
;; Maintainer: Jacek Sliwerski (rzyjontko) <rzyj@plusnet.pl>
;; Created: 5 Oct 2003
;; Version: 0.1
;; Keywords: nemerle, mode
;; Website: http://nemerle.org

;; This file is not part of GNU Emacs, but it is distributed under
;; the same conditions.



;;; Commentary:

;; A major mode for editing nemerle source files.  It provides syntax
;; hilighting, proper indentation, and many other features.
;; To install the nemerle mode, put the following lines into your
;; ~/.emacs file:

;; (setq load-path (cons "/path/to/where/this/file/resides" load-path))
;; (autoload 'nemerle-mode "nemerle-mode"
;;   "Major mode for editing nemerle programs." t)
;; (setq auto-mode-alist (cons '("\\.n$" . nemerle-mode) auto-mode-alist))



;;; Change Log:

;; 2003-10-09 rzyjontko <rzyj@plusnet.pl>
;;   * nemerle mode automatically sets file coding system to utf-8
;;   * syntax table changes
;;   * more colours
;;   * indentation framework

;; 2003-10-05 rzyjontko <rzyj@plusnet.pl>
;;   * initial version



;;; Todo:

;; - indentation
;; - imenu



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

(defvar nemerle-indent-level 4
  "Indentation of blocks in nemerle.")

(unless nemerle-mode-map
  (let ((map (make-sparse-keymap)))
    ;; this is where to put keybindings like this
    ;; (define-key map "\C-c\C-a" 'something)
    (setq nemerle-mode-map map)))

(unless nemerle-font-lock-keywords
  (setq nemerle-font-lock-keywords
	(list
	 ;; comments
	 '("//.*" 0 font-lock-comment-face)
	 '("(\\*+\\(\\([^\\*]\\)\\|\\(\\*[^)]\\)\\)+\\*+)" 
	   0 font-lock-comment-face)
	 
	 ;; strings
	 '("\"[^\"]*\"" 0 font-lock-string-face)

	 ;; keywords
	 '("\\b\\(_\\|abstract\\|and\\|as\\|base\\|class\\|const\\|else\\|enum\\|extends\\|extern\\|field\\|finally\\|fun\\|if\\|implements\\|in\\|interface\\|internal\\|let\\|letfun\\|match\\|method\\|namespace\\|new\\|null\\|open\\|out\\|private\\|protected\\|public\\|raise\\|record\\|ref\\|sealed\\|struct\\|then\\|this\\|try\\|tymatch\\|type\\|value\\|variant\\|volatile\\|where\\|with\\)\\b"
	   0 font-lock-keyword-face)

	 ;; types
	 '("\\b\\(void\\|int\\|char\\|float\\|bool\\|string\\|option\\|list\\|object\\)\\b"
	   0 font-lock-type-face)
	 '("\\btype\\s +\\(\\w+\\)" 1 font-lock-type-face)
	 '("'\\w+" 0 font-lock-type-face)

	 ;; constants
	 '("\\b\\(false\\|true\\)\\b" 0 font-lock-constant-face)

	 ;; variables
	 '("``\\w+``" 0 font-lock-variable-name-face)
	 '("\\b\\(\\w+\\)\\ *:" 1 font-lock-variable-name-face)
	 '("\\bmatch\\s +\\(\\w+\\)" 1 font-lock-variable-name-face)
	 '("\\blet\\s +\\(\\w+\\)" 1 font-lock-variable-name-face)

	 ;; functions
	 '("\\b\\(class\\|variant\\|open\\|interface\\|namespace\\|extends\\|implements\\)\\s +\\(\\w+\\)" 
	   2 font-lock-function-name-face)
	 '("\\b\\(\\(let\\)?fun\\|method\\)\\b\\s *\\(([^)]*)\\s *\\)?\\(\\w+\\)" 
	   4 font-lock-function-name-face))))

(unless nemerle-mode-syntax-table
  (setq nemerle-mode-syntax-table (copy-syntax-table c-mode-syntax-table))
  
  (modify-syntax-entry ?\{ "(}"   nemerle-mode-syntax-table)
  (modify-syntax-entry ?\} "){"   nemerle-mode-syntax-table)
  (modify-syntax-entry ?\[ "(]"   nemerle-mode-syntax-table)
  (modify-syntax-entry ?\] ")["   nemerle-mode-syntax-table)
  (modify-syntax-entry ?\" ".\""  nemerle-mode-syntax-table)
  (modify-syntax-entry ?\, "."    nemerle-mode-syntax-table)
  (modify-syntax-entry ?\` "."    nemerle-mode-syntax-table)
  (modify-syntax-entry ?\( "()1"  nemerle-mode-syntax-table)
  (modify-syntax-entry ?\) ")(4"  nemerle-mode-syntax-table)
  (modify-syntax-entry ?\* "w 23" nemerle-mode-syntax-table)
  (modify-syntax-entry ?\? "w"    nemerle-mode-syntax-table)
  (modify-syntax-entry ?\= "w"    nemerle-mode-syntax-table)
  (modify-syntax-entry ?\< "w"    nemerle-mode-syntax-table)
  (modify-syntax-entry ?\> "w"    nemerle-mode-syntax-table)
  (modify-syntax-entry ?\@ "w"    nemerle-mode-syntax-table)
  (modify-syntax-entry ?\^ "w"    nemerle-mode-syntax-table)
  (modify-syntax-entry ?\| "w"    nemerle-mode-syntax-table)
  (modify-syntax-entry ?\& "w"    nemerle-mode-syntax-table)
  (modify-syntax-entry ?\+ "w"    nemerle-mode-syntax-table)
  (modify-syntax-entry ?\- "w"    nemerle-mode-syntax-table)
  (modify-syntax-entry ?\/ "w"    nemerle-mode-syntax-table)
  (modify-syntax-entry ?\$ "w"    nemerle-mode-syntax-table)
  (modify-syntax-entry ?\% "w"    nemerle-mode-syntax-table)
  (modify-syntax-entry ?\! "w"    nemerle-mode-syntax-table)
  (modify-syntax-entry ?\~ "w"    nemerle-mode-syntax-table)
  (modify-syntax-entry ?\. "w"    nemerle-mode-syntax-table)
  (modify-syntax-entry ?\: "w"    nemerle-mode-syntax-table)
  (modify-syntax-entry ?\_ "w"    nemerle-mode-syntax-table)
  (modify-syntax-entry ?\' "w"    nemerle-mode-syntax-table))



(defun nemerle-calculate-indentation ()
  "Calculates indentation level for the current line."
  (current-indentation))


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
  (setq font-lock-defaults (list 'nemerle-font-lock-keywords nil t))
  
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'nemerle-indent-line)

  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'nemerle-comment-indent)

  (make-local-variable 'comment-start)
  (setq comment-start "(* ")

  (make-local-variable 'comment-end)
  (setq comment-end " *)")

  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "(\\*+[ \t]*\\|//+ *")

  (setq buffer-file-coding-system 'utf-8)
  
  (run-hooks 'nemerle-mode-hook))

;;; nemerle.el ends here
