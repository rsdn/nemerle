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

;; (setq load-path (cons "/path/to/nemerle.el" load-path))
;; (autoload 'nemerle-mode "nemerle-mode"
;;   "Major mode for editing nemerle programs." t)
;; (setq auto-mode-alist (cons '("\\.n$" . nemerle-mode) auto-mode-alist))



;;; Change Log:

;; 2003-10-05 rzyjontko <rzyj@plusnet.pl>
;;   * initial version



;;; Todo:

;; - syntax table
;; - indentation
;; - imenu



;;; Code:

(provide 'nemerle-mode)


(defvar nemerle-mode-map nil
  "*The keymap used in nemerle-mode.")

(defvar nemerle-font-lock-keywords nil
  "*Font lock definitions for nemerle-mode.")

(defvar nemerle-mode-hook nil
  "*This hook is run when nemerle-mode is loaded, or a new nemerle-mode
buffer created.  This is a good place to put your customizations.")

(unless nemerle-mode-map
  (let ((map (make-sparse-keymap)))
    ;; this is where to put keybindings like this
    ;; (define-key map "\C-c\C-a" 'something)
    (setq nemerle-mode-map map)))

(unless nemerle-font-lock-keywords
  (setq nemerle-font-lock-keywords
	(list
	 '("//.*" (0 font-lock-comment-face))
	 '("(\\*+\\(\\([^\\*]\\)\\|\\(\\*[^)]\\)\\)+\\*+)" 
	   (0 font-lock-comment-face))
	 '("\"[^\"]*\"" (0 font-lock-string-face))
	 '("\\(\\b\\(abstract\\|and\\|as\\|base\\|class\\|const\\|else\\|enum\\|extends\\|extern\\|field\\|finally\\|fun\\|if\\|implements\\|in\\|interface\\|internal\\|let\\|letfun\\|match\\|method\\|namespace\\|new\\|null\\|open\\|out\\|private\\|protected\\|public\\|raise\\|record\\|ref\\|sealed\\|struct\\|then\\|this\\|try\\|tymatch\\|type\\|value\\|volatile\\|where\\|with\\)\\b\\)\\|\\(\\B_\\B\\)"
	   (0 font-lock-keyword-face)))))


(defun nemerle-indent-line ()
  )  


(defun nemerle-comment-indent ()
  )


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
  (setq comment-start-skip "(\\*+[ \t]*")

  (run-hooks 'nemerle-mode-hook))

;;; nemerle.el ends here
