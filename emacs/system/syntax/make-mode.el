;; Emacs configuration file
;; Syntax colouring regular expressions for make-mode
;; Written by Martin Ebourne
;; $Id: make-mode.el,v 1.1 2001/05/11 17:31:38 mebourne Exp $

;; Makefile mode
(eval-after-load
 "make-mode"
 '(setq makefile-font-lock-keywords
	(list
	 ;; Comments
	 '("^\#"                  . font-lock-comment-face)
	 ;; Fontify numbers
	 '("\\<\\([0-9][0-9A-Fa-fXx]*\\)\\>" . font-lock-number-face)
	 ;; Variable assignments
	 '("^\\([A-Za-z0-9_ \t]*\\)\\(=\\|:=\\|\\+=\\)" 1 font-lock-keyword-face)
	 ;; Variable expansions
	 '("\\(\\$([^)]*)\\)" . font-lock-keyword-face)
	 ;; Targets
	 '("^\\([^ \t][^:]*\\):" 1 font-lock-function-name-face)
	 ;; Special variables $@ $% $< $? $^ $+ $*
	 '("\\(\\$[@%<?^+*]\\)" . font-lock-keyword-face)
	 ;; Keywords: define else endef endif export
	 ;;           ifdef ifeq ifndef ifneq include override unexport
	 ;;           vpath
	 (cons (concat "^[ -]*\\<\\(define\\|e\\(lse\\|nd\\(ef\\|if\\)\\|xport\\)\\|"
		       "if\\(def\\|eq\\|n\\(def\\|eq\\)\\)\\|include\\|override\\|"
		       "unexport\\|vpath\\)\\>") 'font-lock-keyword-face)
	 ;; Fontify variable names
	 '("\\<\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\>" . font-lock-variable-name-face)
	 )))
