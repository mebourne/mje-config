;; Emacs configuration file
;; Setup for cc-mode
;; Written by Martin Ebourne
;; $Id: cc-mode.el,v 1.7 2002/03/21 13:41:01 mebourne Exp $

;; C and C++ style
(eval-after-load "cc-mode"
  '(progn
     ;; Map _ as a word character to make faster font lock regexps work correctly. Not needed for
     ;; slow font lock regexps
     (modify-syntax-entry ?_ "w" c-mode-syntax-table)
     (modify-syntax-entry ?_ "w" c++-mode-syntax-table)
     (modify-syntax-entry ?_ "w" java-mode-syntax-table)

     ;; Improvements to java decoding for use with speedbar
     (setq cc-imenu-java-generic-expression-DISABLE-TEMPORARILY
	   (`
	    ((nil
	      (,
	       (concat
		"^\\([ \t]\\)*"
		"\\([A-Za-z0-9_-]+[ \t]+\\)?" ; type specs; there can be
		"\\([A-Za-z0-9_-]+[ \t]+\\)?" ; more than 3 tokens, right?
		"\\([A-Za-z0-9_-]+[ \t]*[[]?[]]?\\)"
		"\\([ \t]\\)"
		"\\([A-Za-z0-9_-]+"	; the string we want to get
		"[ \t]*("
		"[][a-zA-Z,_1-9\n \t]*"	; arguments
		")\\)[ \t]*"
					;       "[^;(]"
		"\\(;\\|[,a-zA-Z_1-9\n \t]*{\\)"
		)) 6))))
     ))


;; C & C++
(setq auto-mode-alist
      (append '(("\\.sq[cC]\\'" . c++-mode)
		("\\.inl\\'"   . c++-mode)
		("\\.ipp\\'"   . c++-mode)
		) auto-mode-alist))

(add-hook 'c-mode-common-hook
	  (function (lambda ()
		      (set (make-local-variable 'dabbrev-case-fold-search) nil)
		      (set (make-local-variable 'dabbrev-case-replace) nil)
		      (hs-setup)
		      (c-toggle-auto-state t)
		      (c-toggle-hungry-state t)
		      )))

;; Add our styles
(c-add-style "martin"
	     '((c-basic-offset . 2)
	       (c-offsets-alist . ((statement-cont    . c-lineup-math)
				   (substatement-open . 0)
				   (case-label        . +)
				   (access-label      . -1)
				   ))
	       (c-hanging-braces-alist . ((defun-open before)
					  (defun-close before)
					  (class-open before)
					  (class-close before)
					  (inline-open)
					  (inline-close)
					  (block-open before)
					  (block-close before)
					  (substatement-open before)
					  (statement-case-open before)
					  (extern-lang-open before)
					  (extern-lang-close before)
					  (brace-list-open)
					  (brace-list-close before)
					  (brace-list-intro)
					  (brace-list-entry)
					  ))
	       (c-hanging-semi&comma-criteria . nil)
	       ;;(c-cleanup-list . (empty-defun-braces))
	       (c-electric-pound-behavior . 'alignleft)
	       (comment-column . 45)
	       (fill-column . 78)
	       ))
(c-add-style "martin-java"
	     '("java"
	       (c-basic-offset . 2)
	       (c-offsets-alist . ((statement-cont      . c-lineup-math)
				   (substatement-open   . 0)
				   (statement-case-open . 0)
				   (case-label          . +)
				   ))
	       (c-hanging-braces-alist . ((defun-open before)
					  (defun-close before)
					  (class-open before)
					  (class-close before)
					  (inline-open before)
					  (inline-close before)
					  (block-open before)
					  (block-close before)
					  (substatement-open before)
					  (statement-case-open before)
					  (extern-lang-open before)
					  (extern-lang-close before)
					  (brace-list-open)
					  (brace-list-close before)
					  (brace-list-intro)
					  (brace-list-entry)
					  ))
	       (c-hanging-semi&comma-criteria . nil)
	       ;;(c-cleanup-list . (empty-defun-braces))
	       (c-electric-pound-behavior . 'alignleft)
	       (comment-column . 45)
	       (fill-column . 78)
	       ))

;; Set up our C style
(custom-set-variables
 '(c-default-style '((java-mode . "martin-java")
		     (other     . "martin")
		     ))
 )
