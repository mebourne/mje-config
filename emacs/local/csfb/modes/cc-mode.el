;; Emacs configuration file
;; Setup for cc-mode
;; Written by Martin Ebourne
;; $Id: cc-mode.el,v 1.2 2002/03/21 12:22:57 mebourne Exp $

;; Add our styles
(c-add-style "csfb"
	     '((c-basic-offset . 3)
	       (c-offsets-alist . ((statement-cont    . c-lineup-math)
				   (substatement-open . 0)
				   (case-label        . +)
				   (access-label      . -)
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
	       (indent-tabs-mode . nil)
	       ))

;; Set up our C style
(custom-set-variables
 '(c-default-style '((java-mode . "martin-java")
		     (other     . "csfb")
		     ))
 )
