;; Emacs configuration file
;; Setup for cc-mode
;; Written by Martin Ebourne
;; $Id: cc-mode.el 792 2003-09-22 11:47:18Z martin $

;; Add our styles
(c-add-style "iqfs"
	     '("martin"
	       (c-basic-offset . 4)
	       (c-offsets-alist . ((statement-cont    . c-lineup-math)
				   (substatement-open . 0)
				   (case-label        . +)
				   (access-label      . -)
				   ))
	       (tab-width . 4)
	       ))

;; Set up our C style
(custom-set-variables
 '(c-default-style '((java-mode . "java")
		     (other     . "iqfs")
		     ))
 )
