;; Emacs configuration file
;; Setup for cc-mode
;; Written by Martin Ebourne
;; $Id: cc-mode.el 792 2003-09-22 11:47:18Z martin $

;; Add our styles
(c-add-style "gs-java"
	     '("martin-java"
	       (c-basic-offset . 4)
	       ))

;; Set up our C style
(custom-set-variables
 '(c-default-style '((java-mode . "gs-java")
		     (other     . "martin")
		     ))
 )

;; Strip trailing spaces on save
(add-hook 'c-mode-common-hook
	  (function (lambda ()
		      (add-hook 'write-contents-hooks 'eliminate-trailing-spaces)
		      )))
