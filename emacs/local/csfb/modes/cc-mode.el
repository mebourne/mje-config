;; Emacs configuration file
;; Setup for cc-mode
;; Written by Martin Ebourne

;; Add our styles
(c-add-style "csfb"
	     '("martin"
	       (c-basic-offset . 3)
	       (c-offsets-alist . ((access-label      . -)
				   ))
	       (indent-tabs-mode . nil)
	       ))

;; Set up our C style
(custom-set-variables
 '(c-default-style '((java-mode . "martin-java")
		     (other     . "csfb")
		     ))
 )

;; Strip trailing spaces on save
(add-hook 'c-mode-common-hook
	  (function (lambda ()
		      (add-hook 'write-contents-hooks 'eliminate-trailing-spaces)
		      )))

;; .h is C++
(setq auto-mode-alist
      (append '(("\\.h\\'" . c++-mode)
		) auto-mode-alist))
