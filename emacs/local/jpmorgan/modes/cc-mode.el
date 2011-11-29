;; Emacs configuration file
;; Setup for cc-mode
;; Written by Martin Ebourne

;; Add our styles
(c-add-style "jpmorgan"
	     '("martin"
	       (c-basic-offset . 4)
	       (c-offsets-alist . ((access-label      . -)
				   ))
	       (tab-width . 4)
	       ))

;; Set up our C style
(custom-set-variables
 '(c-default-style '((java-mode . "martin-java")
		     (php-mode  . "pear")
		     (other     . "jpmorgan")
		     ))
 )

;; Strip trailing spaces on save
(add-hook 'c-mode-common-hook
	  (function (lambda ()
		      (add-hook 'write-contents-hooks 'eliminate-trailing-spaces)
		      )))

;; Extra file types for C mode
(setq auto-mode-alist
      (append '(("\\.tpp\\'"   . c++-mode)
		("\\.pdl\\'"   . c++-mode)
		("\\.cfg\\'"   . c++-mode)
		) auto-mode-alist))
