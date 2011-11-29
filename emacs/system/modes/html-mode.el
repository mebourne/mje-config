;; Emacs configuration file
;; Setup for html-mode
;; Written by Martin Ebourne

;; HTML mode
(add-hook 'html-mode-hook
	  (function (lambda ()
		      (setq fill-column 77)
		      (auto-fill-mode t)
		      )))
