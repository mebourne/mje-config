;; Emacs configuration file
;; Setup for html-mode
;; Written by Martin Ebourne
;; $Id: html-mode.el 792 2003-09-22 11:47:18Z martin $

;; HTML mode
(add-hook 'html-mode-hook
	  (function (lambda ()
		      (setq fill-column 77)
		      (auto-fill-mode t)
		      )))
