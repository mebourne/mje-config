;; Emacs configuration file
;; Setup for html-mode
;; Written by Martin Ebourne
;; $Id: html-mode.el,v 1.1 2001/05/11 17:31:38 mebourne Exp $

;; HTML mode
(add-hook 'html-mode-hook
	  (function (lambda ()
		      (setq fill-column 77)
		      (auto-fill-mode t)
		      )))
