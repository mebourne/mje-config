;; Emacs configuration file
;; Setup for cvs-mode/pcl-cvs-mode
;; Written by Martin Ebourne
;; $Id: cvs-mode.el,v 1.1 2001/05/11 17:31:38 mebourne Exp $

;(load "pcl-cvs-startup")
;(setq cvs-cvsroot (getenv "CVSROOT"))
;(setq cvs-auto-remove-handled t)
(add-hook 'cvs-mode-hook
	   (function (lambda ()
		       (local-set-key "\C-m" 'cvs-mode-find-file)
		       )))
