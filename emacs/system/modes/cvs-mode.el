;; Emacs configuration file
;; Setup for cvs-mode/pcl-cvs-mode
;; Written by Martin Ebourne
;; $Id: cvs-mode.el 792 2003-09-22 11:47:18Z martin $

;(load "pcl-cvs-startup")
;(setq cvs-cvsroot (getenv "CVSROOT"))
;(setq cvs-auto-remove-handled t)
(add-hook 'cvs-mode-hook
	   (function (lambda ()
		       (local-set-key "\C-m" 'cvs-mode-find-file)
		       )))
