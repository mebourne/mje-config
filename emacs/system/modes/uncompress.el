;; Emacs configuration file
;; Setup for uncompress
;; Written by Martin Ebourne
;; $Id: uncompress.el,v 1.1 2001/05/11 17:31:38 mebourne Exp $

(setq auto-mode-alist
      (append '(("\\.Z$"  . uncompress-while-visiting)
		("\\.gz$" . uncompress-while-visiting)
		) auto-mode-alist))
