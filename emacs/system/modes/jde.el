;; Emacs configuration file
;; Setup for jde
;; Written by Martin Ebourne
;; $Id: jde.el,v 1.1 2001/05/11 17:31:38 mebourne Exp $

(setq auto-mode-alist
      (append
       '(("\\.java\\'" . jde-mode))
       auto-mode-alist))
