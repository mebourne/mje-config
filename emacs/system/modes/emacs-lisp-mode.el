;; Emacs configuration file
;; Setup for emacs-lisp-mode
;; Written by Martin Ebourne
;; $Id: emacs-lisp-mode.el 792 2003-09-22 11:47:18Z martin $

;; For sawfish lisp (librep)
(setq auto-mode-alist
      (append '(("\\.jl\\'"   . emacs-lisp-mode)
		) auto-mode-alist))
