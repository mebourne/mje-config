;; Emacs configuration file
;; Setup for emacs-lisp-mode
;; Written by Martin Ebourne

;; For sawfish lisp (librep)
(setq auto-mode-alist
      (append '(("\\.jl\\'"   . emacs-lisp-mode)
		) auto-mode-alist))
