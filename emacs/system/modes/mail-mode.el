;; Emacs configuration file
;; Setup for mail-mode/message-mode
;; Written by Martin Ebourne
;; $Id: mail-mode.el,v 1.1 2001/05/11 17:31:38 mebourne Exp $

;; Mail mode
(setq auto-mode-alist
      (append '(("^/tmp/snd.[0-9]+$" . mail-mode)
		("^/tmp/pico.[0-9]+$" . mail-mode)
		) auto-mode-alist))
(add-hook 'mail-mode-hook
	  (function (lambda ()
		      (local-set-key "\C-c\C-c" nil)
		      (setq fill-column 70)
		      (auto-fill-mode t)
		      )))
(add-hook 'message-mode-hook
	  (function (lambda ()
		      (setq fill-column 70)
		      (auto-fill-mode t)
		      )))
