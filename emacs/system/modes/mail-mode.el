;; Emacs configuration file
;; Setup for mail-mode/message-mode
;; Written by Martin Ebourne
;; $Id: mail-mode.el 792 2003-09-22 11:47:18Z martin $

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
