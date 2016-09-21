;; Emacs configuration file
;; Setup for perl-mode
;; Written by Martin Ebourne

;; cperl-mode is preferred to perl-mode
(defalias 'perl-mode 'cperl-mode)

(add-hook 'cperl-mode-hook
	  (function (lambda ()
                      (setq cperl-invalid-face nil)
                      )))
