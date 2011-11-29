;; Emacs configuration file
;; Xwindows specific setup
;; Written by Martin Ebourne

(cond (window-system

       ;; Sort out kill-emacs to confirm first and C-x C-z not to iconify if in X
       (add-hook 'kill-emacs-query-functions
		 (function (lambda ()
			     (y-or-n-p "Are you MAD? "))))
       (global-unset-key "\C-x\C-z")
       ))
