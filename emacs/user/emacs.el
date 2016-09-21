;; Emacs configuration file
;; .emacs.el, main user startup file
;; Written by Martin Ebourne

(setq load-prefer-newer t)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(if (file-readable-p "/etc/config/emacs/startup.el")
    (load "/etc/config/emacs/startup")
  (load "~/config/emacs/startup"))
(load custom-file)

;; Enable useful commands which are disabled by default
(put 'downcase-region 'disabled nil)
(put 'eval-expression 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Keep cursor on middle line
(setq constrain-percentage 50)
