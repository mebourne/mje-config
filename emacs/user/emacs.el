;; Emacs configuration file
;; .emacs.el, main user startup file
;; Written by Martin Ebourne
;; $Id: emacs.el,v 1.1 2001/05/11 17:31:38 mebourne Exp $

(load "~/config/emacs/startup")
(load custom-file)
(if (and (not secondary)
	 window-system)
    (desktop-read))

;; Enable useful commands which are disabled by default
(put 'eval-expression 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Keep cursor on middle line
(setq constrain-percentage 50)

;; Set up mail addresses
(setq mail-user-agent 'message-user-agent)
(setq mail-host-address "arcordia.com")
(setq user-mail-address "ebourne_martin_j@arcordia.com")
(setq message-signature
      "Martin Ebourne
ebourne_martin_j@arcordia.com
")
