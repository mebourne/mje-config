;; Emacs configuration file
;; Custom definitions
;; Written by Martin Ebourne
;; $Id: custom.el,v 1.3 2002/03/26 17:23:57 mebourne Exp $

(custom-set-variables

 ;; No message in scratch buffer
 '(initial-scratch-message nil)

 ;; No beeps
 '(visible-bell t)

 ;; Speedbar size/position
 '(speedbar-frame-parameters (quote ((minibuffer . nil)
				     (unsplittable . t)
				     (menu-bar-lines . 0)
				     (border-width . 0)
				     (top . 20)
				     (left . 762)
				     (width . 55)
				     (user-position . t))))
 )
