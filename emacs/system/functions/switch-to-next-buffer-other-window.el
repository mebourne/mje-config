;; Emacs function file
;; Switch to the first buffer for other window
;; Written by Martin Ebourne
;; $Id: switch-to-next-buffer-other-window.el,v 1.1 2001/05/11 17:31:38 mebourne Exp $

;;;###autoload
(defun switch-to-next-buffer-other-window ()
  "Switch to the first buffer for other window"
  (interactive)
  (switch-to-buffer-other-window (nth 0 (iswitchb-make-buflist nil))))
