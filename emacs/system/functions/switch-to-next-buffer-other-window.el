;; Emacs function file
;; Switch to the first buffer for other window
;; Written by Martin Ebourne
;; $Id: switch-to-next-buffer-other-window.el 792 2003-09-22 11:47:18Z martin $

;;;###autoload
(defun switch-to-next-buffer-other-window ()
  "Switch to the first buffer for other window"
  (interactive)
  (switch-to-buffer-other-window (nth 0 (iswitchb-make-buflist nil))))
