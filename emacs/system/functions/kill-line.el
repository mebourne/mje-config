;; Emacs function file
;; Kill current line
;; Written by Martin Ebourne
;; $Id: kill-line.el 792 2003-09-22 11:47:18Z martin $

;;;###autoload
(defun kill-current-line ()
  "Kills line point is on"
  (interactive)
  (beginning-of-line)
  (kill-line)
  )
