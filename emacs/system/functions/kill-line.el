;; Emacs function file
;; Kill current line
;; Written by Martin Ebourne

;;;###autoload
(defun kill-current-line ()
  "Kills line point is on"
  (interactive)
  (beginning-of-line)
  (kill-line)
  )
