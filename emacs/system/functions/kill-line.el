;; Emacs function file
;; Kill current line
;; Written by Martin Ebourne
;; $Id: kill-line.el,v 1.1 2001/05/11 17:31:38 mebourne Exp $

;;;###autoload
(defun kill-current-line ()
  "Kills line point is on"
  (interactive)
  (beginning-of-line)
  (kill-line)
  )
