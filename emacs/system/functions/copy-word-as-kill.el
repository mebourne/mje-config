;; Emacs function file
;; Copy word point is in to kill ring
;; Written by Martin Ebourne
;; $Id: copy-word-as-kill.el 792 2003-09-22 11:47:18Z martin $

;;;###autoload
(defun copy-word-as-kill ()
  "Copy word point is currently in as for `copy-region-as-kill'"
  (interactive)
  (kill-new (thing-at-point 'word))
  )
