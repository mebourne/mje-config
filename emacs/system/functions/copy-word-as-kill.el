;; Emacs function file
;; Copy word point is in to kill ring
;; Written by Martin Ebourne

;;;###autoload
(defun copy-word-as-kill ()
  "Copy word point is currently in as for `copy-region-as-kill'"
  (interactive)
  (kill-new (thing-at-point 'word))
  )
