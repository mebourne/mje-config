;; Emacs function file
;; Copy word point is in to kill ring
;; Written by Martin Ebourne
;; $Id: copy-word-as-kill.el,v 1.1 2001/05/11 17:31:38 mebourne Exp $

;;;###autoload
(defun copy-word-as-kill ()
  "Copy word point is currently in as for `copy-region-as-kill'"
  (interactive)
  (kill-new (thing-at-point 'word))
  )
