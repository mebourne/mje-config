;; Emacs function file
;; Copy rectangle to kill ring
;; Written by Martin Ebourne
;; $Id: copy-rectangle.el,v 1.1 2001/05/11 17:31:38 mebourne Exp $

;;;###autoload
(defun copy-rectangle (start end)
  "Save rectangle with corners at point and mark as last killed one.
Does not delete rectangle from buffer.
Calling from program, supply two args START and END, buffer positions.
But in programs you might prefer to use `delete-extract-rectangle'."
  (interactive "r")
  (setq killed-rectangle (extract-rectangle start end)))
