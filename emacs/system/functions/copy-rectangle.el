;; Emacs function file
;; Copy rectangle to kill ring
;; Written by Martin Ebourne
;; $Id: copy-rectangle.el 792 2003-09-22 11:47:18Z martin $

;;;###autoload
(defun copy-rectangle (start end)
  "Save rectangle with corners at point and mark as last killed one.
Does not delete rectangle from buffer.
Calling from program, supply two args START and END, buffer positions.
But in programs you might prefer to use `delete-extract-rectangle'."
  (interactive "r")
  (setq killed-rectangle (extract-rectangle start end)))
