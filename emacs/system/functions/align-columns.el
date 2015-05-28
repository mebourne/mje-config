;; Emacs function file
;; Align table columns
;; Written by Martin Ebourne

;;;###autoload
(defun align-columns (begin end sep)
  "Align table colums with given separator"
  (interactive "r\nsSeparator: ")
  (align-regexp begin end (if (or (equal sep "")
                                  (equal sep " "))
                              "\\([ \t]+\\)"
                            (concat sep "\\([ \t]*\\)"))
                1 1 t))
