;; Emacs function file
;; Toggle parenthesis paris
;; Written by Martin Ebourne

;;;###autoload
(defun toggle-paren (arg)
  "toggle inner ( and its matching ) with { and }"
  (interactive "p")
  (save-excursion
    (if (not (looking-at "[({]"))
	(search-backward-regexp "[({})]"))
    (cond ((looking-at "(") (delete-forward-char 1) (insert "{") (backward-char 1) (forward-list 1) (backward-char 1) (delete-forward-char 1) (insert "}"))
	  ((looking-at "{") (delete-forward-char 1) (insert "(") (backward-char 1) (forward-list 1) (backward-char 1) (delete-forward-char 1) (insert ")"))))
)
