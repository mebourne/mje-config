;; Emacs function file
;; Swap case of a character
;; Written by Martin Ebourne
;; $Id: swap-case.el,v 1.1 2001/05/11 17:31:38 mebourne Exp $

;;;###autoload
(defun swap-case (&optional arg)
  "Swap case of character after point and advance point"
  (interactive "p")
  (while (> arg 0)
    (let* ((current (char-after))
	   (lower (downcase current))
	   (upper (upcase current)))
      (if (eq current lower)
	  (setq current upper)
	(setq current lower))
      (insert current)
      (delete-char 1)
      )
    (setq arg (1- arg))
    )
  )
