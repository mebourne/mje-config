;; Emacs function file
;; Extra file-cache functions
;; Written by Martin Ebourne
;; $Id: file-cache-x.el,v 1.1 2001/05/11 17:31:38 mebourne Exp $

;; Command to complete from file cache and open file
(defun file-cache-minibuffer-complete-and-exit (arg)
  "Attempts to complete the minibuffer contents using the file cache
and then proceeds as for minibuffer-complete-and-exit"
  (interactive "P")
  (file-cache-minibuffer-complete arg)
  (minibuffer-complete-and-exit)
  )

;; Command to complete on file name (without path) from file cache
(defun file-cache-minibuffer-complete-on-filename (arg)
  "Strips the path from the filename in the minibuffer and attempts to
complete on it using the file cache"
  (interactive "P")
  (end-of-line)
  (let ((last-backslash (search-backward "\/" nil t nil)))
    (if last-backslash
	(progn
	  (beginning-of-line)
	  (kill-region (point) last-backslash))
      )
    (file-cache-minibuffer-complete arg)
    )
  )

;; Command to complete on file name (without path) from file cache and open file
(defun file-cache-minibuffer-complete-on-filename-and-exit (arg)
  "Strips the path from the filename in the minibuffer and attempts to
complete on it using the file cache and then proceeds as for
minibuffer-complete-and-exit"
  (interactive "P")
  (file-cache-minibuffer-complete-on-filename arg)
  (minibuffer-complete-and-exit)
  )

(defun file-cache-add-classlist ()
  "Add all files in class list to file cache."
  (interactive)
  (file-cache-clear-cache)
  (set-buffer (get-buffer-create file-cache-buffer))
  (erase-buffer)
  (call-process "sh"
		nil 
		(get-buffer file-cache-buffer)
		nil
		"-c"
		"awk '{print $3}' /lighthouse/rcs/classlist | cat - /lighthouse/rcs/classxx | sed 's:^:~/LH/:'")
  (file-cache-add-from-file-cache-buffer)
  )
