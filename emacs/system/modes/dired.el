;; Emacs configuration file
;; Setup for dired
;; Written by Martin Ebourne
;; $Id: dired.el 792 2003-09-22 11:47:18Z martin $

(defun dired-insert-subdir-or-find-file ()
  "Insert a subdir or find file depending on whether we
have a subdir or a file."
  (interactive)
  (let ((subdir (dired-get-filename)))
    (if (file-directory-p subdir)
	(dired-maybe-insert-subdir subdir nil)
      (dired-find-file)))
  )
(load "dired-x")
;(add-hook 'dired-load-hook
;	  (function (lambda ()
;		      (load "dired-x")
;		      (setq dired-omit-extensions (append '("flc") dired-omit-extensions))
;		      )))
(add-hook 'dired-mode-hook
	  (function (lambda ()
		      (local-set-key "\C-m" 'dired-insert-subdir-or-find-file)
		      )))
(setq dired-move-to-filename-regexp
	" \\(\
[A-Za-z\xa0-\xff][A-Za-z\xa0-\xff][A-Za-z\xa0-\xff] [0-3 ][0-9]\\|\
[0-3 ][0-9] [A-Za-z\xa0-\xff][A-Za-z\xa0-\xff][A-Za-z\xa0-\xff]\\)\
 [ 0-9][0-9][:0-9][0-9][ 0-9] ")
