;; Emacs configuration file
;; Emacs server setup
;; Written by Martin Ebourne
;; $Id: server.el 792 2003-09-22 11:47:18Z martin $

;; Check to see if we've already got an emacs running. If we haven't then start the server
(defvar secondary nil
  "Set if this is not the primary (ie. started first) emacs.")
(if (not (eq system-type 'windows-nt))
    (setq secondary (eq (call-process "/bin/sh" nil nil nil "-c" "ps xc | grep emacsserver") 0)))
(if (not secondary)
    (progn
      (if (eq system-type 'windows-nt)
	  (gnuserv-start)
	(server-start))
      (message "Primary emacs. Server started"))
  (message "Secondary emacs"))
