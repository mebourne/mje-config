;; Emacs configuration file
;; Setup for this config install
;; Written by Martin Ebourne
;; $Id: layout.el 792 2003-09-22 11:47:18Z martin $

(setq install-startup-name "startup.el")
(setq install-loadfile-name "loaddefs.el")

(defun install-directories ()
  (install-autoload-dir "3rdparty")
  (install-autoload-dir "package")
  (install-autoload-dir "system/functions")
  (install-autoload-dir "local/current/functions")

  (install-load-dir "system/loadfuncs")
  (install-load-dir "system/config")
  (install-load-dir "system/modes")
  (install-load-dir "system/syntax")

  (install-load-dir "local/current/loadfuncs")
  (install-load-dir "local/current/config")
  (install-load-dir "local/current/modes")
  (install-load-dir "local/current/syntax")
  )
