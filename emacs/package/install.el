;;; install.el --- Install an Emacs config directory

;; Copyright (C) 2001 Martin Ebourne. All rights reserved
;;
;; $Id$

;;; Commentary:

;; Provides a mechanism to easily handle Emacs configurations which are
;; split into many files across a directory hierarchy
;; ...

;;; Code:

(require 'bytecomp)

;; User configurable variables. May be set in the layout file

(defvar install-startup-name "startup.el"
  "*Leaf name of main generated startup file")

(defvar install-loadfile-name "loaddefs.el"
  "*Leaf name of generated definitions files")


;; Internal variables

(defvar install-layout-file nil
  "Name of layout file for this config installation")

(defvar install-base-dir nil
  "Base directory of config installation")

(defvar install-init-file-list nil
  "List of init files")

(defvar install-directory-list nil
  "List of directories")


;; User interface functions

;;;###autoload
(defun install (layoutfile)
  "Create an installation for the given config layout file"
  (interactive "fInstall config (layout file): ")

  ;; Remind user of any buffers that need saving
  (if noninteractive
      nil
    (save-some-buffers)
    (force-mode-line-update))

  ;; Expand layout filename
  (setq install-layout-file (expand-file-name layoutfile))

  ;; Get default base directory from layout name, and remove any trailing /.
  ;; Note that the layout file may override this
  (setq install-base-dir (directory-file-name (file-name-directory install-layout-file)))

  (message "Installing emacs configuration...")
  (message "Executing layout file %s..." install-layout-file)

  (load install-layout-file)

  (setq install-init-file-list nil)
  (setq install-directory-list (list install-base-dir))

  (message "Executing layout file %s... done" install-layout-file)
  (message "Base directory is %s" install-base-dir)

  (install-directories)
  (install-write-startup)

  (message "Byte compiling files...")
  (install-recompile-directory install-directory-list)
  (message "Byte compiling files... done")

  (message "Installing emacs configuration... done")
  )

;;;###autoload
(defun reinstall ()
  "Reinstall the last config - either the last one executed,
or the last one installed"
  (interactive)

  (install install-layout-file)
  )

(defun install-load-dir (dirname)
  "Add a directory containing files which need to be loaded
as part of the configuration. DIRNAME is relative to
install-base-dir.

This function is for use by the layout file, as part of the
install-directories function."

  (setq dirname (concat install-base-dir "/" dirname))
  (if (file-directory-p dirname)
      (progn
	(message "Processing load directory %s..." dirname)

	;; Remember directory for compiling
	(setq install-directory-list (append install-directory-list
					     (list dirname)))

	(save-excursion
	  (install-create-buffer install-loadfile-name)

	  ;; Get all .el files in directory. Insert load statement for each one
	  ;; that isn't the file we are generating
	  (let ((files (directory-files dirname t "\\.el$"))
		name)
	    (while files
	      (setq name (car files))
	      (if (not (string-match (concat "/" install-loadfile-name "$") name))
		  (progn
		    (insert "(load \"" (file-name-sans-extension name) "\")\n")
		    ))
	      (setq files (cdr files))
	      ))
	  (newline)

	  (install-write-buffer dirname)
	  )

	(message "Processing load directory %s... done" dirname)
	)
    (message "Ignoring missing directory %s" dirname)
    ))

(defun install-autoload-dir (dirname)
  "Add a directory containing files which need to be autoloaded
as part of the configuration. DIRNAME is relative to
install-base-dir.

This function is for use by the layout file, as part of the
install-directories function."

  (setq dirname (concat install-base-dir "/" dirname))
  (if (file-directory-p dirname)
      (progn
	(message "Processing autoload directory %s..." dirname)

	;; Remember directory for compiling
	(setq install-directory-list (append install-directory-list
					     (list dirname)))

	;; To start with just create the basics of the file
	(save-excursion
	  (install-create-buffer install-loadfile-name)

	  ;; Insert statement to prepend autoload directory to load-path
	  (insert "(setq load-path (append (list \"" dirname "\")\n")
	  (insert "                        load-path))\n\n")

	  (install-write-buffer dirname)
	  )

	;; Now add the autoloads to our created file
	(let ((filename (concat dirname "/" install-loadfile-name))
	      (backups backup-inhibited)
	      )
	  (setq generated-autoload-file filename)

	  ;; Make sure we stop backups of the generated file being produced
	  (unwind-protect
	      (progn
		(setq backup-inhibited t)
		(funcall (if (fboundp 'update-directory-autoloads)
			     'update-directory-autoloads
			   'update-autoloads-from-directories)
			 dirname)
		)
	    (setq backup-inhibited backups)
	    )

	  ;; update-autoloads leaves the buffer lying around, so kill it
	  (kill-buffer (find-file-noselect filename))
	  )

	(message "Processing autoload directory %s... done" dirname)
	)
    (message "Ignoring missing directory %s" dirname)
    ))


;; Internal functions

(defun install-init-file (filename)
  "Add initialisation file to load list"

  (setq install-init-file-list (append install-init-file-list
				       (list filename)))
  )

(defun install-insert-header ()
  "Insert standard file header into current buffer"

  (insert "\
;; Emacs configuration file
;; Generated by Emacs 'install' package written by Martin Ebourne
;;
;; ***WARNING*** This is an auto-generated file - DO NOT EDIT
\n")
  )

(defun install-create-buffer (buffername)
  "Create temporary buffer, make current, and insert header"

  ;; Prepend a space to buffer name to disable undo
  (set-buffer (get-buffer-create (concat " " buffername)))
  (erase-buffer)
  (set-buffer-multibyte t)

  (install-insert-header)
  )

(defun install-write-buffer (dirname)
  "Write current buffer to given directory if changed, and add to load list"

  (insert ";; END\n")

  (let* (;; Buffer name starts with a space
	 (name (substring (buffer-name) 1))
	 (filename (concat dirname "/" name))
	 ;; Load original file if found (else we get an empty buffer)
	 (other-buffer (find-file-noselect filename))
	 other-buffer-size
	 )

    ;; Get size of original file buffer
    (save-excursion
      (set-buffer other-buffer)
      (setq other-buffer-size (buffer-size)))

    ;; Compare the new and original buffers, and save new contents if
    ;; changed. Note: Only compare up to end of new buffer so we ignore any
    ;; appended text (such as added by autoload). The 'END' comment we've
    ;; already added will ensure that we don't miss anything we generated in
    ;; the comparison
    (if (/= 0 (compare-buffer-substrings (current-buffer) 1 (1+ (buffer-size))
					 other-buffer 1 (1+ (min other-buffer-size
								 (buffer-size)))))
	(write-file filename)
      (message "No changes to file %s" filename)
      )

    ;; Tidy up
    (kill-buffer nil)
    (kill-buffer other-buffer)

    ;; Add to load list
    (install-init-file filename)
    )
  )

(defun install-write-startup ()
  "Write the main startup file"

  (message "Creating main startup file...")

  (save-excursion
    (install-create-buffer install-startup-name)

    ;; Insert statement to set layout file in case we want to reinstall
    (insert "(setq install-layout-file \"" install-layout-file "\")\n")

    ;; Insert statement to set base dir for config to use
    (insert "(setq install-base-dir \"" install-base-dir "\")\n")

    ;; Insert statement to set user base dir for config to use
    (insert "(setq install-user-base-dir \"" install-base-dir "/user\")\n")
    (newline)

    ;; Insert load statement for each file on init list
    (let ((files  install-init-file-list))
      (while files
	(insert "(load \"" (file-name-sans-extension (car files)) "\")\n")
	(setq files (cdr files))
	))
    (newline)

    (install-write-buffer install-base-dir)
    )

  (message "Creating main startup file... done")
  )

(defun install-recompile-directory (directories)
  "Recompile every `.el' file in DIRECTORIES that needs recompilation.
This is if a `.elc' file does not exist or is is older than the `.el' file."
  (let ((file-count 0)
	(dir-count 0)
	last-dir)
     (while directories
       (setq directory (car directories))
       (message "Checking %s..." directory)
       (let ((files (directory-files directory))
	     source dest)
	 (while files
	   (setq source (expand-file-name (car files) directory))

	   ;; Decide whether to compile file
	   (if (and (string-match emacs-lisp-file-regexp source)
		    (not (auto-save-file-name-p source))
		    (setq dest (byte-compile-dest-file source))
		    (file-newer-than-file-p source dest))
	       (progn (message "Compiling %s..." source)
		      (byte-compile-file source)
		      (setq file-count (1+ file-count))
		      (if (not (eq last-dir directory))
			  (setq last-dir directory
				dir-count (1+ dir-count)))
		      ))
	   (setq files (cdr files))))
       (setq directories (cdr directories)))
    (message "Done (Total of %d file%s compiled%s)"
	     file-count (if (= file-count 1) "" "s")
	     (if (> dir-count 1) (format " in %d directories" dir-count) ""))))


(provide 'install)
