;;; find-file-root.el --- Find-File-Root an Emacs config directory

;; Taken from sources on www.emacswiki.org, originally written by Bjorn
;; Lindstrom, since modified
;;
;; $Id:$

;;; Commentary:

;; Provides a mechanism to easily edit files as root, using tramp. Behaves as
;; for find-file and is bound to C-x C-r.

;;; Code:

(require 'tramp)

;; Internal variables

(defvar find-file-root-prefix (if (featurep 'xemacs)
				  "/[su/root@localhost]"
				"/su:root@localhost:" )
  "*The filename prefix used to open a file with `find-file-root'.")

(defvar find-file-root-history nil
  "History list for files found using `find-file-root'.")

(defvar find-file-root-hook nil
  "Normal hook for functions to run after finding a \"root\" file.")


;; Internal functions

;;;###autoload
(defun find-file-root ()
  "*Open a file as the root user.
   Prepends `find-file-root-prefix' to the selected file name so that it
   maybe accessed via the corresponding tramp method."

  (interactive)
  (require 'tramp)
  (let* ( ;; We bind the variable `file-name-history' locally so we can
   	 ;; use a separate history list for "root" files.
   	 (file-name-history find-file-root-history)
   	 (name (or buffer-file-name default-directory))
   	 (tramp (and (tramp-tramp-file-p name)
   		     (tramp-dissect-file-name name)))
   	 path dir file)

    ;; If called from a "root" file, we need to fix up the path.
    (when tramp
      (setq path (tramp-file-name-localname tramp)
   	    dir (file-name-directory path)))

    (when (setq file (read-file-name "Find file (UID = 0): " dir path))
      (find-file (concat find-file-root-prefix file))
      ;; If this all succeeded save our new history list.
      (setq find-file-root-history file-name-history)
      ;; allow some user customization
      (run-hooks 'find-file-root-hook))))


(defface find-file-root-header-face
  '((t (:foreground "white" :background "red3")))
  "*Face use to display header-lines for files opened as root.")

(defun find-file-root-header-warning ()
  "*Display a warning in header line of the current buffer.
   This function is suitable to add to `find-file-root-hook'."
  (let* ((warning "WARNING: EDITING FILE AS ROOT!")
	 (space (+ 6 (- (frame-width) (length warning))))
	 (bracket (make-string (/ space 2) ?-))
	 (warning (concat bracket warning bracket)))
    (setq header-line-format
	  (propertize  warning 'face 'find-file-root-header-face))))

(add-hook 'find-file-root-hook 'find-file-root-header-warning)


(defun find-file-root-safe-backups ()
  "Force appropriate backup strategy when editing files as root."
  (setq buffer-auto-save-file-name nil)
  (set (make-local-variable 'backup-by-copying) t)
  (set (make-local-variable 'backup-directory-alist) nil))

(add-hook 'find-file-root-hook 'find-file-root-safe-backups)

(provide 'find-file-root)
