;;; vc.el --- drive a version-control system from within Emacs

;; $Id: vc.el,v 1.1 2001/05/11 17:31:38 mebourne Exp $

;; Copyright (C) 1992, 93, 94, 95, 96 Free Software Foundation, Inc.

;; Author:     Eric S. Raymond <esr@snark.thyrsus.com>
;; Maintainer: Andre Spiegel <spiegel@inf.fu-berlin.de>
;; Maintainer: (ClearCase) Rod Whitby <rwhitby@geocities.com>
;; XEmacs conversion: Steve Baur <steve@altair.xemacs.org>

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This mode is fully documented in the Emacs user's manual.
;;
;; This was designed and implemented by Eric Raymond <esr@snark.thyrsus.com>.
;; Paul Eggert <eggert@twinsun.com>, Sebastian Kremer <sk@thp.uni-koeln.de>,
;; and Richard Stallman contributed valuable criticism, support, and testing.
;; CVS support was added by Per Cederqvist <ceder@lysator.liu.se>
;; in Jan-Feb 1994.  Further enhancements came from ttn.netcom.com and
;; Andre Spiegel <spiegel@inf.fu-berlin.de>.
;;
;; XEmacs fixes, CVS fixes, and general improvements
;; by Jonathan Stigelman <Stig@hackvan.com>
;;
;; Merged ClearCase features from ClearCase extensions version: 2.2.3 and
;; added highlighting and menubar access to vc-dired-mode by Chris Felaco
;; <felaco@ziplink.net>.
;;
;; Supported version-control systems presently include SCCS, RCS, and CVS.
;;
;; Some features will not work with old RCS versions.  Where
;; appropriate, VC finds out which version you have, and allows or
;; disallows those features (stealing locks, for example, works only 
;; from 5.6.2 onwards).
;; Even initial checkins will fail if your RCS version is so old that ci
;; doesn't understand -t-; this has been known to happen to people running
;; NExTSTEP 3.0.
;;
;; You can support the RCS -x option by adding pairs to the 
;; vc-master-templates list.
;;
;; Proper function of the SCCS diff commands requires the shellscript vcdiff
;; to be installed somewhere on Emacs's path for executables.
;;
;; If your site uses the ChangeLog convention supported by Emacs, the
;; function vc-comment-to-change-log should prove a useful checkin hook.
;;
;; This code depends on call-process passing back the subprocess exit
;; status.  Thus, you need Emacs 18.58 or later to run it.  For the
;; vc-directory command to work properly as documented, you need 19.
;; You also need Emacs 19's ring.el.
;;
;; The vc code maintains some internal state in order to reduce expensive
;; version-control operations to a minimum.  Some names are only computed
;; once.  If you perform version control operations with RCS/SCCS/CVS while
;; vc's back is turned, or move/rename master files while vc is running,
;; vc may get seriously confused.  Don't do these things!
;;
;; Developer's notes on some concurrency issues are included at the end of
;; the file.
;;
;;; Code:

(require 'vc-hooks)
(require 'vc-custom)
(require 'ring)
;; FSF Emacs - dired may not be loaded.
(require 'dired)

(require 'comint)
;; NT Emacs - doesn't use tq.
(if (not (memq window-system '(win32 w32)))
    (require 'tq)) ;; For ClearCase support

(defconst vc-cc-version "0.5")
(defconst vc-cc-maintainer-address "vc-clearcase@geocities.com")

(defun vc-cc-submit-bug-report ()
   "Submit via mail a bug report on VC-ClearCase"
   (interactive)
   (require 'reporter)
   (and (y-or-n-p "Do you really want to submit a report on VC-CC? ")
        (reporter-submit-bug-report
          vc-cc-maintainer-address
          (concat "vc.el " vc-cc-version)
          '(vc-diff-on-checkin vc-keep-workfiles vc-default-back-end
                               vc-master-templates))))


(if (not (assoc 'vc-parent-buffer minor-mode-alist))
    (setq minor-mode-alist
          (cons '(vc-parent-buffer vc-parent-buffer-name)
                minor-mode-alist)))

;; FSF Emacs - doesn't like parameters on mark-marker.
(defun vc-mark-marker ()
  (if vc-elucidated
      (mark-marker t)	;; XEmacs
    (mark-marker)))	;; Emacs

;; NT Emacs - needs a replacement for tq.
(defun vc-get-command-stdout (program stdin &rest args)
  "Call PROGRAM with STDIN as input.
Returns PROGRAM's stdout.
ARGS is the command line arguments to PROGRAM."
  (let ((buf (generate-new-buffer "cleartoolexecution")))
    (prog1
	(save-excursion
	  (set-buffer buf)
	  (insert stdin)  ;; stdin to the command
	  (apply 'call-process-region (point-min) (point-max) program
		 t buf args)
          (buffer-string))
      (kill-buffer buf))))

;; To implement support for a new version-control system, add another
;; branch to the vc-backend-dispatch macro and fill it in in each
;; call.  The variable vc-master-templates in vc-hooks.el will also
;; have to change.

(defmacro vc-backend-dispatch (f s r c cc)
  "Execute FORM1, FORM2, FORM3 or FORM4 for SCCS, RCS, CVS or ClearCase
respectively.
If FORM3 is `RCS', use FORM2 for CVS as well as RCS.
\(CVS shares some code with RCS)."
  (list 'let (list (list 'type (list 'vc-backend f)))
	(list 'cond
	      (list (list 'eq 'type (quote 'SCCS)) s)	;; SCCS
	      (list (list 'eq 'type (quote 'RCS)) r)	;; RCS
	      (list (list 'eq 'type (quote 'CVS))	;; CVS
		    (if (eq c 'RCS) r c))
	      (list (list 'eq 'type (quote '@@)) cc)	;; CC
	      )))

(put 'vc-backend-dispatch 'lisp-indent-function 'defun)
(put 'vc-backend-dispatch 'lisp-indent-hook 0)

;; General customization has been moved to vc-custom.el

(defconst vc-maximum-comment-ring-size 32
  "Maximum number of saved comments in the comment ring.")

;;; XEmacs - This is dumped into loaddefs.el already.
;(defvar diff-switches "-c"
;  "*A string or list of strings specifying switches to be passed to diff.")

;;; FSF Emacs - diff-switches may not be defined.
(or (boundp 'diff-switches)
    (defvar diff-switches "-c"
      "*A string or list of strings specifying switches to be passed to diff.")
    )

;; Variables the user doesn't need to know about.
(defvar vc-log-entry-mode nil)
(defvar vc-log-operation nil)
(defvar vc-log-after-operation-hook nil)
(defvar vc-log-writable nil)
(defvar vc-checkout-writable-buffer-hook 'vc-checkout-writable-buffer)
;; In a log entry buffer, this is a local variable
;; that points to the buffer for which it was made
;; (either a file, or a VC dired buffer).
(defvar vc-parent-buffer nil)
(defvar vc-parent-buffer-name nil)

(defvar vc-log-file)
(defvar vc-log-version)

(defconst vc-name-assoc-file "VC-names")

(defvar vc-comment-ring nil)
(defvar vc-comment-ring-index nil)
(defvar vc-last-comment-match nil)
(defvar vc-window-config nil)

;; File property caching

(defun vc-clear-context ()
  "Clear all cached file properties and the comment ring."
  (interactive)
  (fillarray vc-file-prop-obarray nil)
  ;; Note: there is potential for minor lossage here if there is an open
  ;; log buffer with a nonzero local value of vc-comment-ring-index.
  (setq vc-comment-ring nil))

;; Random helper functions

(defun vc-registration-error (file)
  (if file
      (error "File %s is not under version control" file)
    (error "Buffer %s is not associated with a file" (buffer-name))))

(defvar vc-binary-assoc nil)

(defun vc-find-binary (name)
  "Look for a command anywhere on the subprocess-command search path."
  (or (cdr (assoc name vc-binary-assoc))
      (condition-case nil
	  ;; XEmacs - use locate-file
	  (let ((full (locate-file name exec-path nil 1)))
	    (if full
		(setq vc-binary-assoc (cons (cons name full) vc-binary-assoc)))
	    full)
	;; meep error, FSF Emacs ;; untested.. someone read this!
	(catch 'found
	  (mapcar
	   (function 
	    (lambda (s)
	      (if s
		  (let ((full (concat s "/" name)))
		    (if (file-executable-p full)
			(progn
			  (setq vc-binary-assoc
				(cons (cons name full) vc-binary-assoc))
			  (throw 'found full)))))))
	   exec-path)
	  nil))))

(defun vc-do-command (okstatus command file last &rest flags)
  "Execute a version-control command, notifying user and checking for errors.
The command is successful if its exit status does not exceed OKSTATUS.
Output from COMMAND goes to buffer *vc*.  The last argument of the command is
the master name of FILE if LAST is 'MASTER, or the workfile of FILE if LAST is
'WORKFILE; this is appended to an optional list of FLAGS."
  (setq file (expand-file-name file))
  (let ((camefrom (current-buffer))
        (pwd (file-name-directory (expand-file-name file)))
        (squeezed nil)
        (vc-file (and file (vc-name file)))
        status)
;;; #### - don't know why this code was here...to beautify the echo message?
;;;        the version of code below doesn't break default-directory, but it
;;;        still might mess up CVS and RCS because they like to operate on
;;;        files in the current directory. --Stig
;;;
;;;     (if (string-match (concat "^" (regexp-quote pwd)) file)
;;;         (setq file (substring file (match-end 0)))
;;;       (setq pwd (file-name-directory file)))
    (if vc-command-messages
        (message "Running %s on %s..." command file))
    (set-buffer (get-buffer-create "*vc*"))
    (setq default-directory pwd
          file (file-name-nondirectory file))

    (set (make-local-variable 'vc-parent-buffer) camefrom)
    (set (make-local-variable 'vc-parent-buffer-name)
         (concat " from " (buffer-name camefrom)))

    (erase-buffer)

    (mapcar
     (function (lambda (s) (and s (setq squeezed (append squeezed (list s))))))
     flags)
    (if (and vc-file (eq last 'MASTER))
        (setq squeezed (append squeezed (list vc-file))))
    (if (eq last 'WORKFILE)
        (setq squeezed (append squeezed (list file))))
    (let ((exec-path (if vc-path (append vc-path exec-path) exec-path))
          ;; Add vc-path to PATH for the execution of this command.
          (process-environment (copy-sequence process-environment)))
      (setenv "PATH" (mapconcat 'identity exec-path ":"))
      (setq status (apply 'call-process command nil t nil squeezed)))
    (goto-char (point-max))
    (set-buffer-modified-p nil)         ; XEmacs - fsf uses `not-modified'
    (forward-line -1)
    (if (or (not (integerp status)) (< okstatus status))
        (progn
          (pop-to-buffer "*vc*")
          (goto-char (point-min))
          (shrink-window-if-larger-than-buffer)
          (error "Running %s...FAILED (%s)" command
                 (if (integerp status)
                     (format "status %d" status)
                   status))
          )
      (if vc-command-messages
          (message "Running %s...OK" command))
      )
    (set-buffer camefrom)
    status)
  )

;;; Save a bit of the text around POSN in the current buffer, to help
;;; us find the corresponding position again later.  This works even
;;; if all markers are destroyed or corrupted.
(defun vc-position-context (posn)
  (list posn
        (buffer-size)
        (buffer-substring posn
                          (min (point-max) (+ posn 100)))))

;;; Return the position of CONTEXT in the current buffer, or nil if we
;;; couldn't find it.
(defun vc-find-position-by-context (context)
  (let ((context-string (nth 2 context)))
    (if (equal "" context-string)
        (point-max)
      (save-excursion
        (let ((diff (- (nth 1 context) (buffer-size))))
          (if (< diff 0) (setq diff (- diff)))
          (goto-char (nth 0 context))
          (if (or (search-forward context-string nil t)
                  ;; Can't use search-backward since the match may continue
                  ;; after point.
                  (progn (goto-char (- (point) diff (length context-string)))
                         ;; goto-char doesn't signal an error at
                         ;; beginning of buffer like backward-char would
                         (search-forward context-string nil t)))
              ;; to beginning of OSTRING
              (- (point) (length context-string))))))))

(defun vc-revert-buffer1 (&optional arg no-confirm)
  ;; Most of this was shamelessly lifted from Sebastian Kremer's rcs.el mode.
  ;; Revert buffer, try to keep point and mark where user expects them in spite
  ;; of changes because of expanded version-control key words.
  ;; This is quite important since otherwise typeahead won't work as expected.
  (interactive "P")
  (widen)
  (let ((point-context (vc-position-context (point)))
        ;; Use vc-mark-marker to avoid confusion in transient-mark-mode.
        ;; XEmacs - mark-marker t, FSF Emacs - mark-marker.
        (mark-context  (if (eq (marker-buffer (vc-mark-marker)) (current-buffer))
                           (vc-position-context (vc-mark-marker))))
        ;; We may want to reparse the compilation buffer after revert
        (reparse (and (boundp 'compilation-error-list) ;compile loaded
                      ;; Construct a list; each elt is nil or a buffer
                      ;; iff that buffer is a compilation output buffer
                      ;; that contains markers into the current buffer.
                      (save-excursion
                        (mapcar (function
                                 (lambda (buffer)
                                   (set-buffer buffer)
                                   (let ((errors (or
                                                  (symbol-value 'compilation-old-error-list)
                                                  (symbol-value 'compilation-error-list)))
                                         (buffer-error-marked-p nil))
                                     (while (and (consp errors)
                                                 (not buffer-error-marked-p))
                                       (and (markerp (cdr (car errors)))
                                            (eq buffer
                                                (marker-buffer
                                                 (cdr (car errors))))
                                            (setq buffer-error-marked-p t))
                                       (setq errors (cdr errors)))
                                     (if buffer-error-marked-p buffer))))
                                (buffer-list))))))

    ;; The FSF version intentionally runs font-lock here.  That
    ;; usually just leads to a correctly font-locked buffer being
    ;; redone.  #### We should detect the cases where the font-locking
    ;; may be incorrect (such as on reverts).  We know that it is fine
    ;; during regular checkin and checkouts.

    ;; the actual revisit
    (revert-buffer arg no-confirm)
	
    ;; Reparse affected compilation buffers.
    (while reparse
      (if (car reparse)
          (save-excursion
            (set-buffer (car reparse))
            (let ((compilation-last-buffer (current-buffer)) ;select buffer
                  ;; Record the position in the compilation buffer of
                  ;; the last error next-error went to.
                  (error-pos (marker-position
                              (car (car-safe compilation-error-list)))))
              ;; Reparse the error messages as far as they were parsed before.
              (if (fboundp 'compile-reinitialize-errors)
                  (if vc-elucidated
                      (compile-reinitialize-errors '(4))
                    (compile-reinitialize-errors '(4) compilation-parsing-end)))
              ;; Move the pointer up to find the error we were at before
              ;; reparsing.  Now next-error should properly go to the next one.
              (while (and compilation-error-list
                          (/= error-pos (car (car compilation-error-list))))
                (setq compilation-error-list (cdr compilation-error-list))))))
      (setq reparse (cdr reparse)))

    ;; Restore point and mark
    (let ((new-point (vc-find-position-by-context point-context)))
      (if new-point (goto-char new-point)))
    (if mark-context
        (let ((new-mark (vc-find-position-by-context mark-context)))
          (if new-mark (set-mark new-mark))))))


(defun vc-buffer-sync (&optional not-urgent)
  ;; Make sure the current buffer and its working file are in sync
  ;; NOT-URGENT means it is ok to continue if the user says not to save.
  (if (buffer-modified-p)
      (if (or vc-suppress-confirm
              (y-or-n-p (format "Buffer %s modified; save it? " (buffer-name))))
          (save-buffer)
        (if not-urgent
            nil
          (error "Aborted")))))

;;;###autoload
(defun vc-file-status ()
  "Display the current status of the file being visited.
Currently, this is only defined for CVS and ClearCase.
The information provided in the modeline is generally sufficient for
RCS and SCCS."
  ;; by Stig@hackvan.com
  (interactive)

  (let* ((file (if vc-dired-mode (dired-get-filename)
                   (vc-buffer-sync t) buffer-file-name))
         (type (vc-backend file)))
    (cond ((null type)
           (if file
               (message "`%s' is not registered with a version control system."
                        file)
             (ding)
             (message "Buffer `%s' has no associated file."
                      (buffer-name (current-buffer)))))
          ((eq 'CVS type)
           (vc-do-command 0 "cvs" file 'WORKFILE "status" "-v")
           (set-buffer "*vc*")
           (set-buffer-modified-p nil)
           ;; reparse the status information, since we have it handy...
           (vc-parse-buffer '("Status: \\(.*\\)") file '(vc-cvs-status))
           (goto-char (point-min))
           (shrink-window-if-larger-than-buffer
            (display-buffer (current-buffer))))
          ((eq '@@ type)
           (vc-do-command 0 "cleartool" file 'WORKFILE "describe")
           (set-buffer "*vc*")
           (set-buffer-modified-p nil)
           (goto-char (point-min))
           (shrink-window-if-larger-than-buffer
            (display-buffer (current-buffer))))
          (t
           (ding)
           (message "Operation not yet defined for RCS or SCCS.")))
    ))

(defun vc-workfile-unchanged-p (file &optional want-differences-if-changed)
  ;; Has the given workfile changed since last checkout?
  (cond ((and (eq 'CVS (vc-backend file))
              (not want-differences-if-changed))

         (let ((status (vc-file-getprop file 'vc-cvs-status)))
           ;; #### - should this have some kind of timeout?  how often does
           ;; this get called?  possibly the cached information should be
           ;; flushed out of hand.  The only concern is the VC menu, which
           ;; may indirectly call this function.
           (or status                   ; #### - caching is error-prone
               (setq status (car (vc-log-info "cvs" file 'WORKFILE '("status")
                                              '("Status: \\(.*\\)")
                                              '(vc-cvs-status)))))
           (string= status "Up-to-date")))
        (t
         (let ((checkout-time (vc-file-getprop file 'vc-checkout-time))
               (lastmod (nth 5 (file-attributes file)))
               unchanged)
           (or (equal checkout-time lastmod)
               (and (or (not checkout-time) want-differences-if-changed)
                    (setq unchanged
                          (zerop (vc-backend-diff file nil nil
                                                  (not want-differences-if-changed))))
                    ;; 0 stands for an unknown time; it can't match any mod time.
                    (vc-file-setprop file 'vc-checkout-time (if unchanged lastmod 0))
                    unchanged))))))

(defun vc-owner-equal (o1 o2)
  (let ((len1 (length o1))
        (len2 (length o2)))
    (string-equal (substring o1 0 (min len1 8))
                  (substring o2 0 (min len2 8)))))

(defun vc-next-action-on-file (file verbose &optional comment)
  ;;; If comment is specified, it will be used as an admin or checkin comment.
  (let ((vc-file (vc-name file))
        (vc-type (vc-backend file))
        owner version)
    (cond

     ;; if there is no master file corresponding, create one
     ((not vc-file)
      (vc-register verbose comment))

     ;; if there is no lock on the file, assert one and get it
     ((and (not (eq vc-type 'CVS))      ;There are no locks in CVS.
           (not (setq owner (vc-locking-user file))))
      (if (and vc-checkout-carefully
               (not (vc-workfile-unchanged-p file t)))
          (if (save-window-excursion
                (pop-to-buffer "*vc*")
                (goto-char (point-min))
                (insert (format "Changes to %s since last lock:\n\n" file))
                (not (beep))
                (yes-or-no-p
                 "File has unlocked changes, claim lock retaining changes? "))
              (progn (vc-backend-steal file)
                     (vc-mode-line file))
            (if (not (yes-or-no-p "Revert to checked-in version, instead? "))
                (error "Checkout aborted.")
              (vc-revert-buffer1 t t)
              (vc-checkout-writable-buffer file))
            )
        (vc-checkout-writable-buffer file)))

     ;; a checked-out version exists, but the user may not own the lock
     ((and (not (eq vc-type 'CVS))      ;There are no locks in CVS.
           (not (string-equal owner (user-login-name))))
      (if comment
          (error "Sorry, you can't steal the lock on %s this way" file))
      (vc-steal-lock
       file
       (and verbose (read-string "Version to steal: "))
       owner))

     ;; changes to the master file needs to be merged back into the
     ;; working file
     ((and (eq vc-type 'CVS)
           ;; "0" means "added, but not yet committed"
           (not (string= (vc-file-getprop file 'vc-your-latest-version) "0"))
           (progn
             (vc-fetch-properties file)
             (not (string= (vc-file-getprop file 'vc-your-latest-version)
                           (vc-file-getprop file 'vc-latest-version)))))
      (vc-buffer-sync)
      (if (yes-or-no-p (format "%s is not up-to-date.  Merge in changes now? "
                               (buffer-name)))
          (progn
            (if (and (buffer-modified-p)
                     (not (yes-or-no-p
                           (format
                            "Buffer %s modified; merge file on disc anyhow? "
                            (buffer-name)))))
                (error "Merge aborted"))
            (if (not (zerop (vc-backend-merge-news file)))
                ;; Overlaps detected - what now?  Should use some
                ;; fancy RCS conflict resolving package, or maybe
                ;; emerge, but for now, simply warn the user with a
                ;; message.
                (message "Conflicts detected!"))
            (vc-resynch-window file t (not (buffer-modified-p))))

        (error "%s needs update" (buffer-name))))

     ((and buffer-read-only (eq vc-type 'CVS))
      (toggle-read-only)
      ;; Sites who make link farms to a read-only gold tree (or
      ;; something similar) can use the hook below to break the
      ;; sym-link.
      (run-hooks 'vc-make-buffer-writable-hook))

     ;; OK, user owns the lock on the file (or we are running CVS)
     (t
      (find-file file)

      ;; give luser a chance to save before checking in.
      (vc-buffer-sync)

      ;; Revert if file is unchanged and buffer is too.
      ;; If buffer is modified, that means the user just said no
      ;; to saving it; in that case, don't revert,
      ;; because the user might intend to save
      ;; after finishing the log entry.
      (if (and (vc-workfile-unchanged-p file)
               (not (buffer-modified-p)))
          (progn
            (if (eq vc-type 'CVS)
                (message "No changes to %s" file)

              (vc-backend-revert file)
              ;; DO NOT revert the file without asking the user!
              (vc-resynch-window file t nil)))

        ;; user may want to set nonstandard parameters
        (if verbose
            (setq version (read-string "New version level: ")))

        ;; OK, let's do the checkin
        (vc-checkin file version comment)
        )))
    ))

(defun vc-next-action-on-dir (dirname verbose &optional comment)
  ;;; If comment is specified, it will be used as an admin or checkin comment.
  (let ((vc-file (vc-name dirname))
        (vc-type (vc-backend dirname))
        owner version)
    (cond

     ;; if there is no master file corresponding, create one
     ((not vc-file)
      (vc-register verbose comment))

     ;; if there is no lock on the file, assert one and get it
     ((not (setq owner (vc-locking-user dirname)))
      (vc-checkout-writable-buffer dirname))

     ;; a checked-out version exists, but the user may not own the lock
     ((not (string-equal owner (user-login-name)))

      (if comment
          (error "Sorry, you can't steal the lock on %s this way" dirname))
      (vc-steal-lock
       dirname
       (and verbose (read-string "Version to steal: "))
       owner))

     ;; OK, user owns the lock on the file
     (t
      ;; TODO: figure out how to check if directory is unchanged

      ;; user may want to set nonstandard parameters
      (if verbose
          (setq version (read-string "New version level: ")))

      ;; OK, let's do the checkin
      (vc-checkin dirname version comment)
      ))
    ))

(defun vc-next-action-dired (file &optional writable rev workfile comment)
  ;; We've accepted a log comment, now do a vc-next-action using it on all
  ;; marked files.
  (set-buffer vc-parent-buffer)
  (dired-map-over-marks
     (save-window-excursion
     ;;; !!! dired-get-filename doesn't  DTRT if the filename entry is
     ;;; an absolute pathname..
     (let ((file (dired-get-filename)))
       (message "Processing %s..." file)
       (save-window-excursion
         (vc-next-action-on-file file nil comment)
         (message "Processing %s...done" file))))
   nil t)
  )

(defun vc-next-dir-action (verbose)
  (let* ((file default-directory)
         (owner (vc-locking-user file)))
    (cond
     ((not owner)
      (vc-checkout-writable-buffer file))
     ((vc-owner-equal owner (user-login-name))
      (vc-checkin file nil nil))
     (t
      (error "Now what do we do?")))))

;; Here's the major entry point.

;;;###autoload
(defun vc-next-action (verbose)
  "Do the next logical checkin or checkout operation on the current file.

For RCS and SCCS files:
   If the file is not already registered, this registers it for version
control and then retrieves a writable, locked copy for editing.
   If the file is registered and not locked by anyone, this checks out
a writable and locked file ready for editing.
   If the file is checked out and locked by the calling user, this
first checks to see if the file has changed since checkout.  If not,
it performs a revert.
   If the file has been changed, this pops up a buffer for entry
of a log message; when the message has been entered, it checks in the
resulting changes along with the log message as change commentary.  If
the variable `vc-keep-workfiles' is non-nil (which is its default), a
read-only copy of the changed file is left in place afterwards.
   If the file is registered and locked by someone else, you are given
the option to steal the lock.

For CVS files:
   If the file is not already registered, this registers it for version
control.  This does a \"cvs add\", but no \"cvs commit\".
   If the file is added but not committed, it is committed.
   If the file has not been changed, neither in your working area or
in the repository, a message is printed and nothing is done.
   If your working file is changed, but the repository file is
unchanged, this pops up a buffer for entry of a log message; when the
message has been entered, it checks in the resulting changes along
with the logmessage as change commentary.  A writable file is retained.
   If the repository file is changed, you are asked if you want to
merge in the changes into your working copy.

The following is true regardless of which version control system you
are using:

   If you call this from within a VC dired buffer with no files marked,
it will operate on the file in the current line.
   If you call this from within a VC dired buffer, and one or more
files are marked, it will accept a log message and then operate on
each one.  The log message will be used as a comment for any register
or checkin operations, but ignored when doing checkouts.  Attempted
lock steals will raise an error.

   For checkin, a prefix argument lets you specify the version number to use."
  (interactive "P")
  (catch 'nogo
    (if (or vc-dired-mode
            (and (eq major-mode 'dired-mode)
                 (string= "\C-xvv" (if vc-elucidated
                                       (events-to-keys (this-command-keys))
                                     (this-command-keys)))))
        (let ((files (dired-get-marked-files)))
          (if (= (length files) 1)
              (if (file-directory-p (car files))
                  (let* ((dirname (car files))
                         (registered (vc-registered dirname)))
                    (if (and registered (eq '@@ (cdr registered)))
                        (progn (vc-next-action-on-dir dirname nil)
                               (throw 'nogo nil)))
                    (vc-registration-error nil))
                (find-file-other-window (car files)))
            (vc-start-entry nil nil nil
                            "Enter a change comment for the marked files."
                            'vc-next-action-dired
                            nil
                            nil         ;after-hook could be used here??
                            )
            (throw 'nogo nil))))
    (while vc-parent-buffer
      (pop-to-buffer vc-parent-buffer))
    (if buffer-file-name
        (vc-next-action-on-file buffer-file-name verbose)
      ;; TODO: Add code to operate on ClearCase directories here.
      (vc-registration-error nil))))

;;; These functions help the vc-next-action entry point

(defun vc-checkout-writable-buffer (&optional file)
  "Retrieve a writable copy of the latest version of the current buffer's file."
  (if (and (vc-file-getprop file 'vc-need-pre-checkout-message)
           (not vc-suppress-checkout-comments))
      (progn
        (vc-start-entry file nil nil "Enter a checkout comment." 'vc-backend-checkout t 'vc-checkout))
    (vc-checkout (or file (buffer-file-name)) t)))

;;;###autoload
(defun vc-register (&optional override comment)
  "Register the current file into your version-control system."
  (interactive "P")
  (let ((master (vc-name buffer-file-name)))
    (and master (file-exists-p master)
         (error "This file is already registered"))
    (and master
         (not (y-or-n-p "Previous master file has vanished.  Make a new one? "))
         (error "This file is already registered")))
  ;; Watch out for new buffers of size 0: the corresponding file
  ;; does not exist yet, even though buffer-modified-p is nil.
  (if (and (not (buffer-modified-p))
           (zerop (buffer-size))
           (not (file-exists-p buffer-file-name)))
      (set-buffer-modified-p t))
  (vc-buffer-sync)
  (vc-admin
   buffer-file-name
   (and override
        (read-string
         (format "Initial version level for %s: " buffer-file-name)))
   comment)
  )

(defun vc-resynch-window (file &optional keep noquery)
  ;; If the given file is in the current buffer,
  ;; either revert on it so we see expanded keyworks,
  ;; or unvisit it (depending on vc-keep-workfiles)
  ;; NOQUERY if non-nil inhibits confirmation for reverting.
  ;; NOQUERY should be t *only* if it is known the only difference
  ;; between the buffer and the file is due to RCS rather than user editing!
  (and (string= buffer-file-name file)
       (if keep
           (progn
             (vc-revert-buffer1 t noquery)
             (vc-mode-line buffer-file-name))
         (progn
           (delete-window)
           (kill-buffer (current-buffer))))))

(defsubst vc-log-buffer-name (file-name)
  (format "*Log-%s*" (or file-name "Marked")))

(defun vc-start-entry (file rev comment msg action &optional writable after-hook before-hook)
  ;; Accept a comment for an operation on FILE revision REV.  If COMMENT
  ;; is nil, pop up a VC-log buffer, emit MSG, and set the
  ;; action on close to ACTION; otherwise, do action immediately.
  ;; Remember the file's buffer in vc-parent-buffer (current one if no file).
  ;; AFTER-HOOK specifies the local value for vc-log-operation-hook.
  ;; BEFORE-HOOK specifies a hook to run before even asking for the
  ;; checkin comments.
  (let* ((parent (if file (find-file-noselect file) (current-buffer)))
         (log-buffer-name (vc-log-buffer-name (if file (file-name-nondirectory file))))
         (log-buffer (get-buffer-create log-buffer-name)))
    (when before-hook
      (save-excursion
        (set-buffer parent)
        (run-hooks before-hook)))
    (if comment
        (set-buffer log-buffer)
      (let ((old-window-config (current-window-configuration)))
        (pop-to-buffer log-buffer)
        (set (make-local-variable 'vc-window-config) old-window-config)))
    (set (make-local-variable 'vc-parent-buffer) parent)
;;    (set (make-local-variable 'vc-parent-buffer-name)
;;       (concat " from " (buffer-name vc-parent-buffer)))
    (vc-mode-line (or file " (no file)"))
    ;; If file is not checked out, the log buffer will be read-only
    (setq buffer-read-only nil)
    (vc-log-mode)
    (make-local-variable 'vc-log-after-operation-hook)
    (if after-hook
        (setq vc-log-after-operation-hook after-hook))
    (setq vc-log-operation action)
    (setq vc-log-file file)
    (setq vc-log-version rev)
    (setq vc-log-writable writable)
    (if comment
        (progn
          (erase-buffer)
          (if (eq comment t)
              (vc-finish-logentry t)
            (insert comment)
            (vc-finish-logentry nil)))
      (or writable
          (vc-backend-fetch-default-comment file rev))
      (message "%s  Type C-c C-c when done." msg))))

(defun vc-admin (file rev &optional comment)
  "Check a file into your version-control system.
FILE is the unmodified name of the file.  REV should be the base version
level to check it in under.  COMMENT, if specified, is the checkin comment."

  ;; For ClearCase, we need to checkout the directory
  (let ((filedir (file-name-directory file)) user)
    (if (eq (vc-backend filedir) '@@)
        (progn
          (setq user (vc-locking-user filedir))
          (if user
              (if (not (equal user (user-login-name)))
                  (error "Directory is locked by %s." user))
            (if (cond
                 ((eq vc-checkout-dir-on-register 'ask)
                  (y-or-n-p (format "Checkout directory %s " filedir)))
                 (vc-checkout-dir-on-register)
                 (t nil))
                (vc-backend-checkout filedir t rev nil comment)
              (error "Can't register file unless directory is reserved.")
              )))))

  (vc-start-entry file rev
                  (or comment (not vc-initial-comment))
                  "Enter initial comment." 'vc-backend-admin
                  nil 'vc-register-hook 'vc-before-register-hook))

;;;###autoload
(defun vc-checkout (file &optional writable)
  "Retrieve a copy of the latest version of the given file."
  ;; XEmacs - ftp is suppressed by the check for a filename handler in
  ;;          vc-registered, so this is needless surplussage
  ;; If ftp is on this system and the name matches the ange-ftp format
  ;; for a remote file, the user is trying something that won't work.
  ;;   (if (and (string-match "^/[^/:]+:" file) (vc-find-binary "ftp"))
  ;;       (error "Sorry, you can't check out files over FTP"))
  (vc-backend-checkout file writable)
  (if (string-equal file buffer-file-name)
      (vc-resynch-window file t t))
  )

(defun vc-steal-lock (file rev &optional owner)
  "Steal the lock on the current workfile."
  (let (file-description)
    (if (not owner)
        (setq owner (vc-locking-user file)))
    (if rev
        (setq file-description (format "%s:%s" file rev))
      (setq file-description file))
    (if (not (y-or-n-p (format "Take the lock on %s from %s? "
                               file-description owner)))
        (error "Steal cancelled"))
    (pop-to-buffer (get-buffer-create "*VC-mail*"))
    (setq default-directory (expand-file-name "~/"))
    (auto-save-mode auto-save-default)
    (mail-mode)
    (erase-buffer)
    (mail-setup owner (format "Stolen lock on %s" file-description) nil nil nil
                (list (list 'vc-finish-steal file rev)))
    (goto-char (point-max))
    (insert
     (format "I stole the lock on %s, " file-description)
     (current-time-string)
     ".\n")
    (message "Please explain why you stole the lock.  Type C-c C-c when done.")))

;; This is called when the notification has been sent.
(defun vc-finish-steal (file version)
  (vc-backend-steal file version)
  (if (get-file-buffer file)
      (save-excursion
        (set-buffer (get-file-buffer file))
        (vc-resynch-window file t t))))

(defun vc-checkin (file &optional rev comment)
  "Check in the file specified by FILE.
The optional argument REV may be a string specifying the new version level
\(if nil increment the current level).  The file is either retained with write
permissions zeroed, or deleted (according to the value of `vc-keep-workfiles').
If the back-end is CVS, a writable workfile is always kept.
COMMENT is a comment string; if omitted, a buffer is
popped up to accept a comment."
  (vc-start-entry file rev comment
                  "Enter a change comment." 'vc-backend-checkin
                  nil 'vc-checkin-hook 'vc-before-checkin-hook)
  (if (and (not comment) (not (file-directory-p file)) vc-diff-on-checkin)
      (save-excursion
        (let ((tmp-buffer (current-buffer)))
          (message "Running diff...")
          (vc-diff nil)
          (message "Running diff...done")
          (set-buffer "*vc*")
          (if (get-buffer "*vc-diff*")
              (kill-buffer "*vc-diff*"))
          (rename-buffer "*vc-diff*")
          (pop-to-buffer tmp-buffer)))))

;;; Here is a checkin hook that may prove useful to sites using the
;;; ChangeLog facility supported by Emacs.
(defun vc-comment-to-change-log (&optional whoami file-name)
  "Enter last VC comment into change log file for current buffer's file.
Optional arg (interactive prefix) non-nil means prompt for user name and site.
Second arg is file name of change log.  \
If nil, uses `change-log-default-name'."
  (interactive (if current-prefix-arg
                   (list current-prefix-arg
                         (prompt-for-change-log-name))))
  ;; Make sure the defvar for add-log-current-defun-function has been executed
  ;; before binding it.
  (require 'add-log)
  (let (;; Extract the comment first so we get any error before doing anything.
        (comment (ring-ref vc-comment-ring 0))
        ;; Don't let add-change-log-entry insert a defun name.
        (add-log-current-defun-function 'ignore)
        end)
    ;; Call add-log to do half the work.
    (add-change-log-entry whoami file-name t t)
    ;; Insert the VC comment, leaving point before it.
    (setq end (save-excursion (insert comment) (point-marker)))
    (if (looking-at "\\s *\\s(")
        ;; It starts with an open-paren, as in "(foo): Frobbed."
        ;; So remove the ": " add-log inserted.
        (delete-char -2))
    ;; Canonicalize the white space between the file name and comment.
    (just-one-space)
    ;; Indent rest of the text the same way add-log indented the first line.
    (let ((indentation (current-indentation)))
      (save-excursion
        (while (< (point) end)
          (forward-line 1)
          (indent-to indentation))
        (setq end (point))))
    ;; Fill the inserted text, preserving open-parens at bol.
    (let ((paragraph-separate (concat paragraph-separate "\\|^\\s *\\s("))
          (paragraph-start (concat paragraph-start "\\|^\\s *\\s(")))
      (beginning-of-line)
      (fill-region (point) end))
    ;; Canonicalize the white space at the end of the entry so it is
    ;; separated from the next entry by a single blank line.
    (skip-syntax-forward " " end)
    (delete-char (- (skip-syntax-backward " ")))
    (or (eobp) (looking-at "\n\n")
        (insert "\n"))))


(defun vc-cc-save-logentry (comment buffer)
  (save-excursion
    (set-buffer buffer)
    (let* ((file (buffer-file-name))
           (target (concat (vc-latest-version file))))
      (cond ((string-match "/CHECKEDOUT$" target)
             (vc-do-cleartool-command "chevent"
                                      (vc-cc-build-version file target)
                                      (vc-cleanup-comment comment) "-replace"))
            (t
             (error "Can't change comment of checked-in version with this interface"))))))

(defun vc-save-logentry ()
  "Checkpoint currently entered comment"
  (interactive)
  (let ((comment-string (buffer-string))
        (parent-buffer vc-parent-buffer))
    (if (not (buffer-modified-p))
        (message "(No changes need to be saved)")
      (progn
        (save-excursion
          (set-buffer parent-buffer)
          (vc-backend-dispatch buffer-file-name
            (error "Logentry saves not supported under SCCS")
            (error "Logentry saves not supported under RCS")
            (error "Logentry saves not supported under CVS")
            (vc-cc-save-logentry comment-string parent-buffer)))
        (set-buffer-modified-p nil)))))

(defun vc-num-num-error ()
  (interactive)
  (message "Perhaps you wanted to type C-c C-c instead?"))

(defun vc-finish-logentry (&optional nocomment)
  "Complete the operation implied by the current log entry."
  (interactive)
  ;; Check and record the comment, if any.
  (let ((log-buffer (current-buffer)))
    (if (not nocomment)
        (progn
          (goto-char (point-max))
          (if (not (bolp))
              (newline))
          ;; Comment too long?
          (vc-backend-logentry-check vc-log-file)
          ;; Record the comment in the comment ring
          (if (null vc-comment-ring)
              (setq vc-comment-ring (make-ring vc-maximum-comment-ring-size)))
          (ring-insert vc-comment-ring (buffer-string))
          ))
    ;; Sync parent buffer in case the user modified it while editing the comment.
    ;; But not if it is a vc-dired buffer.
    (save-excursion
      (set-buffer vc-parent-buffer)
      (or vc-dired-mode
          (vc-buffer-sync)))
    ;; OK, do it to it
    (if vc-log-operation
        (save-excursion
          (funcall vc-log-operation
                   vc-log-file
                   vc-log-writable
                   vc-log-version
                   nil
                   (buffer-string)))
      (error "No log operation is pending"))
    ;; Return to "parent" buffer of this checkin and remove checkin window
    ;; save the vc-log-after-operation-hook of log buffer
    (let ((after-hook vc-log-after-operation-hook)
          (old-window-config vc-window-config))
      (pop-to-buffer vc-parent-buffer)
      (delete-windows-on log-buffer)
      (kill-buffer log-buffer)
      (if old-window-config (set-window-configuration old-window-config))
      ;; Now make sure we see the expanded headers
      (if buffer-file-name
          (vc-resynch-window buffer-file-name vc-keep-workfiles t))
      (if (and (not (eq vc-log-operation 'vc-next-action-dired))
               (eq major-mode 'dired-mode))
          (revert-buffer t t))
      (run-hooks after-hook))))

;; Code for access to the comment ring

(defun vc-previous-comment (arg)
  "Cycle backwards through comment history."
  (interactive "*p")
  (let ((len (ring-length vc-comment-ring)))
    (cond ((or (not len) (<= len 0))    ; XEmacs change from Barry Warsaw
           (message "Empty comment ring")
           (ding))
          (t
           (erase-buffer)
           ;; Initialize the index on the first use of this command
           ;; so that the first M-p gets index 0, and the first M-n gets
           ;; index -1.
           (if (null vc-comment-ring-index)
               (setq vc-comment-ring-index
                     (if (> arg 0) -1
                       (if (< arg 0) 1 0))))
           (setq vc-comment-ring-index
                 (mod (+ vc-comment-ring-index arg) len))
           (message "%d" (1+ vc-comment-ring-index))
           (insert (ring-ref vc-comment-ring vc-comment-ring-index))))))

(defun vc-next-comment (arg)
  "Cycle forwards through comment history."
  (interactive "*p")
  (vc-previous-comment (- arg)))

(defun vc-comment-search-reverse (str)
  "Searches backwards through comment history for substring match."
  (interactive "sComment substring: ")
  (if (string= str "")
      (setq str vc-last-comment-match)
    (setq vc-last-comment-match str))
  (if (null vc-comment-ring-index)
      (setq vc-comment-ring-index -1))
  (let ((str (regexp-quote str))
        (len (ring-length vc-comment-ring))
        (n (1+ vc-comment-ring-index)))
    (while (and (< n len) (not (string-match str (ring-ref vc-comment-ring n))))
      (setq n (+ n 1)))
    (cond ((< n len)
           (vc-previous-comment (- n vc-comment-ring-index)))
          (t (error "Not found")))))

(defun vc-comment-search-forward (str)
  "Searches forwards through comment history for substring match."
  (interactive "sComment substring: ")
  (if (string= str "")
      (setq str vc-last-comment-match)
    (setq vc-last-comment-match str))
  (if (null vc-comment-ring-index)
      (setq vc-comment-ring-index 0))
  (let ((str (regexp-quote str))
        (n vc-comment-ring-index))
    (while (and (>= n 0) (not (string-match str (ring-ref vc-comment-ring n))))
      (setq n (- n 1)))
    (cond ((>= n 0)
           (vc-next-comment (- n vc-comment-ring-index)))
          (t (error "Not found")))))

;; Additional entry points for examining version histories

;;;###autoload
(defun vc-diff (historic &optional not-urgent)
  "Display diffs between file versions.
Normally this compares the current file and buffer with the most recent
checked in version of that file.  This uses no arguments.
With a prefix argument, it reads the file name to use
and two version designators specifying which versions to compare."
  (interactive "P")
  (if vc-dired-mode
      (set-buffer (find-file-noselect (dired-get-filename))))
  (while vc-parent-buffer
    (pop-to-buffer vc-parent-buffer))
  (if historic
      (call-interactively 'vc-version-diff)
    (if (or (null buffer-file-name) (null (vc-name buffer-file-name)))
        (error
         "There is no version-control master associated with this buffer"))
    (let ((file buffer-file-name)
          unchanged)
      (or (and file (vc-name file))
          (vc-registration-error file))
      (vc-buffer-sync not-urgent)
      (setq unchanged (vc-workfile-unchanged-p buffer-file-name))
      (if unchanged
          (message "No changes to %s since latest version." file)
        (vc-backend-diff file)
        ;; Ideally, we'd like at this point to parse the diff so that
        ;; the buffer effectively goes into compilation mode and we
        ;; can visit the old and new change locations via next-error.
        ;; Unfortunately, this is just too painful to do.  The basic
        ;; problem is that the `old' file doesn't exist to be
        ;; visited.  This plays hell with numerous assumptions in
        ;; the diff.el and compile.el machinery.
        (pop-to-buffer "*vc*")
        (setq default-directory (file-name-directory file))
        (if (= 0 (buffer-size))
            (progn
              (setq unchanged t)
              (message "No changes to %s since latest version." file))
          (goto-char (point-min))
          (shrink-window-if-larger-than-buffer)))
      (not unchanged))))

;;;###autoload
(defun vc-version-diff (file rel1 rel2)
  "For FILE, report diffs between two stored versions REL1 and REL2 of it.
If FILE is a directory, generate diffs between versions for all registered
files in or below it."
  (interactive
   (progn
     (let ((file (read-file-name "File or directory to diff: "
                                 default-directory nil t nil)))
       (if (file-directory-p file)
           (let ((r1 (read-string "Older version: "))
                 (r2 (read-string "Newer version: ")))
             (list file r1 r2))
         (let ((r1 (vc-read-version
                    (format "Older version of %s: "
                            (file-name-nondirectory file))
                    file))
               (r2 (vc-read-version
                    (format "Newer version of %s: "
                            (file-name-nondirectory file))
                    file)))
           (list file r1 r2))))))
  (if (string-equal rel1 "") (setq rel1 nil))
  (if (string-equal rel2 "") (setq rel2 nil))
  (if (file-directory-p file)
      (let ((camefrom (current-buffer)))
        (set-buffer (get-buffer-create "*vc-status*"))
        (set (make-local-variable 'vc-parent-buffer) camefrom)
        (set (make-local-variable 'vc-parent-buffer-name)
             (concat " from " (buffer-name camefrom)))
        (erase-buffer)
        (insert "Diffs between "
                (or rel1 "last version checked in")
                " and "
                (or rel2 "current workfile(s)")
                ":\n\n")
        (set-buffer (get-buffer-create "*vc*"))
        (cd file)
        (vc-file-tree-walk
         (function (lambda (f)
                     (message "Looking at %s" f)
                     (and
                      (not (file-directory-p f))
                      (vc-registered f)
                      (vc-backend-diff f rel1 rel2)
                      (append-to-buffer "*vc-status*" (point-min) (point-max)))
                     )))
        (pop-to-buffer "*vc-status*")
        (insert "\nEnd of diffs.\n")
        (goto-char (point-min))
        (set-buffer-modified-p nil)
        )
    (if (zerop (vc-backend-diff file rel1 rel2))
        (message "No changes to %s between %s and %s." file rel1 rel2)
      (pop-to-buffer "*vc*"))))

(defun vc-read-version (prompt file)
  (vc-backend-dispatch file
      (read-string prompt (vc-latest-version file))
      (read-string prompt (vc-latest-version file))
      (read-string prompt (vc-latest-version file))
      ;; For ClearCase, use the version-extended path
      (let* (;; Don't show the whole gory path
             (insert-default-directory nil)
             ;; Fetch the latest version for defaults - it's not critical that
             ;; it be up-to-date, so try the property-list first.
             (latest (or (vc-file-getprop file 'vc-latest-version)
                         (vc-latest-version file)))
             (default (vc-cc-build-version file latest))
             ;; Make the current directory the branch of the latest version
             ;; This is the only way that read-file-name will work properly, if
             ;; I pass in this directory, it doesn't complete properly when
             ;; subdirectories are used.
             (default-directory (expand-file-name
                                (vc-cc-build-version file
                                                     (file-name-directory latest))))
             )
        ;; Extract just version name, but still complete
        (vc-cc-version-name
         (expand-file-name
          (read-file-name (format "%s (default %s): " prompt latest)
                          nil
                          default
                          t
                          nil
                          ))
         )
;;        ;; The completing-read method safer, but slower
;;        (completing-read prompt (vc-cc-enumerate-versions file) nil t
;;                         (or (vc-cc-version-name file) (vc-latest-version file)))
        )
      ))

;;;###autoload
(defun vc-version-other-window (rev)
  "Visit version REV of the current buffer in another window.
If the current buffer is named `F', the version is named `F.~REV~'.
If `F.~REV~' already exists, it is used instead of being re-created.

ClearCase uses the normal version-extended naming convention instead."
  (interactive
   (list
    (progn
      (if vc-dired-mode
          (set-buffer (find-file-noselect (dired-get-filename))))
      (while vc-parent-buffer
        (pop-to-buffer vc-parent-buffer))
      (vc-read-version (format "Version of %s to visit: "
                               (file-name-nondirectory buffer-file-name))
                       buffer-file-name))))

  (if (and buffer-file-name (vc-name buffer-file-name))
      (let* ((version (if (string-equal rev "")
                          (vc-latest-version buffer-file-name)
                        rev))
             (filename (vc-backend-dispatch
                        buffer-file-name
                        (concat buffer-file-name ".~" version" ~")      ;; SCCS
                        (concat buffer-file-name ".~" version" ~")      ;; RCS
                        (concat buffer-file-name ".~" version" ~")      ;; CVS
                        (vc-cc-build-version
                         (vc-cc-element-name buffer-file-name)
                                 version) ;; ClearCase
                        )))
         (or (file-exists-p filename)
             (vc-backend-checkout buffer-file-name nil version filename nil))
         (find-file-other-window filename))
    (vc-registration-error buffer-file-name)))

;; Header-insertion code

;;;###autoload
(defun vc-insert-headers ()
  "Insert headers in a file for use with your version-control system.
Headers desired are inserted at the start of the buffer, and are pulled from
the variable `vc-header-alist'."
  (interactive)
  (if vc-dired-mode
      (find-file-other-window (dired-get-filename)))
  (while vc-parent-buffer
    (pop-to-buffer vc-parent-buffer))
  (save-excursion
    (save-restriction
      (widen)
      (if (or (not (vc-check-headers))
              (y-or-n-p "Version headers already exist.  Insert another set? "))
          (progn
            (let* ((delims (cdr (assq major-mode vc-comment-alist)))
                   (comment-start-vc (or (car delims) comment-start "#"))
                   (comment-end-vc (or (car (cdr delims)) comment-end ""))
                   (hdstrings (cdr (assoc (vc-backend (buffer-file-name)) vc-header-alist))))
              (mapcar (function (lambda (s)
                                  (insert comment-start-vc s
                                          comment-end-vc "\n")))
                      hdstrings)
              (if vc-static-header-alist
                  (mapcar (function (lambda (f)
                              (if (and buffer-file-name
                                       (string-match (car f) buffer-file-name))
                                  (insert (format (cdr f) (car hdstrings))))))
                          vc-static-header-alist))
              )
            )))))

;; The VC directory submode.  Coopt Dired for this.
;; All VC commands get mapped into logical equivalents.

(defvar vc-dired-prefix-map (make-sparse-keymap))
(define-key vc-dired-prefix-map "\C-xv" vc-prefix-map)

(if (not (fboundp 'set-keymap-name))
    (defun set-keymap-name (&rest args)
      nil))

(set-keymap-name vc-dired-prefix-map 'vc-dired-prefix-map)

(or (not (boundp 'minor-mode-map-alist))
    (assq 'vc-dired-mode (symbol-value 'minor-mode-map-alist))
    (setq minor-mode-map-alist
          (cons (cons 'vc-dired-mode vc-dired-prefix-map)
                minor-mode-map-alist)))

;;;###autoload
(defun vc-dired-mode (&optional arg)
  "The augmented Dired minor mode used in VC directory buffers.
All Dired commands operate normally.  Users currently locking listed files
are listed in place of the file's owner and group.
Keystrokes bound to VC commands will execute as though they had been called
on a buffer attached to the file named in the current Dired buffer line."
  (interactive "P")

  ;; Behave like a proper minor-mode
  (setq vc-dired-mode
        (if (interactive-p)
            (if (null arg)
                (not vc-dired-mode)
              ;; Check if the numeric arg is positive
              (> (prefix-numeric-value arg) 0)
              )
          ;; else
          ;; Use the car if it's a list
          (if (consp arg) (setq arg (car arg)))

          (if (symbolp arg)
              (if (null arg)
                  (not vc-dired-mode)  ;; toggle mode switch
                (not (eq '- arg))) ;; True if symbol is not '-
            ;; else
            ;; assume it's a number and check that
            (> arg 0)
            )))

  (if (or (not (eq major-mode 'dired-mode)))
      (setq vc-dired-mode nil))

  (if (and vc-dired-mode vc-dired-highlight)
      (progn
        (vc-reformat-dired-buffer)
        (setq vc-mode " under VC")
        )
    (setq vc-mode ""))
  )

;;(defvar vc-dired-width 15
;;  "Field width of locking-user information inserted into dired buffer")

;; Create a face for highlighting checked out files in vc-dired
(if (not (memq 'vc-dired-checkedout-face (face-list)))
    (progn
      (make-face 'vc-dired-checkedout-face)
      ;; FSF Emacs - doesn't have set-face-doc-string.
      (if vc-elucidated
	  (set-face-doc-string 'vc-dired-checkedout-face
			       "Checked out files in VC dired buffers"))
      (set-face-foreground 'vc-dired-checkedout-face "yellow")))

;; Create a face for highlighting registered files in vc-dired
(if (not (memq 'vc-dired-registered-face (face-list)))
    (progn
      (make-face 'vc-dired-registered-face)
      ;; FSF Emacs - doesn't have set-face-doc-string.
      (if vc-elucidated
	  (set-face-doc-string 'vc-dired-registered-face
			       "Registered files in VC dired buffers"))
      (set-face-foreground 'vc-dired-registered-face "#6920ac")))

;;;###autoload
(defun vc-reformat-dired-buffer (&optional filelist)
  "Reformats the current dired buffer using the given file alist."
  (let* ((givenlist filelist)
         (directory default-directory)
         subdir)
    (save-excursion
      (goto-char (point-min))

      (while (not (eobp))
        (cond
         ;; Look for directory markers
         ((setq subdir (dired-get-subdir))
          (setq directory subdir)
          ;; If no filelist was passed in, look up the checkouts.
          ;; The routine vc-list-registered-files is optimized (at least for
          ;; SCCS and ClearCase) to return the checked out files quickly.
          ;; This routine should be much faster than checking each file.
          (if (not givenlist)
              (setq filelist (vc-list-registered-files 'all nil directory)))
          ;; If no registered files are found, we don't need to check each
          ;; file, and it's very slow.  The filelist should contain something
          ;; so it doesn't attempt to do this.
          (if (null filelist) (setq filelist '(nil)))
          (message "Reformatting %s..." directory))

         ;; Look for files (the safest way to get the filename)
         ((setq fullpath (dired-get-filename nil t))
          ;; Expand it to get rid of . and .. entries
          (setq fullpath (expand-file-name fullpath))
	  ;; Only modify directory listings of the correct format
          (and
	   (looking-at
	    "..\\([drwxlts-]+ \\) *[0-9]+ \\(.+\\) +[0-9]+\\( [^ 0-9]+ [0-9 ][0-9] .*\\)")
	   (let* ((file-registered (assoc fullpath filelist))
		  (owner (if file-registered (cdr file-registered)
			   ;; If a filelist was not specified, try to find the
			   ;; locking user.  The only time this should happen
			   ;; is when we are updating a single file entry.  For
			   ;; whole subdirectories, the filelist should have
			   ;; been generated.
			   (save-match-data
			     (and (null filelist)
				  (vc-registered fullpath)
				  (vc-locking-user fullpath)))))
		  (info-length (- (match-end 2) (match-beginning 2)))
		  (rep (format "%-8s RESERVED" owner))
		  )
	     
	     ;; Remove this element from the alist (in case multiple users have
	     ;; a file checked out)
	     (if (consp file-registered) (setcar file-registered nil))
	     
	     ;; Highlight the line if the file is reserved
	     (if owner
		 (progn
		   (goto-char (match-beginning 2))
		   ;; Replace the user/group with user/RESERVED
		   (let ((buffer-read-only nil))
		     (cond ((>= info-length 17) (setq info-length 17))
			   ((>= info-length 8) (setq info-length 8))
			   (t (error "Bad format.")))
		     
		     (delete-char info-length)
		     (insert (substring rep 0 info-length))
		     )
		   
		   ;; Highlight the checked out files sort of like xclearcase
		   (if (featurep 'text-props)
		       (put-text-property (match-beginning 2) (match-end 2)
					  'face 'vc-dired-checkedout-face)))
	       ;; Else if it is registered, but not checked out
	       (if (or file-registered
		       (and (not filelist)
			    (save-match-data (vc-registered fullpath))))
		   ;; Highlight the registered files
		   (if (featurep 'text-props)
		       (put-text-property (match-beginning 2) (match-end 2)
					  'face 'vc-dired-registered-face)))
	       )))))
        (forward-line 1))       ;; go to next line
      ))
  (message "Reformatting...Done")
  )

(defvar vc-dired-listing-switches (concat dired-listing-switches "d"))

(defun vc-list-registered-files (verbose recurse &optional directory)
  "Returns an alist of files to users in the directory."
  (or directory (setq directory default-directory))
  (let ((filelist nil) (default-directory directory))
    (cond
     ;; ClearCase
     ((eq (vc-backend directory) '@@)

      (message "Listing ClearCase checkouts...")

      ;; File-tree walk is too slow to use for ClearCase
      (setq filelist (vc-cc-list-checkouts directory verbose recurse))

      ;; Check if the directory itself is checked-out
      (let* ((dirname (directory-file-name directory))
             (user (vc-locking-user dirname)))
        (if user (setq filelist (cons (cons dirname user) filelist)))
        ;; Check the parent too, he's in the list
        (setq dirname (directory-file-name (expand-file-name (concat directory "..")))
              user (vc-locking-user dirname))
        (if user (setq filelist (cons (cons dirname user) filelist))))

      ;; List all registered files if requested
      (if (eq verbose 'all)
          ;; Add all the rest of the non-private files to the list
          (let ((tree-walk-function
                 (function (lambda (file)
                             (let ((fullpath (expand-file-name file directory)))
                               (if (and (not (assoc fullpath filelist))
                                        (clearcase-element-p file))
                                   (setq filelist (cons (cons fullpath nil)
                                                        filelist))
                                 ))))))
            ;; Add all the rest of the non-private files to the list
            (if recurse
                (vc-file-tree-walk tree-walk-function)
              (vc-dir-all-files tree-walk-function))
            ))
      (message "Listing ClearCase checkouts...done")
      )
     ;; SCCS
     ((or (file-directory-p "SCCS") (eq vc-default-back-end 'SCCS))

      (message "Listing SCCS checkouts...")

      ;; If recursive was requested, use vc-subdir-tree-walk
      (if recurse
          (vc-subdir-tree-walk 'vc-sccs-info-list verbose)
        (vc-sccs-info-list directory verbose))

      ;; List all registered files if requested
      (if (eq verbose 'all)
          (let ((tree-walk-function
                 (function (lambda (file)
                             (let ((fullpath (expand-file-name file directory)))
                               (if (and (not (file-directory-p fullpath))
                                        (not (assoc fullpath filelist))
                                        ;; Cut to the chase
                                        (file-exists-p
                                         (format
                                          "%sSCCS/s.%s"
                                          (file-name-directory file)
                                          (file-name-nondirectory file)))
                                        )
                                   (setq filelist (cons (cons fullpath nil)
                                                        filelist))
                                 ))))))
            ;; Add all the rest of the registered files to the list
            (if recurse
                (vc-file-tree-walk tree-walk-function)
              (vc-dir-all-files tree-walk-function))
            ))

      (message "Listing SCCS checkouts...done")
      )
     (t
      ;; All other VC systems... this may be too slow for comfort
      (let ((tree-walk-function
             (function (lambda (f)
                         (if (vc-registered f)
                             (let ((user (vc-locking-user f)))
                               (if (cond ((null verbose))
                                         ((eq verbose 'all))
                                         ((eq verbose 'yours) (string-equal user (user-login-name))))
                                   (setq filelist
                                         (cons (cons f user) filelist)))))))))
        (if recurse
            (vc-file-tree-walk tree-walk-function)
          (vc-dir-all-files tree-walk-function)))
      )
     )
    ;; Return the filelist
    filelist
    ))

(defun vc-cc-list-checkouts (directory verbose &optional recurse)
  (or directory (setq directory default-directory))
  (vc-cc-cd directory)

  (let* (;; Trim off the view specifier
         (reldir (vc-cc-relpath directory))
         ;; Don't use avobs, it takes way too long
         (cmd (concat
               "lsco -fmt \"%n %u %Tf\\n\""
               (if (eq verbose 'yours) " -me" "")
               ;; If recursive list requested, use "-r", otherwise,
               ;; assume the request is for a single vc-dired-mode
               ;; directory, and use "-cview" to restrict list to
               ;; checkouts in the current view.
               (if recurse " -r" "")
               ;; If the directory is view-specific, use -cview, otherwise all
               (if (string-match "^/view/\\([^/]+\\)" directory) " -cview" "")
               ;; Put the directory so all names will be fullpaths
               ;; For some reason ClearCase adds an extra slash if you leave
               ;; the trailing slash on the directory, so we need to remove it.
               " " (directory-file-name (or reldir directory))))
         (string (vc-cleartool-cmd (or reldir directory) cmd))
         (tmplist (vc-split-string "\n" string)))
    ;; Return the reversed alist constructed from the file/version pairs
    (cdr (nreverse
          (mapcar (function
                   (lambda (string)
                     (let* ((split-list (vc-split-string " " string))
                            (fullname (vc-cc-build-version
                                       (car split-list) nil
                                       (if (or reldir recurse)
                                           (caddr split-list))))
                            (locking-user (cadr split-list)))
                       ;; Record the locking user for future operations
                       (vc-file-setprop fullname 'vc-locking-user locking-user)
                       (cons fullname locking-user))
                     ))
                  tmplist)))
    ))

;; This assumes that the caller has defined the variable filelist.
;; Should be used only by vc-list-registered-files.
(defun vc-sccs-info-list (directory verbose)
  "List the SCCS checked out files in the given directory."

  (if (file-directory-p (concat directory "SCCS/"))
      (save-excursion
        ;; Call sccs info to list all checked out files
        (apply 'vc-do-command 0 "sccs" directory nil
               "info" (if (eq verbose 'yours) '("-U") nil))
        (set-buffer "*vc*")
        (beginning-of-buffer)

        ;; Find all lines matching the format and create a filelist
        (while (re-search-forward
                "^[ \t]*\\([^ \t\n]*\\): being edited: \\([0-9.]+\\) \\([0-9.]+\\) \\([^ \t\n]+\\) .*$"
                nil t)
          (let* ((file (match-string 1))
                 (user (match-string 4))
                 (fullpath (expand-file-name file directory)))

            ;; Add the entry to the filelist if not there already
            (if (not (assoc fullpath filelist))
                (setq filelist (cons (cons fullpath user) filelist)))

            ;; Record the locking user for future operations
            (vc-file-setprop (expand-file-name file directory)
                             'vc-locking-user user)
          ))
        )
    ))

;; This is a very useful function, and I'm surprised that I couldn't find an
;; equivalent
(defun find-base-dir (dirlist)
  "Find the base directory of the given list of directories."
  (if (cdr dirlist)
      (let ((dirlist (mapcar (function (lambda (x) (vc-split-string "/" x)))
                             dirlist))
            prelist first postlist)
        (setq postlist
              (catch 'done
                (while t
                  (setq
                   first nil
                   dirlist (mapcar (function (lambda (list)
                                               (if first
                                                   (if (not (string-equal first (car list)))
                                                       (throw 'done prelist))
                                                 (setq first (car list)))
                                               (if (consp list) (cdr list) list)))
                                   dirlist))
                  (setq prelist (append prelist (list first)))
                  )))
        (if postlist
            (concat (mapconcat 'identity postlist "/") "/")
          "/")
        )
    ;; Otherwise, only one thing in the list
    (file-name-directory (car dirlist))
    )
  )


;;; Note in Emacs 18 the following defun gets overridden
;;; with the symbol 'vc-directory-18.  See below.
;;;###autoload
(defun vc-directory (dir verbose)
  "Show version-control status of all files under the directory DIR.
With a single prefix argument, all registered files are listed.
With two prefix arguments, only files you have checked out are listed.

Files are listed in a dired buffer, in `vc-dired-mode'."
  (interactive "DVC status of directory: \nP")

  ;; Determine value of verbose flag from prefix arg
  (setq verbose (cond ((equal '(4) verbose) 'all)
                      ((equal '(16) verbose) 'yours)
                      (t nil)))

  (if (null dir) (setq dir default-directory))

  (let* (nonempty
         dired-buf
         (default-directory dir)
         (filelist (vc-list-registered-files verbose t))
         ;; Local modifications to hook required
         (dired-after-readin-hook dired-after-readin-hook)
         )

    ;; Remove the vc-find-dir-hook, so that the call to
    ;; `vc-reformat-dired-buffer' can't possibly take place until it's done here.
    (remove-hook 'dired-after-readin-hook 'vc-find-dir-hook)

    (if filelist
        (save-excursion
          ;; This uses a semi-documented feature of dired; giving a switch
          ;; argument forces the buffer to refresh each time.
          (message "Reading directory entries...")
          ;; Because ClearCase lists the views the files were found in, the
          ;; highest level must be found
          (let ((uniquify-buffer-name-style nil)  ;; shut off uniquify temporarily
                (files (mapcar 'car filelist)))
            (dired (cons (find-base-dir files)
                         (reverse files))
                   vc-dired-listing-switches)
            (message "Reading directory entries...done")
            (setq dired-buf (current-buffer))
            (rename-buffer (generate-new-buffer-name
                            (concat "VC:" (or (vc-cc-relpath dir) dir))))
            (setq nonempty (not (zerop (buffer-size))))))
          )
    (if nonempty
        (progn
          (pop-to-buffer dired-buf)
          (setq vc-dired-mode t
                vc-mode " under VC")
          ;; Now reformat the dired buffer with the file list already obtained.
          (vc-reformat-dired-buffer filelist)
          )

      (message "No files are currently %s under %s"
               (cond ((eq 'all verbose) "registered")
                     ((eq 'yours verbose) "locked by you")
                     (t "locked"))
               default-directory))
    ))

;; Emacs 18 version
(defun vc-directory-18 (dir verbose)
  "Show version-control status of all files under the directory DIR.
With a single prefix argument, all registered files are listed.
With two prefix arguments, only files you have checked out are listed.

Files are listed in a plain buffer, with the name of the locking user."
  (interactive "DVC status of directory: \nP")

  ;; Determine value of verbose flag from prefix arg
  (setq verbose (cond ((equal '(4) verbose) 'all)
                      ((equal '(16) verbose) 'yours)
                      (t nil)))

  (if (null dir) (setq dir default-directory))

  (let ((default-directory dir)
        (action (cond ((eq 'all verbose) "registered")
                      ((eq 'yours verbose) "locked by you")
                      (t "locked"))))
    (save-excursion
      (set-buffer (get-buffer-create "*vc-status*"))
      (erase-buffer)
      (cd dir)
      (insert "Files " action " under " default-directory "\n\n")
      (display-buffer "*vc-status*")
      (sit-for 0)

      ;; Map over the file list and print
      (let ((filelist (vc-list-registered-files verbose t default-directory)))
        (if filelist
            (progn
              (mapcar
               (function (lambda (f)
                           (let ((user (cdr f)) (f (car f)))
                             (if (cond ((null verbose) user)
                                       ((eq verbose 'all))
                                       ((eq verbose 'yours) (equal user (user-login-name))))
                                 (progn
                                   (insert (format
                                            "%s      %s\n"
                                            (concat user) f)))))))
               filelist)

              (pop-to-buffer "*vc-status*" t)
              (goto-char (point-min))
              (shrink-window-if-larger-than-buffer))
          (message "No files are currently %s under %s"
                   action default-directory)))
      )))

(cond ((string-lessp emacs-version "18")
       (fset 'vc-directory 'vc-directory-18)
       ;; Emacs 18 also lacks these.
       (or (boundp 'compilation-old-error-list)
           (setq compilation-old-error-list nil))
       )
      ;; XEmacs with efs does work
      ((and (string-match "Lucid" emacs-version)
            (or (and
                 (= emacs-major-version 19)
                 (<= emacs-minor-version 14))
                (< emacs-major-version 19)))
       ;; lucid emacs does not have the new dired yet
       (fset 'vc-directory 'vc-directory-18)
       )
      )

;; Named-configuration support for SCCS

(defun vc-add-triple (name file rev)
  (save-excursion
    (find-file (concat (vc-backend-subdirectory-name file) "/" vc-name-assoc-file))
    (goto-char (point-max))
    (insert name "\t:\t" file "\t" rev "\n")
    (basic-save-buffer)
    (kill-buffer (current-buffer))
    ))

(defun vc-record-rename (file newname)
  (save-excursion
    (find-file (concat (vc-backend-subdirectory-name file) "/" vc-name-assoc-file))
    (goto-char (point-min))
    ;; (replace-regexp (concat ":" (regexp-quote file) "$") (concat ":" newname))
    (while (re-search-forward (concat ":" (regexp-quote file) "$") nil t)
      (replace-match (concat ":" newname) nil nil))
    (basic-save-buffer)
    (kill-buffer (current-buffer))
    ))

(defun vc-lookup-triple (file name)
  ;; Return the numeric version corresponding to a named snapshot of file
  ;; If name is nil or a version number string it's just passed through
  (cond ((null name) name)
        ((let ((firstchar (aref name 0)))
           (and (>= firstchar ?0) (<= firstchar ?9)))
         name)
        (t
         (car (vc-master-info
               (concat (vc-backend-subdirectory-name file) "/" vc-name-assoc-file)
               (list (concat name "\t:\t" file "\t\\(.+\\)"))))
         )))

;; Named-configuration entry points

(defun vc-locked-example ()
  ;; Return an example of why the current directory is not ready to be snapshot
  ;; or nil if no such example exists.
  (catch 'vc-locked-example
    (vc-file-tree-walk
     (function (lambda (f)
                 (if (and (vc-registered f) (vc-locking-user f))
                     (throw 'vc-locked-example f)))))
    nil))

;;;###autoload
(defun vc-create-snapshot (name)
  "Make a snapshot called NAME.
The snapshot is made from all registered files at or below the current
directory.  For each file, the version level of its latest
version becomes part of the named configuration."
  (interactive "sNew snapshot name: ")
  (let ((locked (vc-locked-example)))
    (if locked
        (error "File %s is locked" locked)
      (progn
        (vc-backend-define-name default-directory name)
        (vc-file-tree-walk
          (function (lambda (f) (and
                                 (vc-name f)
                                 (vc-backend-assign-name f name)))))
      ))))

;;;###autoload
(defun vc-assign-name (name)
  "Assign a NAME to the current file version.
In vc-dired-mode all marked files are labeled."
  (interactive (list (vc-read-label "Label: ")))
  (let ((filelist
         (if vc-dired-mode (dired-get-marked-files)
                (list (buffer-file-name))))
        file)
    (while filelist
      (setq file (car filelist) filelist (cdr filelist))
      (if (and (vc-registered file) (vc-locking-user file))
          (error "File %s is locked" file)
        (vc-backend-assign-name file name))
      )))

(defun vc-read-label (prompt)
  "Read a label name."
  (if (eq (vc-backend default-directory) '@@)
      (let* ((string (vc-cleartool-cmd default-directory
                                       "lstype -kind lbtype -short"))
             labels)
        (mapcar (function (lambda (arg)
                            (if (string-match "(locked)" arg)
                                nil
                              (setq labels (cons (list arg) labels)))))
                (vc-split-string "\n" string))
        (completing-read prompt labels nil t))
    ;; Otherwise, just read any string
    (read-string prompt)
    ))

;;;###autoload
(defun vc-retrieve-snapshot (name)
  "Retrieve the snapshot called NAME.
This function fails if any files are locked at or below the current directory
Otherwise, all registered files are checked out (unlocked) at their version
levels in the snapshot."
  (interactive "sSnapshot name to retrieve: ")
  (let ((locked (vc-locked-example)))
    (if locked
        (error "File %s is locked" locked)
      (vc-file-tree-walk
       (function (lambda (f) (and
                              (vc-name f)
                              (vc-error-occurred
                               (vc-backend-checkout f nil name))))))
      )))

;; Miscellaneous other entry points

;;;###autoload
(defun vc-print-log ()
  "List the change log of the current buffer in a window."
  (interactive)
  (if vc-dired-mode
      (set-buffer (find-file-noselect (dired-get-filename))))
  (while vc-parent-buffer
    (pop-to-buffer vc-parent-buffer))
  (if (and buffer-file-name (vc-name buffer-file-name))
      (let ((file buffer-file-name))
        (vc-backend-print-log file)
        (pop-to-buffer (get-buffer-create "*vc*"))
        (setq default-directory (file-name-directory file))
        (while (looking-at "=*\n")
          (delete-char (- (match-end 0) (match-beginning 0)))
          (forward-line -1))
        (goto-char (point-min))
        (if (looking-at "[\b\t\n\v\f\r ]+")
            (delete-char (- (match-end 0) (match-beginning 0))))
        (shrink-window-if-larger-than-buffer)
        )
    (vc-registration-error buffer-file-name)))

;;;###autoload
(defun vc-revert-buffer ()
  "Revert the current buffer's file back to the latest checked-in version.
This asks for confirmation if the buffer contents are not identical
to that version.
If the back-end is CVS, this will give you the most recent revision of
the file on the branch you are editing."
  (interactive)
  (while vc-parent-buffer
    (pop-to-buffer vc-parent-buffer))
  (let (filelist file changed keep-buffer new-buffer
        (obuf (current-buffer)))

    (if vc-dired-mode
        (setq filelist (dired-get-marked-files))
      (setq filelist (list (buffer-file-name))))

    (while filelist
      (setq file (car filelist) filelist (cdr filelist)
            changed nil)

      (if (file-directory-p file)
          (setq new-buffer nil keep-buffer t)

        ;; Otherwise find the file
        (setq new-buffer (find-buffer-visiting file))

        (if new-buffer
            (setq keep-buffer t)
          (setq new-buffer (find-file-noselect file)
                keep-buffer nil))

        (set-buffer new-buffer)
        )

      (if (or
           (not (vc-name file))
           (not (equal (vc-locking-user file) (user-login-name)))
           (and (not (file-directory-p file))
                (setq changed (vc-diff nil t))
                (or vc-suppress-confirm
                    (not (yes-or-no-p "Discard changes? ")))))
          (progn
            (unless keep-buffer (kill-buffer new-buffer))
            (message "Revert of %s cancelled" file))
        (set-buffer obuf)
        (if changed
            (delete-window))
        (vc-backend-revert file)
        (vc-resynch-window file t t)
        (if keep-buffer
            (if new-buffer (progn (set-buffer new-buffer) (revert-buffer nil t)))
          (kill-buffer new-buffer))

        ;; See if we can keep dired up-to-date
        (if (fboundp 'dired-relist-file)
            (dired-relist-file file)
          ))
      )))

;;;###autoload
(defun vc-cancel-version (norevert)
  "Get rid of most recently checked in version of this file.
A prefix argument means do not revert the buffer afterwards."
  (interactive "P")
  (if vc-dired-mode
      (find-file-other-window (dired-get-filename)))
  (while vc-parent-buffer
    (pop-to-buffer vc-parent-buffer))
  (let* ((target (concat (vc-latest-version (buffer-file-name))))
         (yours (concat (vc-your-latest-version (buffer-file-name))))
         (prompt (if (string-equal yours target)
                     "Remove your version %s from master? "
                   "Version %s was not your change.  Remove it anyway? ")))
    (if (null (yes-or-no-p (format prompt target)))
        nil
      (vc-backend-uncheck (buffer-file-name) target)
      (if (or norevert
              (not (yes-or-no-p "Revert buffer to most recent remaining version? ")))
          (vc-mode-line (buffer-file-name))
        (vc-checkout (buffer-file-name) nil)))
    ))

;;;###autoload
(defun vc-rename-file (old new)
  "Rename file OLD to NEW, and rename its master file likewise."
  (interactive "fVC rename file: \nFRename to: ")
  ;; There are several ways of renaming files under CVS 1.3, but they all
  ;; have serious disadvantages.  See the FAQ (available from think.com in
  ;; pub/cvs/).  I'd rather send the user an error, than do something he might
  ;; consider to be wrong.  When the famous, long-awaited rename database is
  ;; implemented things might change for the better.  This is unlikely to occur
  ;; until CVS 2.0 is released.  --ceder 1994-01-23 21:27:51
  (if (eq (vc-backend old) 'CVS)
      (error "Renaming files under CVS is dangerous and not supported in VC."))
  (if (eq (vc-backend old) '@@)
      (error "VC's ClearCase support cannot rename files."))
  (let ((oldbuf (get-file-buffer old)))
    (if (and oldbuf (buffer-modified-p oldbuf))
        (error "Please save files before moving them"))
    (if (get-file-buffer new)
        (error "Already editing new file name"))
    (if (file-exists-p new)
        (error "New file already exists"))
    (let ((oldmaster (vc-name old)))
      (if oldmaster
          (progn
            (if (vc-locking-user old)
                (error "Please check in files before moving them"))
            (if (or (file-symlink-p oldmaster)
                    ;; This had FILE, I changed it to OLD. -- rms.
                    (file-symlink-p (vc-backend-subdirectory-name old)))
                (error "This is not a safe thing to do in the presence of symbolic links"))
            (rename-file
             oldmaster
             (let ((backend (vc-backend old))
                   (newdir (or (file-name-directory new) ""))
                   (newbase (file-name-nondirectory new)))
               (catch 'found
                 (mapcar
                  (function
                   (lambda (s)
                     (if (eq backend (cdr s))
                         (let* ((newmaster (format (car s) newdir newbase))
                                (newmasterdir (file-name-directory newmaster)))
                           (if (or (not newmasterdir)
                                   (file-directory-p newmasterdir))
                               (throw 'found newmaster))))))
                  vc-master-templates)
                 (error "New file lacks a version control directory"))))))
      (if (or (not oldmaster) (file-exists-p old))
          (rename-file old new)))
    ;; ?? Renaming a file might change its contents due to keyword expansion.
    ;; We should really check out a new copy if the old copy was precisely equal
    ;; to some checked in version.  However, testing for this is tricky....
    (if oldbuf
        (save-excursion
          (set-buffer oldbuf)
          (set-visited-file-name new)
          (set-buffer-modified-p nil))))
  ;; This had FILE, I changed it to OLD. -- rms.
  (vc-backend-dispatch old
    (vc-record-rename old new)          ;SCCS
    ;; #### - This CAN kinda be done for both rcs and
    ;; cvs.  It needs to be implemented. -- Stig
    nil                                 ;RCS
    nil                                 ;CVS
    nil                                 ;CC
    )
  )

;;;###autoload
(defun vc-update-change-log (&rest args)
  "Find change log file and add entries from recent RCS logs.
The mark is left at the end of the text prepended to the change log.
With prefix arg of C-u, only find log entries for the current buffer's file.
With any numeric prefix arg, find log entries for all files currently visited.
Otherwise, find log entries for all registered files in the default directory.
From a program, any arguments are passed to the `rcs2log' script."
  (interactive
   (cond ((consp current-prefix-arg)    ;C-u
          (list buffer-file-name))
         (current-prefix-arg            ;Numeric argument.
          (let ((files nil)
                (buffers (buffer-list))
                file)
            (while buffers
              (setq file (buffer-file-name (car buffers)))
              (and file (vc-backend file)
                   (setq files (cons file files)))
              (setq buffers (cdr buffers)))
            files))
         (t
          (let ((RCS (concat default-directory "RCS")))
            (and (file-directory-p RCS)
                 (mapcar (function
                          (lambda (f)
                            (if (string-match "\\(.*\\),v$" f)
                                (substring f 0 (match-end 1))
                              f)))
                         (directory-files RCS nil "...\\|^[^.]\\|^.[^.]")))))))
  (let ((odefault default-directory))
    (find-file-other-window (find-change-log))
    (barf-if-buffer-read-only)
    (vc-buffer-sync)
    (undo-boundary)
    (goto-char (point-min))
    (push-mark)
    (message "Computing change log entries...")
    (message "Computing change log entries... %s"
             (if (or (null args)
                     (eq 0 (apply 'call-process "rcs2log" nil t nil
                                  "-n"
                                  (user-login-name)
                                  (user-full-name)
                                  user-mail-address
                                  (mapcar (function
                                           (lambda (f)
                                             (file-relative-name
                                              (if (file-name-absolute-p f)
                                                  f
                                                (concat odefault f)))))
                                          args))))
                 "done" "failed"))))

;; Functions for querying the master and lock files.

;; %%% The nodates argument is not used anywhere.
;; %%% To merge this requires changing the format of patterns.
(defun vc-parse-buffer (patterns &optional file properties nodates)
  ;; Each pattern is of the form:
  ;;    regex                ; subex is 1, and date-subex is 2 (or nil)
  ;;    (regex subex date-subex)
  ;;
  ;; Use PATTERNS to parse information out of the current buffer by matching
  ;; each REGEX in the list and the returning the string matched by SUBEX.
  ;; If a DATE-SUBEX is present, then the SUBEX from the match with the
  ;; highest value for DATE-SUBEX (string comparison is used) will be
  ;; returned.
  ;;
  ;; If FILE and PROPERTIES are given, the latter must be a list of
  ;; properties of the same length as PATTERNS; each property is assigned
  ;; the corresponding value.
  (mapcar (function (lambda (p)
             (goto-char (point-min))
             (if (and (not nodates) (string-match "\\\\(.*\\\\(" p))
                 (let ((latest-date "") (latest-val))
                   (while (re-search-forward p nil t)
                     (let ((date (vc-match-substring 2)))
                       (if (string< latest-date date)
                           (progn
                             (setq latest-date date)
                             (setq latest-val
                                   (vc-match-substring 1))))))
                   latest-val))
             (prog1
                 (let ((value nil))
                   (if (re-search-forward p nil t)
                       (setq value (vc-match-substring 1)))
                        (if file
                            (vc-file-setprop file (car properties) value))
                   value)
               (setq properties (cdr properties)))))
          patterns)
  )

(defun vc-master-info (file fields &optional rfile properties)
  ;; Search for information in a master file.
  (if (and file (file-exists-p file))
      (save-excursion
        (let ((buf))
          (setq buf (create-file-buffer file))
          (set-buffer buf))
        (erase-buffer)
        (insert-file-contents file nil)
        (set-buffer-modified-p nil)
        (auto-save-mode nil)
        (prog1
            (vc-parse-buffer fields rfile properties)
          (kill-buffer (current-buffer)))
        )
    (if rfile
        (mapcar
         (function (lambda (p) (vc-file-setprop rfile p nil)))
         properties))
    )
  )

;; %%% The nodates argument is not used anywhere.
(defun vc-log-info (command file last flags patterns &optional properties nodates)
  ;; Search for information in log program output
  (if (and file (file-exists-p file))
      (save-excursion
        (let ((buf))
          (setq buf (get-buffer-create "*vc*"))
          (set-buffer buf)
          ;; Special case for cleartool commands
          (if (string-equal command "cleartool")
              (insert (vc-cleartool-cmd file
                       ;; Concatenate each flag and the file name into a
                       ;; single space separated string.
                       (mapconcat 'identity (nconc flags (list file)) " ")))
            (apply 'vc-do-command 0 command file last flags))
          (set-buffer-modified-p nil)
          (prog1
              (vc-parse-buffer patterns file properties nodates)
            (kill-buffer buf))))
    (if file
        (mapcar
         (function (lambda (p) (vc-file-setprop file p nil)))
         properties))))

;;;###autoload
(defun vc-locking-user (file)
  "Return the name of the person currently holding a lock on FILE.
Return nil if there is no such person.
Under CVS, a file is considered locked if it has been modified since it
was checked out...even though it may well be writable by you."
  (setq file (expand-file-name file))   ; use full pathname
  (cond ((eq (vc-backend file) 'CVS)
         (if (vc-workfile-unchanged-p file t)
             nil
           ;; XEmacs - ahead of the pack...
           (user-login-name (nth 2 (file-attributes file)))))
        (t
         ;; #### - this can probably be cleaned up as a result of the changes to
         ;; user-login-name...
	 ;; NT Emacs - don't use vc-mistrust-permissions on NT.
	 (if (and (not (memq window-system '(win32 w32)))
 		  (or (not vc-keep-workfiles)
 		      (eq vc-mistrust-permissions 't)
 		      (and vc-mistrust-permissions
 			   (funcall vc-mistrust-permissions (vc-backend-subdirectory-name
 							     file)))
 		      (vc-file-newer file)))
	     (vc-true-locking-user file)

           ;; This implementation assumes that any file which is under version
           ;; control and has -rw-r--r-- is locked by its owner.  This is true
           ;; for both RCS and SCCS, which keep unlocked files at -r--r--r--.
           ;; We have to be careful not to exclude files with execute bits on;
           ;; scripts can be under version control too.  Also, we must ignore
           ;; the group-read and other-read bits, since paranoid users turn them off.
           ;; This hack wins because calls to the very expensive vc-fetch-properties
           ;; function only have to be made if (a) the file is locked by someone
           ;; other than the current user, or (b) some untoward manipulation
           ;; behind vc's back has changed the owner or the `group' or `other'
           ;; write bits.
           (let ((attributes (file-attributes file)))
             (cond ((string-match ".r-..-..-." (nth 8 attributes))
                    nil)
                   ((and (= (nth 2 attributes) (user-uid))
			 (if (memq window-system '(win32 w32))
			     ;; NT Emacs - returns -rw-rw-rw-
			     (string-match ".rw......." (nth 8 attributes))
			   (string-match ".rw..-..-." (nth 8 attributes))
			   ))
                    (user-login-name))
                   (t
                    (vc-true-locking-user file)))) ; #### - this looks recursive!!!
           ))))

(defun vc-true-locking-user (file)
  ;; The slow but reliable version
  (vc-fetch-properties file)
  (vc-file-getprop file 'vc-locking-user))

;;;###autoload
(defun vc-latest-version (file)
  ;; Return version level of the latest version of FILE
  (vc-fetch-properties file)
  (vc-file-getprop file 'vc-latest-version))

(defun vc-your-latest-version (file)
  ;; Return version level of the latest version of FILE checked in by you
  (vc-fetch-properties file)
  (vc-file-getprop file 'vc-your-latest-version))

(defun vc-cc-previous-version (file version)
  (let ((ext-name (if version
                      (vc-cc-build-version file version)
                    file)))
    (vc-fetch-properties ext-name )
    (vc-file-getprop ext-name 'vc-pred-version)))

;; Collect back-end-dependent stuff here
;;
(defun vc-fetch-properties (file)
  ;; Re-fetch all properties associated with the given file.
  ;; Currently these properties are:
  ;;   vc-locking-user
  ;;   vc-locked-version
  ;;   vc-latest-version
  ;;   vc-your-latest-version
  ;;   vc-cvs-status (cvs only)
  ;;   vc-cc-predecessor (ClearCase only)
  (vc-backend-dispatch
    file
    ;; SCCS
    (progn
      (vc-master-info (vc-lock-file file)
                      (list
                       "^[^ ]+ [^ ]+ \\([^ ]+\\)"
                       "^\\([^ ]+\\)")
                      file
                      '(vc-locking-user vc-locked-version))
      (vc-master-info (vc-name file)
                      (list
                       "^\001d D \\([^ ]+\\)"
                       (concat "^\001d D \\([^ ]+\\) .* "
                               (regexp-quote (user-login-name)) " ")
                       )
                      file
                      '(vc-latest-version vc-your-latest-version))
      )
    ;; RCS
    (vc-log-info "rlog" file 'MASTER nil
                 (list
                  "^locks: strict\n\t\\([^:]+\\)"
                  "^locks: strict\n\t[^:]+: \\(.+\\)"
                  "^revision[\t ]+\\([0-9.]+\\).*\ndate: \\([ /0-9:]+\\);"
                  (concat
                   "^revision[\t ]+\\([0-9.]+\\)\n.*author: "
                   (regexp-quote (user-login-name))
                   ";"))
                 '(vc-locking-user vc-locked-version
                                   vc-latest-version vc-your-latest-version))
    ;; CVS
    ;; Don't fetch vc-locking-user and vc-locked-version here, since they
    ;; should always be nil anyhow.  Don't fetch vc-your-latest-version, since
    ;; that is done in vc-find-cvs-master.
    (vc-log-info
     "cvs" file 'WORKFILE '("status")
     ;; CVS 1.3 says "RCS Version:", other releases "RCS Revision:",
     ;; and CVS 1.4a1 says "Repository revision:".  The regexp below
     ;; matches much more, but because of the way vc-log-info is
     ;; implemented it is impossible to use additional groups.
     '(("\\(RCS Version\\|RCS Revision\\|Repository revision\\):[\t ]+\\([0-9.]+\\)" 2)
       "Status: \\(.*\\)")
     '(vc-latest-version
       vc-cvs-status))
    ;; CC
   (progn
     (vc-file-setprop file 'vc-need-pre-checkout-message t)
     (vc-log-info "cleartool" file nil (list "describe" file)
                  (list
                   "checked out .* by .* (\\([^ .]+\\)..*@.*)"
                   "from \\([^ ]+\\) (reserved)"
                   "version [^\"]*\".*@@\\([^ ]+\\)\""
;;                   "version [^\"]*\".*@@\\([^ ]+\\)\""
                   "predecessor version: \\([^ ]+\\)\n")
                  '(vc-locking-user
                    vc-locked-version
                    vc-latest-version
;;                    vc-your-latest-version
                    vc-cc-predecessor))
     ;; Rather than search twice, why not just set the property manually
     (vc-file-setprop file 'vc-your-latest-version
                      (vc-file-getprop file 'vc-latest-version))
     )
   ))


(defun vc-backend-subdirectory-name (&optional file)
  ;; Where the master and lock files for the current directory are kept
  (let ((backend
         (or
          (and file (vc-backend file))
          vc-default-back-end
          (setq vc-default-back-end (if (vc-find-binary "rcs") 'RCS 'SCCS)))))
    (cond
     ((eq backend 'SCCS) "SCCS")
     ((eq backend 'RCS)  "RCS")
     ((eq backend 'CVS)  "CVS")
     ((eq backend '@@)   "@@"))
    ))

(defun vc-backend-admin (file &optional writable rev workfile comment)
  ;; Register a file into the version-control system
  ;; Automatically retrieves a read-only version of the file with
  ;; keywords expanded if vc-keep-workfiles is non-nil, otherwise
  ;; it deletes the workfile.
  (vc-file-clearprops file)
  (or vc-default-back-end
      (setq vc-default-back-end (if (vc-find-binary "rcs") 'RCS 'SCCS)))
  (message "Registering %s..." file)
  (let ((backend
         (cond
          ((file-exists-p (vc-backend-subdirectory-name)) vc-default-back-end)
          ((file-exists-p "RCS") 'RCS)
          ((file-exists-p "SCCS") 'SCCS)
          ((file-exists-p "CVS") 'CVS)
          ((file-exists-p "@@") '@@)
          (t vc-default-back-end))))
    (cond ((eq backend 'SCCS)
           (vc-do-command 0 "admin" file 'MASTER ; SCCS
                          (and rev (concat "-r" rev))
                          "-fb"
                          (concat "-i" file)
                          (and comment (concat "-y" comment))
                          (format
                           (car (rassq 'SCCS vc-master-templates))
                           (or (file-name-directory file) "")
                           (file-name-nondirectory file)))
           (delete-file file)
           (if vc-keep-workfiles
               (vc-do-command 0 "get" file 'MASTER)))
          ((eq backend 'RCS)
           (vc-do-command 0 "ci" file 'MASTER ; RCS
                          (concat (if vc-keep-workfiles "-u" "-r") rev)
                          (and comment (concat "-t-" comment))
                          file))
          ((eq backend 'CVS)
           ;; #### - should maybe check to see if the master file is
           ;; already in the repository...in which case we need to add the
           ;; appropriate branch tag and do  an update.
           ;; #### - note that adding a file is a 2 step process in CVS...
           (vc-do-command 0 "cvs" file 'WORKFILE "add")
           (vc-do-command 0 "cvs" file 'WORKFILE "commit"
                          (and comment (not (string= comment ""))
                               (concat "-m" comment)))
           )
          ((eq backend '@@)
           (if vc-checkin-on-register
               (vc-do-cleartool-command "mkelem" file comment "-ci")
             (vc-do-cleartool-command "mkelem" file comment)
             ;; clearcase mkelem leaves file checked out!
             (vc-file-setprop file 'vc-checkout-time '(0 . 0))))
          (t (error "Backend %s unknown!" backend))))
  (message "Registering %s...done" file)
  ;; See if we can keep dired up-to-date
  (if (fboundp 'dired-relist-file)
      (dired-relist-file file))
  )

(defun vc-backend-checkout (file &optional writable rev workfile comment)
  ;; Retrieve a copy of a saved version into a workfile
  (let ((filename (or workfile file)))
    (message "Checking out %s..." filename)
    (save-excursion
      ;; Change buffers to get local value of vc-checkin-switches.
      (set-buffer (or (get-file-buffer file) (current-buffer)))
      (vc-backend-dispatch
        file
        ;; SCCS
        (if workfile
            ;; Some SCCS implementations allow checking out directly to a
            ;; file using the -G option, but then some don't so use the
            ;; least common denominator approach and use the -p option
            ;; ala RCS.
            (let ((vc-modes (logior (file-modes (vc-name file))
                                    (if writable 128 0)))
                  (failed t))
              (unwind-protect
                  (progn
                    (apply 'vc-do-command
                           0 "/bin/sh" file 'MASTER "-c"
                           ;; Some shells make the "" dummy argument into $0
                           ;; while others use the shell's name as $0 and
                           ;; use the "" as $1.  The if-statement
                           ;; converts the latter case to the former.
                           (format "if [ x\"$1\" = x ]; then shift; fi; \
                              umask %o; exec >\"$1\" || exit; \
                               shift; umask %o; exec get \"$@\""
                                   (logand 511 (lognot vc-modes))
                                   (logand 511 (lognot (default-file-modes))))
                           ""           ; dummy argument for shell's $0
                           filename
                           (if writable "-e")
                           "-p" (and rev
                                     (concat "-r" (vc-lookup-triple file rev)))
                           vc-checkout-switches)
                    (setq failed nil))
                (and failed (file-exists-p filename) (delete-file filename))))
          (apply 'vc-do-command 0 "get" file 'MASTER ; SCCS
                 (if writable "-e")
                 (and rev (concat "-r" (vc-lookup-triple file rev)))
                 vc-checkout-switches))
        ;; RCS
        (if workfile
            ;; RCS doesn't let us check out into arbitrary file names directly.
            ;; Use `co -p' and make stdout point to the correct file.
            (let ((vc-modes (logior (file-modes (vc-name file))
                                    (if writable 128 0)))
                  (failed t))
              (unwind-protect
                  (progn
                    (apply 'vc-do-command
                           0 "/bin/sh" file 'MASTER "-c"
                           ;; See the SCCS case, above, regarding the
                           ;; if-statement.
                           (format "if [ x\"$1\" = x ]; then shift; fi; \
                              umask %o; exec >\"$1\" || exit; \
                               shift; umask %o; exec co \"$@\""
                                   (logand 511 (lognot vc-modes))
                                   (logand 511 (lognot (default-file-modes))))
                           ""           ; dummy argument for shell's $0
                           filename
                           (if writable "-l")
                           (concat "-p" rev)
                           vc-checkout-switches)
                    (setq failed nil))
                (and failed (file-exists-p filename) (delete-file filename))))
          (apply 'vc-do-command 0 "co" file 'MASTER
                 (if writable "-l")
                 (and rev (concat "-r" rev))
                 vc-checkout-switches))
        ;; CVS
        (if workfile
            ;; CVS is much like RCS
            (let ((failed t))
              (unwind-protect
                  (progn
                    (apply 'vc-do-command
                           0 "/bin/sh" file 'WORKFILE "-c"
                           "exec >\"$1\" || exit; shift; exec cvs update \"$@\""
                           ""           ; dummy argument for shell's $0
                           workfile
                           (concat "-r" rev)
                           "-p"
                           vc-checkout-switches)
                    (setq failed nil))
                (and failed (file-exists-p filename) (delete-file filename))))
          (apply 'vc-do-command 0 "cvs" file 'WORKFILE
                 "update"
                 (and rev (concat "-r" rev))
                 file
                 vc-checkout-switches))
        ;; CC
       (if writable
            (vc-do-cleartool-command "co" file
                      comment vc-checkout-switches
                      (and rev "-branch")
                      rev))
        ))
    (or workfile
        (vc-file-setprop file
                         'vc-checkout-time (nth 5 (file-attributes file))))
    (message "Checking out %s...done" filename))
  ;; See if we can keep dired up-to-date
  (if (fboundp 'dired-relist-file)
      (dired-relist-file file))
  )


(defun vc-backend-logentry-check (file)
  (vc-backend-dispatch file
    (if (>= (buffer-size) 512)          ; SCCS
        (progn
          (goto-char 512)
          (error
           "Log must be less than 512 characters; point is now at pos 512")))
    nil                                 ; RCS
    nil                                 ; CVS
    nil)                                ; CC
  )

(defun vc-backend-checkin (file &optional writable rev workfile comment)
  ;; Register changes to FILE as level REV with explanatory COMMENT.
  ;; Automatically retrieves a read-only version of the file with
  ;; keywords expanded if vc-keep-workfiles is non-nil, otherwise
  ;; it deletes the workfile.
  (message "Checking in %s..." file)
  (save-excursion
    (setq comment (vc-cleanup-comment comment nil))
    ;; Change buffers to get local value of vc-checkin-switches.
    (set-buffer (or (get-file-buffer file) (current-buffer)))
    (vc-backend-dispatch file
      ;; SCCS
      (progn
        (apply 'vc-do-command 0 "delta" file 'MASTER
               (if rev (concat "-r" rev))
               (if comment (concat "-y" comment))
               vc-checkin-switches)
        (if vc-keep-workfiles
            (vc-do-command 0 "get" file 'MASTER))
        )
      ;; RCS
      (apply 'vc-do-command 0 "ci" file 'MASTER
             (concat (if vc-keep-workfiles "-u" "-r") rev)
             (if comment (concat "-m" comment))
             vc-checkin-switches)
      ;; CVS
      (progn
        (apply 'vc-do-command 0 "cvs" file 'WORKFILE
               "ci"
               (if comment (concat "-m" comment))
               vc-checkin-switches)
        (vc-file-setprop file 'vc-checkout-time
                         (nth 5 (file-attributes file))))
      ;; ClearCase
      (progn
        (apply 'vc-do-cleartool-command "ci" file comment
               vc-checkin-switches)
        (vc-file-setprop file 'vc-checkout-time
                         (nth 5 (file-attributes file))))
      ))
  (vc-file-setprop file 'vc-locking-user nil)
  (message "Checking in %s...done" file)
  ;; See if we can keep dired up-to-date
  (if (fboundp 'dired-relist-file)
      (dired-relist-file file))
  )

(defun vc-backend-revert (file)
  ;; Revert file to latest checked-in version.
  (message "Reverting %s..." file)
  (vc-backend-dispatch
    file
    (progn                              ; SCCS
      (vc-do-command 0 "unget" file 'MASTER nil)
      (vc-do-command 0 "get" file 'MASTER nil))
    (vc-do-command 0 "co" file 'MASTER  ; RCS.  This deletes the work file.
                   "-f" "-u")
    (progn                              ; CVS
      (delete-file file)
      (vc-do-command 0 "cvs" file 'WORKFILE "update"))
    (vc-do-cleartool-command "unco" file 'unused "-rm") ;CC
    )
  (vc-file-setprop file 'vc-locking-user nil)
  (message "Reverting %s...done" file)
  )

(defun vc-backend-steal (file &optional rev)
  ;; Steal the lock on the current workfile.  Needs RCS 5.6.2 or later for -M.
  (message "Stealing lock on %s..." file)
  (vc-backend-dispatch file
    (progn                              ; SCCS
      (vc-do-command 0 "unget" file 'MASTER "-n" (if rev (concat "-r" rev)))
      (vc-do-command 0 "get" file 'MASTER "-g" (if rev (concat "-r" rev)))
      )
    (vc-do-command 0 "rcs" file 'MASTER ; RCS
                   "-M" (concat "-u" rev) (concat "-l" rev))
    (error "You cannot steal a CVS lock; there are no CVS locks to steal.") ; CVS
    (error "VC's ClearCase support cannot steal locks.") ; CC
    )
  (vc-file-setprop file 'vc-locking-user (user-login-name))
  (message "Stealing lock on %s...done" file)
  )

(defun vc-backend-uncheck (file target)
  ;; Undo the latest checkin.  Note: this code will have to get a lot
  ;; smarter when we support multiple branches.
  (message "Removing last change from %s..." file)
  (vc-backend-dispatch file
    (vc-do-command 0 "rmdel" file 'MASTER (concat "-r" target))
    (vc-do-command 0 "rcs" file 'MASTER (concat "-o" target))
    (error "Unchecking files under CVS is dangerous and not supported in VC.")
    (let ((vc-command-messages t))
     (cond ((string-match "/CHECKEDOUT$" target)
            (vc-do-cleartool-command "unco" file 'unused "-rm"))
           ((string-match "/0$" target)
            (vc-do-cleartool-command
             "rmbranch"
             (vc-cc-build-version file
                         (substring target 0 (match-beginning 0)))
             "Removing empty branch"
             "-force"))
           (t
            (vc-do-cleartool-command "rmver"
                                     (vc-cc-build-version file target)
                                     nil "-force"))))
    )
  (message "Removing last change from %s...done" file)
  ;; See if we can keep dired up-to-date
  (if (fboundp 'dired-relist-file)
      (dired-relist-file file))
  )

(defun vc-backend-print-log (file)
  ;; Print change log associated with FILE to buffer *vc*.
  (vc-backend-dispatch
    file
    (vc-do-command 0 "prs" file 'MASTER)
    (vc-do-command 0 "rlog" file 'MASTER)
    (vc-do-command 0 "cvs" file 'WORKFILE "log")
    (vc-do-cleartool-command "lshistory" file 'unused))
  )

(defun vc-backend-define-name (file name)
  ;; Under directory FILE, allow NAME as a version name.
  (vc-backend-dispatch file
   nil
   nil
   nil
   (vc-do-cleartool-command "mklbtype" nil nil "-vob" file name)))

(defun vc-backend-assign-name (file name)
  ;; Assign to a FILE's latest version a given NAME.
  (vc-backend-dispatch file
    (vc-add-triple name file (vc-latest-version file)) ; SCCS
    (vc-do-command 0 "rcs" file 'MASTER (concat "-n" name ":")) ; RCS
    (vc-do-command 0 "cvs" file 'WORKFILE "tag" name) ; CVS
    (vc-do-cleartool-command "mklabel" file nil "-replace" name) ; CC
    )
  )

(defun vc-backend-diff (file &optional oldvers newvers cmp)
  ;; Get a difference report between two versions of FILE.
  ;; Get only a brief comparison report if CMP, a difference report otherwise.
  (let ((backend (vc-backend file)))
    (cond
     ((eq backend 'SCCS)
      (setq oldvers (vc-lookup-triple file oldvers))
      (setq newvers (vc-lookup-triple file newvers))))
    (cond
     ;; SCCS and RCS shares a lot of code.
     ((or (eq backend 'SCCS) (eq backend 'RCS))
      (let* ((command (if (eq backend 'SCCS)
                          "vcdiff"
                        "rcsdiff"))
             (mode (if (eq backend 'RCS) 'WORKFILE 'MASTER))
             (options (append (list (and cmp "--brief")
                                    "-q"
                                    (and oldvers (concat "-r" oldvers))
                                    (and newvers (concat "-r" newvers)))
                              (and (not cmp)
                                   (if (listp diff-switches)
                                       diff-switches
                                     (list diff-switches)))))
             (status (apply 'vc-do-command 2 command file mode options)))
        ;; Some RCS versions don't understand "--brief"; work around this.
        (if (eq status 2)
            (apply 'vc-do-command 1 command file 'WORKFILE
                   (if cmp (cdr options) options))
          status)))
     ;; CVS is different.
     ;; cmp is not yet implemented -- we always do a full diff.
     ((eq backend 'CVS)
      (if (string= (vc-file-getprop file 'vc-your-latest-version) "0") ; CVS
          ;; This file is added but not yet committed; there is no master file.
          ;; diff it against /dev/null.
          (if (or oldvers newvers)
              (error "No revisions of %s exists" file)
            (apply 'vc-do-command
                   1 "diff" file 'WORKFILE "/dev/null"
                   (if (listp diff-switches)
                       diff-switches
                     (list diff-switches))))
        (apply 'vc-do-command
               1 "cvs" file 'WORKFILE "diff"
               (and oldvers (concat "-r" oldvers))
               (and newvers (concat "-r" newvers))
               (if (listp diff-switches)
                   diff-switches
                 (list diff-switches)))))
     ;; ClearCase is completely different, but can also use normal diff.
     ((eq backend '@@)
      (let ((switches (if (listp diff-switches)
			  diff-switches
			(list diff-switches)))
	    command options vers1 vers2)
	(if vc-cc-use-normal-diff
	    (setq command "diff"
		  options (if cmp '("-q") switches))
	  (setq command "cleardiff"
		options (list (if cmp "-status_only"
				(if (memq "-w" switches)
				    ;; Like the "-w" flag to diff
				    "-blank_ignore"))
			      ;; Display as much as possible of the file
			      "-columns" (format "%d" (window-width)))))
	(setq vers1
	      (if oldvers
		  ;; The pseudo-version CHECKEDOUT is the working file.
		  (and (not (string-match "/CHECKEDOUT$" oldvers)) oldvers)
		(vc-file-getprop file 'vc-cc-predecessor)))
	(setq vers2
	      (if newvers
		  ;; The pseudo-version CHECKEDOUT is the working file.
		  (and (not (string-match "/CHECKEDOUT$" newvers)) newvers)))
	(apply 'vc-do-command 2 command file nil
	       (append options
		       (list (vc-cc-build-version file vers1)
			     (vc-cc-build-version file vers2))))
	))
     (t
      (vc-registration-error file)))))

(defun vc-backend-merge-news (file)
  ;; Merge in any new changes made to FILE.
  (vc-backend-dispatch
    file
    (error "vc-backend-merge-news not meaningful for SCCS files") ; SCCS
    (error "vc-backend-merge-news not meaningful for RCS files") ; RCS
    (vc-do-command 1 "cvs" file 'WORKFILE "update") ; CVS
    (error "vc-backend-merge-news not meaningful for ClearCase files") ; CC
    ))

(defun vc-backend-fetch-default-comment (file rev)
  "Fetch default comment, if any, and insert into the current buffer"
  (vc-backend-dispatch file
                       (insert vc-default-comment)      ;; SCCS
                       (insert vc-default-comment)      ;; RCS
                       (insert vc-default-comment)      ;; CVS
                       (vc-clearcase-fetch-default-comment file)))

(defun vc-cleanup-comment (string &optional from-describe)
  (if string
      (let (tmpbuf)
        (unwind-protect
            (progn
              (setq tmpbuf (get-buffer-create " *vc-cleanup*"))
              (set-buffer tmpbuf)
              (erase-buffer)
              (insert string)
              (if from-describe
                  (progn
                    (goto-char (point-min))
                    (while (re-search-forward "^   " nil t)
                      (replace-match ""))))
              (goto-char (point-min))
              (while (re-search-forward "[ \t]+$" nil t)
                (replace-match ""))
              (goto-char (point-max))
              (skip-chars-backward " \n\t")
              (delete-region (point) (point-max))
              (if (= (point) (point-min))
                  nil
                (buffer-string)))
          (if tmpbuf
              (kill-buffer tmpbuf))))))

(defun vc-clearcase-fetch-default-comment (file)
  "Fetch default comment, if any, and insert into current buffer."
  (insert
   (save-excursion
     (let ((vc-command-messages t))
       ;; Use the fmt feature to get just the comment
       (or (vc-file-getprop file 'vc-old-comment)
           (vc-file-setprop file 'vc-old-comment
                            (vc-cleanup-comment
                             (vc-cleartool-cmd (or (file-name-directory file) default-directory)
                                               (format "describe -fmt %%c %s" file)) t))
           vc-default-comment
           )))))

(defun vc-check-headers ()
  "Check if the current file has any headers in it."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (vc-backend-dispatch buffer-file-name
      (re-search-forward  "%[MIRLBSDHTEGUYFPQCZWA]%" nil t) ; SCCS
      (re-search-forward "\\$[A-Za-z\300-\326\330-\366\370-\377]+\\(: [\t -#%-\176\240-\377]*\\)?\\$" nil t) ; RCS
      'RCS ; CVS works like RCS in this regard.
      nil ; ClearCase does not recognise headers.
      )
    ))

;; Back-end-dependent stuff ends here.

;; Set up key bindings for use while editing log messages

(defun vc-log-mode ()
  "Minor mode for driving version-control tools.
These bindings are added to the global keymap when you enter this mode:
\\[vc-next-action]              perform next logical version-control operation on current file
\\[vc-register]                 register current file
\\[vc-toggle-read-only]         like next-action, but won't register files
\\[vc-insert-headers]           insert version-control headers in current file
\\[vc-print-log]                display change history of current file
\\[vc-revert-buffer]            revert buffer to latest version
\\[vc-cancel-version]           undo latest checkin
\\[vc-diff]                     show diffs between file versions
\\[vc-version-other-window]             visit old version in another window
\\[vc-directory]                show all files locked by any user in or below .
\\[vc-update-change-log]        add change log entry from recent checkins

While you are entering a change log message for a version, the following
additional bindings will be in effect.

\\[vc-finish-logentry]   proceed with check in, ending log message entry

Whenever you do a checkin, your log comment is added to a ring of
saved comments.  These can be recalled as follows:

\\[vc-next-comment]     replace region with next message in comment ring
\\[vc-previous-comment] replace region with previous message in comment ring
\\[vc-comment-search-reverse]   search backward for regexp in the comment ring
\\[vc-comment-search-forward]   search backward for regexp in the comment ring

Entry to the change-log submode calls the value of text-mode-hook, then
the value of vc-log-mode-hook.

Global user options:
 vc-initial-comment      If non-nil, require user to enter a change
                         comment upon first checkin of the file.

 vc-keep-workfiles       Non-nil value prevents workfiles from being
                         deleted when changes are checked in

 vc-suppress-confirm     Suppresses some confirmation prompts,
                         notably for reversions.

 vc-header-alist         Which keywords to insert when adding headers
                         with \\[vc-insert-headers].  Defaults to
                         '(\"\%\W\%\") under SCCS, '(\"\$Id\$\") under
                         RCS and CVS.

 vc-static-header-alist  By default, version headers inserted in C files
                         get stuffed in a static string area so that
                         ident(RCS/CVS) or what(SCCS) can see them in
                         the compiled object code.  You can override
                         this by setting this variable to nil, or change
                         the header template by changing it.

 vc-command-messages     if non-nil, display run messages from the
                         actual version-control utilities (this is
                         intended primarily for people hacking vc
                         itself).
"
  (interactive)
  (set-syntax-table text-mode-syntax-table)
  (use-local-map vc-log-entry-mode)
  (setq local-abbrev-table text-mode-abbrev-table)
  (setq major-mode 'vc-log-mode)
  (setq mode-name "VC-Log")
  (make-local-variable 'vc-log-file)
  (make-local-variable 'vc-log-version)
  (make-local-variable 'vc-comment-ring-index)
  (make-local-variable 'vc-log-writable)
  (set-buffer-modified-p nil)
  (setq buffer-file-name nil)
  (run-hooks 'text-mode-hook 'vc-log-mode-hook)
  )

;; Initialization code, to be done just once at load-time
(if vc-log-entry-mode
    nil
  (setq vc-log-entry-mode (make-sparse-keymap))
  (define-key vc-log-entry-mode "\M-n" 'vc-next-comment)
  (define-key vc-log-entry-mode "\M-p" 'vc-previous-comment)
  (define-key vc-log-entry-mode "\M-r" 'vc-comment-search-reverse)
  (define-key vc-log-entry-mode "\M-s" 'vc-comment-search-forward)
  (define-key vc-log-entry-mode "\C-c\C-c" 'vc-finish-logentry)
  (define-key vc-log-entry-mode "\C-x\C-s" 'vc-save-logentry)
  (define-key vc-log-entry-mode "\C-x\C-q" 'vc-num-num-error)
  )

;;; These things should probably be generally available

(defun vc-file-tree-walk (func &rest args)
  "Walk recursively through default directory.
Invoke FUNC f ARGS on each non-directory file f underneath it."
  (vc-file-tree-walk-internal default-directory func args nil)
  (message "Traversing directory %s...done" default-directory))

(defun vc-file-tree-walk-internal (file func args quiet)
  (if (not (file-directory-p file))
      (apply func file args)
    (or quiet
        (message "Traversing directory %s..." file))
    (let ((dir (file-name-as-directory file)))
      (mapcar
       (function
        (lambda (f) (or
                     (string-equal f ".")
                     (string-equal f "..")
                     (member f vc-directory-exclusion-list)
                     (let ((dirf (concat dir f)))
                        (or
                         (file-symlink-p dirf) ;; Avoid possible loops
                         (vc-file-tree-walk-internal dirf func args quiet))))))
       (directory-files dir)))))

(defun vc-subdir-tree-walk (func &rest args)
  "Walk recursively through default directory.
Invoke FUNC f ARGS on each subdirectory underneath it."
  (vc-subdir-tree-walk-internal default-directory func args nil)
  (message "Traversing directory %s...done" default-directory))

(defun vc-subdir-tree-walk-internal (file func args quiet)
  (if (file-directory-p file)
      (let ((dir (file-name-as-directory file)))
        (apply func dir args)
        (or quiet
            (message "Traversing directory %s..." file))
        (mapcar
         (function
          (lambda (f) (or
                       (string-equal f ".")
                       (string-equal f "..")
                       (member f vc-directory-exclusion-list)
                       (let ((dirf (concat dir f)))
                         (or
                            (file-symlink-p dirf) ;; Avoid possible loops
                            (vc-subdir-tree-walk-internal dirf func args quiet))))))
         (directory-files dir)))
    ))

(defun vc-dir-all-files (func &rest args)
  "Invoke FUNC f ARGS on each regular file f in default directory."
  (let ((dir default-directory))
    (message "Scanning directory %s..." dir)
    (mapcar (function (lambda (f)
                        (let ((dirf (expand-file-name f dir)))
                          (apply func dirf args))))
            (directory-files dir))
    (message "Scanning directory %s...done" dir)))

;;; Lucid Emacs-specific code.
(defun vc-file-newer (file)
  (or (null file)
      (let ((vc-file (vc-file-getprop file 'vc-name)))
        (or (null vc-file)
            (let ((time (vc-file-getprop file 'vc-file-timestamp)))
              (or (null time)
                  (> time (nth 5 (file-attributes vc-file)))))))))

(defun vc-revision-diff (arg)
  "Compare the version being edited with the last checked-in revision.
With a prefix argument prompt for revision to compare with."
  (interactive "P")
  (save-excursion
    (if vc-dired-mode
        (set-buffer (find-file-noselect (dired-get-filename))))
    ;; check that the file is not modified
    (if (and (buffer-modified-p)
             (or
              (y-or-n-p (format "%s has been modified. Write it out? "
                                (buffer-name)))))
        (save-buffer))
    (if arg
        (vc-version-diff buffer-file-name
                         (vc-read-version "Revision to compare against: "
                                          (buffer-file-name))
                         ())
      (vc-diff ())))
  )

(defun vc-diff-since-revision ()
  (interactive)
  (vc-revision-diff '(4)))

(defun vc-backend-checkout-in-buffer (file rev)
  ;; Retrieve a copy of a saved version into the current buffer
  (message "Checking out %s..." file)
  (vc-backend-dispatch file
   (progn
     (vc-do-command 0 "get" file 'WORKFILE       ;; SCCS
                    "-p" "-s"
                    (and rev (concat "-r" (vc-lookup-triple file rev))))
     )
   (vc-do-command 0 "co" file 'WORKFILE  ;; RCS
                  "-p" "-q"
                  (and rev (concat "-r" rev)))
   (error "Command not yet supported in CVS.")
   (error "Just use ClearCase extended naming.")
   )
  (message "Checking out %s...done" file)
  )


;;;(defun vc-visit-previous-revision (revision)
;;;  "Show a previous revision of the current file"
;;;  (interactive "sShow previous revision number: ")
;;;  (if (not buffer-file-name)
;;;      (error "There is no file associated with buffer %s" (buffer-name)))
;;;  (let* ((file buffer-file-name)
;;;      (other-file
;;;       (make-temp-name
;;;        (concat (or revision "current") "-" (file-name-nondirectory file)
;;;                "-")))
;;;      (buffer-name (concat (file-name-nondirectory file)
;;;                           "<" (vc-backend file) " " revision ">")))
;;;    (vc-backend-checkout-in-buffer file revision)
;;;    (save-excursion
;;;      (set-buffer "*vc*")
;;;      (write-region (point-min) (point-max) other-file t 0)
;;;      (erase-buffer))
;;;    (pop-to-buffer (get-buffer-create buffer-name))
;;;    (erase-buffer)
;;;    (insert-file other-file)
;;;    ;; get the same major mode as the original file
;;;    (setq buffer-file-name file)
;;;    (normal-mode)
;;;    (setq buffer-file-name ())
;;;    (set-buffer-modified-p ())
;;;    (toggle-read-only)
;;;    (delete-file other-file)))

;;;###autoload
(defun vc-rename-this-file (new)
  "Rename the file of the current buffer.
It also renames the source control archive with it"
  (interactive "FVC rename file to: ")
  (if (and (buffer-modified-p)
           (y-or-n-p (format "%s has been modified. Write it out? "
                             (buffer-name))))
      (save-buffer))
  (vc-rename-file buffer-file-name new)
  (let ((old-buffer (current-buffer))
        (new-buffer (find-file-noselect new)))
    (set-window-buffer (selected-window) new-buffer)
    (pop-to-buffer (current-buffer))
    (bury-buffer old-buffer)))

(defun vc-update-directory ()
  "Updates the current directory by getting the latest copies of the files"
  (interactive)
  (save-some-buffers)
  (let ((failed ())
        (any-updated ())
        (dir default-directory))
    (save-excursion
      (set-buffer (get-buffer-create "*vc-update*"))
      (setq default-directory dir)
      (erase-buffer)
      (insert "Updating " default-directory "\n\n")
      (display-buffer (current-buffer))
      (sit-for 0)
      (condition-case ()
          (vc-file-tree-walk
           (function (lambda (f)
                       (if (and f (vc-registered f))
                           (progn
                             (cond ((file-newer-than-file-p f (vc-name f)))
                                   ((equal (vc-locking-user f)
                                           (user-login-name)))
                                   ((not (zerop (vc-backend-diff f nil)))
                                    (vc-backend-checkout f nil nil nil nil)
                                    (insert "updated  " f "\n")
                                    (setq any-updated t)
                                    (sit-for 0))))))))
        (error (setq failed t)))
      (if (null any-updated)
          (insert "No files needed to be updated\n")
        (goto-char (point-max))
        (insert "\ndone\n")))
    (if failed
        (save-excursion
          (set-buffer "*vc*")
          (append-to-buffer "*vc-update*" (point-min) (point-max)))
        (save-excursion
          (set-buffer "*vc*")
          (goto-char (point-min))))
    (pop-to-buffer "*vc-update*")
    (if failed
        (error "update failed"))))

(provide 'vc)

;;; DEVELOPER'S NOTES ON CONCURRENCY PROBLEMS IN THIS CODE
;;;
;;; These may be useful to anyone who has to debug or extend the package.
;;;
;;; A fundamental problem in VC is that there are time windows between
;;; vc-next-action's computations of the file's version-control state and
;;; the actions that change it.  This is a window open to lossage in a
;;; multi-user environment; someone else could nip in and change the state
;;; of the master during it.
;;;
;;; The performance problem is that rlog/prs calls are very expensive; we want
;;; to avoid them as much as possible.
;;;
;;; ANALYSIS:
;;;
;;; The performance problem, it turns out, simplifies in practice to the
;;; problem of making vc-locking-user fast.  The two other functions that call
;;; prs/rlog will not be so commonly used that the slowdown is a problem; one
;;; makes snapshots, the other deletes the calling user's last change in the
;;; master.
;;;
;;; The race condition implies that we have to either (a) lock the master
;;; during the entire execution of vc-next-action, or (b) detect and
;;; recover from errors resulting from dispatch on an out-of-date state.
;;;
;;; Alternative (a) appears to be unfeasible.  The problem is that we can't
;;; guarantee that the lock will ever be removed.  Suppose a user starts a
;;; checkin, the change message buffer pops up, and the user, having wandered
;;; off to do something else, simply forgets about it?
;;;
;;; Alternative (b), on the other hand, works well with a cheap way to speed up
;;; vc-locking-user.  Usually, if a file is registered, we can read its locked/
;;; unlocked state and its current owner from its permissions.
;;;
;;; This shortcut will fail if someone has manually changed the workfile's
;;; permissions; also if developers are munging the workfile in several
;;; directories, with symlinks to a master (in this latter case, the
;;; permissions shortcut will fail to detect a lock asserted from another
;;; directory).
;;;
;;; Note that these cases correspond exactly to the errors which could happen
;;; because of a competing checkin/checkout race in between two instances of
;;; vc-next-action.
;;;
;;; For VC's purposes, a workfile/master pair may have the following states:
;;;
;;; A. Unregistered.  There is a workfile, there is no master.
;;;
;;; B. Registered and not locked by anyone.
;;;
;;; C. Locked by calling user and unchanged.
;;;
;;; D. Locked by the calling user and changed.
;;;
;;; E. Locked by someone other than the calling user.
;;;
;;; This makes for 25 states and 20 error conditions.  Here's the matrix:
;;;
;;; VC's idea of state
;;;  |
;;;  V  Actual state   RCS action              SCCS action          Effect
;;;    A  B  C  D  E
;;;  A .  1  2  3  4   ci -u -t-          admin -fb -i<file>      initial admin
;;;  B 5  .  6  7  8   co -l              get -e                  checkout
;;;  C 9  10 .  11 12  co -u              unget; get              revert
;;;  D 13 14 15 .  16  ci -u -m<comment>  delta -y<comment>; get  checkin
;;;  E 17 18 19 20 .   rcs -u -M ; rcs -l unget -n ; get -g       steal lock
;;;
;;; All commands take the master file name as a last argument (not shown).
;;;
;;; In the discussion below, a "self-race" is a pathological situation in
;;; which VC operations are being attempted simultaneously by two or more
;;; Emacsen running under the same username.
;;;
;;; The vc-next-action code has the following windows:
;;;
;;; Window P:
;;;    Between the check for existence of a master file and the call to
;;; admin/checkin in vc-buffer-admin (apparent state A).  This window may
;;; never close if the initial-comment feature is on.
;;;
;;; Window Q:
;;;    Between the call to vc-workfile-unchanged-p in and the immediately
;;; following revert (apparent state C).
;;;
;;; Window R:
;;;    Between the call to vc-workfile-unchanged-p in and the following
;;; checkin (apparent state D).  This window may never close.
;;;
;;; Window S:
;;;    Between the unlock and the immediately following checkout during a
;;; revert operation (apparent state C).  Included in window Q.
;;;
;;; Window T:
;;;    Between vc-locking-user and the following checkout (apparent state B).
;;;
;;; Window U:
;;;    Between vc-locking-user and the following revert (apparent state C).
;;; Includes windows Q and S.
;;;
;;; Window V:
;;;    Between vc-locking-user and the following checkin (apparent state
;;; D).  This window may never be closed if the user fails to complete the
;;; checkin message.  Includes window R.
;;;
;;; Window W:
;;;    Between vc-locking-user and the following steal-lock (apparent
;;; state E).  This window may never close if the user fails to complete
;;; the steal-lock message.  Includes window X.
;;;
;;; Window X:
;;;    Between the unlock and the immediately following re-lock during a
;;; steal-lock operation (apparent state E).  This window may never cloce
;;; if the user fails to complete the steal-lock message.
;;;
;;; Errors:
;;;
;;; Apparent state A ---
;;;
;;; 1. File looked unregistered but is actually registered and not locked.
;;;
;;;    Potential cause: someone else's admin during window P, with
;;; caller's admin happening before their checkout.
;;;
;;;    RCS: ci will fail with a "no lock set by <user>" message.
;;;    SCCS: admin will fail with error (ad19).
;;;
;;;    We can let these errors be passed up to the user.
;;;
;;; 2. File looked unregistered but is actually locked by caller, unchanged.
;;;
;;;    Potential cause: self-race during window P.
;;;
;;;    RCS: will revert the file to the last saved version and unlock it.
;;;    SCCS: will fail with error (ad19).
;;;
;;;    Either of these consequences is acceptable.
;;;
;;; 3. File looked unregistered but is actually locked by caller, changed.
;;;
;;;    Potential cause: self-race during window P.
;;;
;;;    RCS: will register the caller's workfile as a delta with a
;;; null change comment (the -t- switch will be ignored).
;;;    SCCS: will fail with error (ad19).
;;;
;;; 4. File looked unregistered but is locked by someone else.
;;;
;;;    Potential cause: someone else's admin during window P, with
;;; caller's admin happening *after* their checkout.
;;;
;;;    RCS: will fail with a "no lock set by <user>" message.
;;;    SCCS: will fail with error (ad19).
;;;
;;;    We can let these errors be passed up to the user.
;;;
;;; Apparent state B ---
;;;
;;; 5. File looked registered and not locked, but is actually unregistered.
;;;
;;;    Potential cause: master file got nuked during window P.
;;;
;;;    RCS: will fail with "RCS/<file>: No such file or directory"
;;;    SCCS: will fail with error ut4.
;;;
;;;    We can let these errors be passed up to the user.
;;;
;;; 6. File looked registered and not locked, but is actually locked by the
;;; calling user and unchanged.
;;;
;;;    Potential cause: self-race during window T.
;;;
;;;    RCS: in the same directory as the previous workfile, co -l will fail
;;; with "co error: writable foo exists; checkout aborted".  In any other
;;; directory, checkout will succeed.
;;;    SCCS: will fail with ge17.
;;;
;;;    Either of these consequences is acceptable.
;;;
;;; 7. File looked registered and not locked, but is actually locked by the
;;; calling user and changed.
;;;
;;;    As case 6.
;;;
;;; 8. File looked registered and not locked, but is actually locked by another
;;; user.
;;;
;;;    Potential cause: someone else checks it out during window T.
;;;
;;;    RCS: co error: revision 1.3 already locked by <user>
;;;    SCCS: fails with ge4 (in directory) or ut7 (outside it).
;;;
;;;    We can let these errors be passed up to the user.
;;;
;;; Apparent state C ---
;;;
;;; 9. File looks locked by calling user and unchanged, but is unregistered.
;;;
;;;    As case 5.
;;;
;;; 10. File looks locked by calling user and unchanged, but is actually not
;;; locked.
;;;
;;;    Potential cause: a self-race in window U, or by the revert's
;;; landing during window X of some other user's steal-lock or window S
;;; of another user's revert.
;;;
;;;    RCS: succeeds, refreshing the file from the identical version in
;;; the master.
;;;    SCCS: fails with error ut4 (p file nonexistent).
;;;
;;;    Either of these consequences is acceptable.
;;;
;;; 11. File is locked by calling user.  It looks unchanged, but is actually
;;; changed.
;;;
;;;    Potential cause: the file would have to be touched by a self-race
;;; during window Q.
;;;
;;;    The revert will succeed, removing whatever changes came with
;;; the touch.  It is theoretically possible that work could be lost.
;;;
;;; 12. File looks like it's locked by the calling user and unchanged, but
;;; it's actually locked by someone else.
;;;
;;;    Potential cause: a steal-lock in window V.
;;;
;;;    RCS: co error: revision <rev> locked by <user>; use co -r or rcs -u
;;;    SCCS: fails with error un2
;;;
;;;    We can pass these errors up to the user.
;;;
;;; Apparent state D ---
;;;
;;; 13. File looks like it's locked by the calling user and changed, but it's
;;; actually unregistered.
;;;
;;;    Potential cause: master file got nuked during window P.
;;;
;;;    RCS: Checks in the user's version as an initial delta.
;;;    SCCS: will fail with error ut4.
;;;
;;;    This case is kind of nasty.  It means VC may fail to detect the
;;; loss of previous version information.
;;;
;;; 14. File looks like it's locked by the calling user and changed, but it's
;;; actually unlocked.
;;;
;;;    Potential cause: self-race in window V, or the checkin happening
;;; during the window X of someone else's steal-lock or window S of
;;; someone else's revert.
;;;
;;;    RCS: ci will fail with "no lock set by <user>".
;;;    SCCS: delta will fail with error ut4.
;;;
;;; 15. File looks like it's locked by the calling user and changed, but it's
;;; actually locked by the calling user and unchanged.
;;;
;;;    Potential cause: another self-race --- a whole checkin/checkout
;;; sequence by the calling user would have to land in window R.
;;;
;;;    SCCS: checks in a redundant delta and leaves the file unlocked as usual.
;;;    RCS: reverts to the file state as of the second user's checkin, leaving
;;; the file unlocked.
;;;
;;;    It is theoretically possible that work could be lost under RCS.
;;;
;;; 16. File looks like it's locked by the calling user and changed, but it's
;;; actually locked by a different user.
;;;
;;;    RCS: ci error: no lock set by <user>
;;;    SCCS: unget will fail with error un2
;;;
;;;    We can pass these errors up to the user.
;;;
;;; Apparent state E ---
;;;
;;; 17. File looks like it's locked by some other user, but it's actually
;;; unregistered.
;;;
;;;    As case 13.
;;;
;;; 18. File looks like it's locked by some other user, but it's actually
;;; unlocked.
;;;
;;;    Potential cause: someone released a lock during window W.
;;;
;;;    RCS: The calling user will get the lock on the file.
;;;    SCCS: unget -n will fail with cm4.
;;;
;;;    Either of these consequences will be OK.
;;;
;;; 19. File looks like it's locked by some other user, but it's actually
;;; locked by the calling user and unchanged.
;;;
;;;    Potential cause: the other user relinquishing a lock followed by
;;; a self-race, both in window W.
;;;
;;;     Under both RCS and SCCS, both unlock and lock will succeed, making
;;; the sequence a no-op.
;;;
;;; 20. File looks like it's locked by some other user, but it's actually
;;; locked by the calling user and changed.
;;;
;;;     As case 19.
;;;
;;; PROBLEM CASES:
;;;
;;;    In order of decreasing severity:
;;;
;;;    Cases 11 and 15 under RCS are the only one that potentially lose work.
;;; They would require a self-race for this to happen.
;;;
;;;    Case 13 in RCS loses information about previous deltas, retaining
;;; only the information in the current workfile.  This can only happen
;;; if the master file gets nuked in window P.
;;;
;;;    Case 3 in RCS and case 15 under SCCS insert a redundant delta with
;;; no change comment in the master.  This would require a self-race in
;;; window P or R respectively.
;;;
;;;    Cases 2, 10, 19 and 20 do extra work, but make no changes.
;;;
;;;    Unfortunately, it appears to me that no recovery is possible in these
;;; cases.  They don't yield error messages, so there's no way to tell that
;;; a race condition has occurred.
;;;
;;;    All other cases don't change either the workfile or the master, and
;;; trigger command errors which the user will see.
;;;
;;;    Thus, there is no explicit recovery code.

;;; ClearCase extensions:

;;; vc-edit-config

(defvar vc-config-edit-mode nil)

(defvar vc-tag-name nil
  "Name of view tag which is currently being edited")

(if vc-config-edit-mode
    nil
  (setq vc-config-edit-mode (make-sparse-keymap))
  (define-key vc-config-edit-mode "\C-c\C-c" 'vc-finish-config)
  (define-key vc-config-edit-mode "\C-x\C-s" 'vc-save-config))

;; NT Emacs - use environment variable TEMP if it exists.
(defsubst vc-temp-filename ()
  (make-temp-name (concat (or (getenv "TEMP") "/tmp") "/VC-")))

(defun vc-save-config ()
  (interactive)
  (if (not (buffer-modified-p))
      (message "Configuration not changed since last saved")
    (let ((tmp (vc-temp-filename)))
      (unwind-protect
          (progn
            (message "Setting configuration for %s..." vc-tag-name)
            (write-region (point-min) (point-max) tmp nil 'dont-mention-it)
            (let ((vc-command-messages t))
              (vc-cleartool-cmd default-directory
                                (format "setcs -tag %s %s" vc-tag-name tmp)))
            (set-buffer-modified-p nil)
            (message "Setting configuration for %s...done" vc-tag-name))
        (if (file-exists-p tmp)
            (delete-file tmp))))))

(defun vc-finish-config ()
  (interactive)
  (let ((old-buffer (current-buffer)))
    (vc-save-config)
    (bury-buffer nil)
;;    (if (bufferp vc-parent-buffer)
;;      (pop-to-buffer vc-parent-buffer))
    (kill-buffer old-buffer)))

(defun vc-config-edit-mode ()
  (interactive)
  (set-syntax-table text-mode-syntax-table)
  (use-local-map vc-config-edit-mode)
  (setq major-mode 'vc-config-edit-mode)
  (setq mode-name "VC-Config")
  (make-variable-buffer-local 'vc-parent-buffer)
  (set-buffer-modified-p nil)
  (setq buffer-file-name nil)
  (run-hooks 'text-mode-hook 'vc-config-edit-mode-hook))

(defun vc-cc-tagname (str)
  "Extract the view tag name from a view-extended-pathname"
  (and str
       (stringp str)
       (string-match "^/view/\\([^/]+\\)" str)
       (substring str
                  (match-beginning 1)
                  (match-end 1))))

;;;###autoload
(defun vc-cc-pwv (path)
  (or (vc-cc-tagname path)
      (vc-cc-tagname (getenv "CLEARCASE_ROOT"))))

(defvar vc-tag-history ()
  "History of view tags used in vc-edit-config")

;;;###autoload
(defun vc-edit-config (tag-name)
  "Edit a ClearCase configuration specification"
  (interactive
   (let ((vxname (vc-cc-pwv default-directory)))
     (list (directory-file-name
            (completing-read "View Tag: "
                            (vc-cc-get-view-cache-noslash)
                            nil
                            'fascist
                            vxname
                            'vc-tag-history)))))
  (let ((start (current-buffer))
        (buffer-name (format "*VC-Config-%s*" tag-name)))
    (kill-buffer (get-buffer-create buffer-name))
    (pop-to-buffer (get-buffer-create buffer-name))
    (auto-save-mode auto-save-default)
    (erase-buffer)
    (insert (vc-cleartool-cmd default-directory
                              (format "catcs -tag %s" tag-name)))
    (goto-char (point-min))
    (re-search-forward "^[^#\n]" nil 'end)
    (beginning-of-line)
    (vc-config-edit-mode)
    (setq vc-parent-buffer start)
    (make-local-variable 'vc-tag-name)
    (setq vc-tag-name tag-name))
  )

(defun vc-nosupport (which op)
  (error (format "%s does not support %s" which op)))

;;;###autoload
(defun vc-graph-history (arg)
  "Display a graph of a file's history"
  (interactive "p")
  (let ((pathname (cond (vc-dired-mode (dired-get-filename))
                        (buffer-file-name)
                        (default-directory)))
        (vtreearg (cond ((eq arg 1) nil)
                        ((eq arg 4) "-all ")
                        ((eq arg 16) "-nmerge ")))
        (debug-on-error t))
    (vc-backend-dispatch
     pathname
     (vc-nosupport 'SCCS 'graph-history)
     (vc-nosupport 'RCS 'graph-history)
     (vc-nosupport 'CVS 'graph-history)
     (progn
       (if vc-alternate-lsvtree
           (start-process "lsvtree" (get-buffer-create "*lsvtree*")
                          vc-alternate-lsvtree pathname)
         (vc-cleartool-cmd pathname
                           (concat "xlsvtree " vtreearg pathname)))
       (message "xlsvtree window for %s should appear shortly" pathname)))))

(defvar cleartool-tq nil
  "Transaction queue to talk to ClearTool in a subprocess")

(defvar vc-return nil
  "Return value when we're involved in a blocking call")

(defvar cleartool-view ""
  "Current view of cleartool subprocess, or the empty string if none")

(defvar cleartool-wdir ""
  "Current working directory of cleartool subprocess, or the empty string if none")

(defvar vc-running nil)

(defconst vc-subproc-timeout 30
  "Timeout on calls to subprocess")

(defun vc-accept-process-output (proc timeout)
  (if vc-elucidated
      (accept-process-output proc)
    (accept-process-output proc timeout)))

(defun vc-start-cleartool ()
  (interactive)
  (let ((process-environment (append '("ATRIA_NO_BOLD=1"
                                        "ATRIA_FORCE_GUI=1")
                                     ;;; emacs is a GUI, right? :-)
                                     process-environment)))
    (let ((cleartool-process (start-process "cleartool" "*cleartool*" vc-cleartool-path)))
      (process-kill-without-query cleartool-process)
      (setq cleartool-view "")
      (setq cleartool-tq (tq-create cleartool-process))
      (tq-enqueue cleartool-tq "" "[.\n]*cleartool> " 'vc-running 'set)
      (while (not vc-running)
        (message "waiting for cleartool to start...")
        (vc-accept-process-output (tq-process cleartool-tq) vc-subproc-timeout))
      (message "waiting for cleartool to start...done"))))

(defun vc-kill-cleartool ()
  "Kill off cleartool subprocess.  If another one is needed,
it will be restarted.  This may be useful if you're debugging clearcase."
  (interactive)
  (vc-kill-tq))

(defun vc-callback (arg val)
  (setq vc-return (substring val 0 -11)))

;;(defun vc-foo (cmd)
;;  (interactive)
;;  (tq-enqueue cleartool-tq cmd "[.\n]*\ncleartool> " nil 'vc-callback)
;;  )

(defvar vc-cc-uninteresting-commands "\\(describe\\|lsview\\)")

(defun vc-do-cleartool-command (command file comment &rest flags)
  "Execute a cleartool version-control command, notifying user and checking for errors.
Output from COMMAND goes to buffer *vc*.  The last argument of the command is
the master name of FILE; this is appended to an optional list of FLAGS."
  (if file
      (setq file (expand-file-name file)))
  (if (listp command)
;;      (progn
;;      (setq flags (append (cdr command) flags))
;;      (setq command (car command)))
      (error "command must not be a list"))
  (if vc-command-messages
      (message "Running %s on %s..." command file))
  (let ((obuf (current-buffer)) (camefrom (current-buffer))
        (squeezed nil)
        (vc-file (and file (or (vc-name file) file)))
        status)
    (set-buffer (get-buffer-create "*vc*"))
    (set (make-local-variable 'vc-parent-buffer) camefrom)
    (set (make-local-variable 'vc-parent-buffer-name)
         (concat " from " (buffer-name camefrom)))
    (erase-buffer)
    ;; This is so that command arguments typed in the *vc* buffer will
    ;; have reasonable defaults.
    (if file
        (setq default-directory (file-name-directory file)))

    (mapcar
     (function (lambda (s)
                 (and s
                      (setq squeezed
                            (concat squeezed " " s)))))
     flags)
    (let ((tmpfile (vc-temp-filename)))
      (unwind-protect
          (progn
            (if (not (eq comment 'unused))
                (if comment
                    (progn
                      (write-region comment nil tmpfile nil 'noprint)
                      (setq squeezed (concat squeezed " -cfile " tmpfile)))
                  (setq squeezed (concat squeezed " -nc"))))
            (if vc-file
                (setq squeezed (concat squeezed " " vc-file)))
            (let ((default-directory (file-name-directory
                                      (or file default-directory))))
              (vc-cc-cd default-directory)
              (if vc-command-messages
                  (message "Running %s %s..." command squeezed))
              (insert
               (vc-cleartool-cmd file (concat command " " squeezed)))
              (if vc-command-messages
                  (message "Running %s %s...done" command squeezed))))
        (if (file-exists-p tmpfile)
            (delete-file tmpfile))))
    (goto-char (point-min))
    (if (re-search-forward "^cleartool: Error:.*$" nil t)
        (progn
          (setq status (buffer-substring (match-beginning 0) (match-end 0)))
          (pop-to-buffer "*vc*")
          (goto-char (point-min))
          (shrink-window-if-larger-than-buffer)
          (error "Running %s...FAILED (%s)" squeezed status))
      (if vc-command-messages
          (message "Running %s...OK" command)))
    (set-buffer obuf)
    status))

(defun vc-cc-cd (dir)
  (if (or (not dir)
          (string= dir cleartool-wdir))
      cleartool-wdir
    (vc-setview (vc-cc-pwv dir))
    (let ((ret (vc-cleartool-blocking-call (format "cd %s" dir))))
      (if (string-match "cleartool: Error:" ret)
          (error (substring ret (match-end 0)))
        (setq cleartool-wdir dir)))))

(defun vc-setview (view)
  "Set the cleartool subprocess to a named view"
  (if (or (not view)
          (string= view cleartool-view))
      cleartool-view
    (let ((ret (vc-cleartool-blocking-call (format "setview %s" view))))
      (if (string-match "cleartool: Error:" ret)
          (error (substring ret (match-end 0)))
        (setq cleartool-view view)))))

(defun vc-cleartool-cmd (path cmd)
  (vc-setview (vc-cc-pwv path))
  (vc-cleartool-blocking-call cmd))

(defun vc-cleartool-blocking-call (cmd)
  (interactive)
  (save-excursion
    (setq vc-return nil)
    ;; NT Emacs - doesn't use tq.
    (if (memq window-system '(win32 w32))
 	(setq vc-return (vc-get-command-stdout "cleartool" (concat cmd "\n")))
      (setq vc-return nil)
      (if (not cleartool-tq)
 	  (vc-start-cleartool))
      (unwind-protect
 	  (progn
 	    (tq-enqueue cleartool-tq (concat cmd "\n") "[.\n]*cleartool> " nil 'vc-callback)
 	    (while (not vc-return)
 	      (vc-accept-process-output (tq-process cleartool-tq) vc-subproc-timeout)))
 	(while (tq-queue cleartool-tq)
 	  (tq-queue-pop cleartool-tq)))))
  vc-return)

(defun vc-kill-tq ()
  (process-send-eof (tq-process cleartool-tq))
  (kill-process (tq-process cleartool-tq))
  (setq vc-running nil)
  (setq cleartool-tq nil))

(defun vc-what-the-hey ()
  ;; NT Emacs - doesn't use tq.
  (if (not (memq window-system '(win32 w32)))
      (let ((kill-buffer-hook nil))
	(if (eq (current-buffer) (tq-buffer cleartool-tq))
	    (error "Don't kill TQ buffer %s, use `vc-kill-tq'" (current-buffer)))
 	)))

(add-hook 'kill-buffer-hook 'vc-what-the-hey)

(defun vc-split-string (pat str &optional indir suffix)
  (let ((ret nil)
        (start 0)
        (last (length str)))
    (while (< start last)
      (if (string-match pat str start)
          (progn
            (let ((tmp (substring str start (match-beginning 0))))
              (if suffix (setq tmp (concat tmp suffix)))
              (setq ret (cons (if indir (cons tmp nil)
                                tmp)
                          ret)))
            (setq start (match-end 0)))
        (setq start last)
        (setq ret (cons (substring str start) ret))))
    (nreverse ret)))

;;;###autoload
(defun vc-mkbrtype (typename comment)
  (interactive "sBranch type name: \nsBranch type comment: ")
  (message "%s" (vc-cleartool-cmd default-directory
                                  (format "mkbrtype -vob %s -c '%s' %s"
                                          default-directory comment typename))))

;;; warn when editing files which *should* be under change
;;; control, but aren't.
;;;

(defvar vc-controlled-trees ()
  "*A list of regexps; if any of these match buffer-file name, and
the file in question is not under version control, an error will be signalled")

(defun vc-edit-warn ()
  (let ((need-work
         (and buffer-file-name
              buffer-file-number
              (not vc-mode)
              (let ((paths vc-controlled-trees))
                (catch 'break
                  (while paths
                    (if (string-match (car paths) buffer-file-name)
                        (throw 'break t))
                    (setq paths (cdr paths))))))))
    (if need-work
        (progn
          (vc-register)
          (if buffer-read-only
              (vc-toggle-read-only))))))

(add-hook 'first-change-hook 'vc-edit-warn)

;;; completions, view autostart in /view given view registry

(defvar vc-cc-known-view-cache nil
  "Cache of views known to the local system.")

(defvar vc-cc-view-cache-timeout 300
  "*Default timeout of view list cache, in seconds.")

(defun vc-timeout (delay function arg)
  (if (fboundp 'start-itimer)
      (start-itimer "VC-CC-Cache" function delay)
    (if (fboundp 'run-at-time)
        (run-at-time (format "%s sec" delay) nil function arg)
      (error "vc-timeout: don't know how to set a timeout function"))))

(defun vc-cc-known-views  ()
  (message "Fetching view names...")
  (let ((raw-views (vc-cleartool-blocking-call "lsview -short")))
    (prog1
        (vc-split-string "\n *" raw-views t "/")
      (message "Fetching view names...done"))))

(defun vc-cc-get-view-cache ()
  (or vc-cc-known-view-cache
      (progn
        (let ((default-directory "/"))
          (vc-timeout vc-cc-view-cache-timeout
                       (function (lambda (&rest ignore)
                                   (setq vc-cc-known-view-cache nil)))
                       nil))
        (setq vc-cc-known-view-cache (vc-cc-known-views)))))

(defun vc-cc-get-view-cache-noslash ()
  (let ((cache (vc-cc-get-view-cache)))
    (mapcar (lambda (vn)
              (let ((tmp (car vn)))
                (cons (substring tmp 0 (- (length tmp) 1))
                      (cdr vn))))
            cache)))

(defun vc-cc-start-view (view-tag)
  (let ((tagfile (file-name-as-directory view-tag)))
    (if (assoc tagfile (vc-cc-get-view-cache))
        (let ((tagname (directory-file-name view-tag)))
          (message "Starting view server for %s..." tagname)
          (vc-cleartool-blocking-call (format "startview %s"
                                    (directory-file-name tagname)))
          (message "Starting view server for %s...done" tagname)))))

(defun vc-syslog (buf value)
  (save-excursion
    (let ((tmpbuf (get-buffer buf)))
      (if (bufferp tmpbuf)
          (progn
            (set-buffer buf)
            (goto-char (point-max))
            (insert (format "%s\n" value)))))))

(defun vc-cc-dedouble (pathname)
  (let ((root (file-name-as-directory (or (getenv "CLEARCASE_ROOT") ""))))
    (if (stringp pathname)
        (cond ((string-match "^/view/[^/]+\\(/view/\\)" pathname)
               (vc-cc-dedouble (substring pathname (match-beginning 1))))
              ((string= root "/")
               pathname)
              ((and (< (length root) (length pathname))
                    (string= root (substring pathname 0 (length root))))
               (vc-cc-dedouble (substring pathname (- (length root) 1))))
              (t
               pathname)))))

(defun vc-view-completion (file dir)
  (try-completion file (vc-cc-get-view-cache)))

(defun vc-view-completions (file dir)
  (all-completions file (vc-cc-get-view-cache)))

;; vc-cc-what

;;;###autoload
(defun vc-cc-what-rule (file)
  (interactive (list (cond (vc-dired-mode (dired-get-filename))
                           (buffer-file-name)
                           (default-directory))))

  ;; File should not have a version extension, it will not have a rule!
  (setq file (vc-cc-element-name file))

  (let ((result (vc-cleartool-cmd file (format "ls -d %s" file))))
    (if (string-match "Rule: \\(.*\\)\n" result)
        (message (substring result
                            ;; Be a little more verbose
                            (match-beginning 0) (match-end 1)))
      (error result)
      )))

;;(defun vc-cc-enumerate-versions (file)
;;  "Produce an alist, suitable for the completion functions, of the
;;versions of the element named by FILE"
;;  (message "Finding versions of %s..." file)
;;  (let ((versions nil))
;;    (vc-file-tree-walk-internal
;;     (vc-cc-build-version file "")
;;     (function (lambda (f &rest args)
;;               (if (not (string-match "/CHECKEDOUT[^/]*$" f))
;;                   (setq versions
;;                         (cons (list (vc-cc-version-name f))
;;                               versions)))))
;;     nil
;;     t)
;;    (message "Finding versions of %s...done" file)
;;    (nreverse versions)))

(defun vc-cc-enumerate-versions (file)
  "Produce an alist, suitable for the completion functions, of the
versions of the element named by FILE"
  (message "Finding versions of %s..." file)
  (let ((raw-info (vc-cleartool-cmd
                   default-directory
                   (format "lsvtree -all -nco -short %s" file))))
    (prog1
        (mapcar (function (lambda (x)
                            (list (vc-cc-version-name x))))
                (vc-split-string "\n" raw-info))
      (message "Finding versions of %s...done" file))))

(defsubst vc-buffer-match (tag)
  (buffer-substring (match-beginning tag)
                    (match-end tag)))

(defun vc-extract-environ-from-view-1 (tag)
  (let ((tmp-buffer (generate-new-buffer " *env temp*"))
        ret)
    (unwind-protect
        (save-excursion
          (set-buffer tmp-buffer)
          (insert (vc-cleartool-blocking-call (format "catcs -tag %s" tag)))
          (goto-char (point-min))
          (keep-lines "%ENV%")
          (goto-char (point-min))
          (while (re-search-forward "^.*%ENV%[ \t]\\(.*\\)=\\(.*\\)$" nil t)
            (setq ret (cons (format "%s=%s"
                                    (vc-buffer-match 1)
                                    (vc-buffer-match 2))
                            ret)))
          ret)
      (kill-buffer tmp-buffer))))

(defun vc-add-env (env var)
  (catch 'return
    (let ((a env)
          (vname (substring var 0
                            (and (string-match "=" var)
                                 (match-end 0)))))
      (let ((vnl (length vname)))
        (while a
          (if (and (> (length (car a)) vnl)
                   (string= (substring (car a) 0 vnl)
                            vname))
              (throw 'return env))
          (setq a (cdr a)))
        (cons var env)))))

(defun vc-extract-environ-from-view (old-env tag &optional add-ons)
  (let ((newenv nil)
        (cc-env (vc-extract-environ-from-view-1 tag)))
    (while add-ons
      (setq newenv (vc-add-env newenv (car add-ons)))
      (setq add-ons (cdr add-ons)))
    (while cc-env
      (setq newenv (vc-add-env newenv (car cc-env)))
      (setq cc-env (cdr cc-env)))
    (while old-env
      (setq newenv (vc-add-env newenv (car old-env)))
      (setq old-env (cdr old-env)))
    newenv))

(defsubst vc-setview-arglist (dir args)
  (let ((r (concat (if dir (format "cd %s; " dir) "")
                   "exec "
                   (mapconcat 'identity args " "))))
    (insert r "\n")
    r))

(if (not (fboundp 'dired-get-filename))
(defun dired-get-filename (&optional localp no-error-if-not-filep)
  "In dired, return name of file mentioned on this line.
Value returned normally includes the directory name.
Optional arg LOCALP with value `no-dir' means don't include directory
  name in result.  A value of t means construct name relative to
  `default-directory', which still may contain slashes if in a subdirectory.
Optional arg NO-ERROR-IF-NOT-FILEP means return nil if no filename on
  this line, otherwise an error occurs."
  (let (case-fold-search file p1 p2)
    (save-excursion
      (if (setq p1 (dired-move-to-filename (not no-error-if-not-filep)))
          (setq p2 (dired-move-to-end-of-filename no-error-if-not-filep))))
    ;; nil if no file on this line, but no-error-if-not-filep is t:
    (if (setq file (and p1 p2 (format "%s" (buffer-substring p1 p2))))
        ;; Check if ls quoted the names, and unquote them.
        ;; Using read to unquote is much faster than substituting
        ;; \007 (4 chars) -> ^G  (1 char) etc. in a lisp loop.
        (cond ((string-match "b" dired-actual-switches) ; System V ls
               ;; This case is about 20% slower than without -b.
               (setq file
                     (read
                      (concat "\""
                              ;; some ls -b don't escape quotes, argh!
                              ;; This is not needed for GNU ls, though.
                              (or (dired-string-replace-match
                                   "\\([^\\]\\)\"" file "\\1\\\\\"")
                                  file)
                              "\""))))
              ;; If you do this, update dired-insert-subdir-validate too
              ;; ((string-match "Q" dired-actual-switches) ; GNU ls
              ;;  (setq file (read file)))
              ))
    (if (or (eq localp 'no-dir) (null file) (eq (aref file 0) ?/))
        file
      (concat (dired-current-directory localp) file)))))

;;;###autoload
(defun vc-cc-browse-versions ()
  (interactive)
  (let ((file (cond (vc-dired-mode (dired-get-filename))
                    (buffer-file-name)
                    (default-directory))))
    (if (clearcase-element-p file)
        (if (boundp 'vc-mode)
            (let* ((clearcase-extended-name
                    (concat (vc-cc-element-name file)
                            "@@"
                            (or (vc-cc-version-name file)
                                (vc-latest-version file))))
                   (dired-listing-switches (concat dired-listing-switches
                                                   "tr"))
;;                   (file-name-handler-alist nil) ;; shut off my special handler
                   (clearcase-branch (file-name-directory clearcase-extended-name)))
              (if (file-exists-p clearcase-extended-name)
                  (progn
                    ;; Invoke dired on the directory of the version branch
                    (dired clearcase-branch)

                    (if (re-search-forward (concat "[ \t]+"
                                                   "\\("
                                                   (regexp-quote clearcase-branch)
                                                   "\\)"
                                                   "$")
                                       nil
                                       t)
                        (goto-char (match-beginning 1))))
                (dired (concat file "@@")))))
      (error "Not a Clearcase element"))))

;;----------------------------------------------------------------------------
;; Advise various other functions to properly handle ClearCase extended paths
;;----------------------------------------------------------------------------
(defadvice gud-find-file (before vc-gud-find-file protect activate)
  "Sets the current view and comint-file-name-prefix if necessary for ClearCase support."
  (let ((tag (and (string-match "^\\(/view/[^/]+\\)" default-directory)
		  (substring default-directory
			     (match-beginning 1)
			     (match-end 1)))))
    (and tag
	 (ad-set-arg 0 (concat tag (ad-get-arg 0))))))

(defadvice comint-exec-1 (around vc-comint-exec-1 protect activate)
  "Sets the current view and comint-file-name-prefix if necessary for ClearCase support."
  (let ((tag (vc-cc-pwv default-directory))
	(view-rel (vc-cc-relpath default-directory))
	(wdir-in-clearcase (file-directory-p ".@@/main")))
    (if (or (not (or (string-match "^/view/" default-directory)
		     wdir-in-clearcase))
	    (string= tag (vc-cc-pwv (getenv "CLEARCASE_ROOT"))))
	ad-do-it
      (let ((process-environment
	     ;; If using termcap, we specify `emacs' as the terminal type
	     ;; because that lets us specify a width.
	     ;; If using terminfo, we specify `unknown' because that is
	     ;; a defined terminal type.  `emacs' is not a defined terminal
	     ;; type and there is no way for us to define it here.
	     ;; Some programs that use terminfo get very confused
	     ;; if TERM is not a valid terminal type.
	     (vc-extract-environ-from-view process-environment tag
					   (if (and (boundp 'system-uses-terminfo)
                                                    (symbol-value 'system-uses-terminfo))
					       (list "EMACS=t" "TERM=unknown"
						     (format "COLUMNS=%d" (frame-width)))
					     (list "EMACS=t" "TERM=emacs"
						   (format "TERMCAP=emacs:co#%d:tc=unknown" (frame-width)))))))
	(insert "setview " tag "\n")
	(make-variable-buffer-local 'comint-file-name-prefix)
	(setq comint-file-name-prefix (format "/view/%s" tag))
	(setq ad-return-value
	      (start-process name buffer vc-cleartool-path
			     "setview" "-exec"

		     (vc-setview-arglist view-rel (cons command switches))
			     tag))))))

(defadvice start-process-shell-command (around vc-spsc protect activate)
  "Sets the current view and comint-file-name-prefix if necessary for ClearCase support."
  (let ((tag (vc-cc-pwv default-directory))
	(view-rel (vc-cc-relpath default-directory))
	(wdir-in-clearcase (file-directory-p ".@@/main")))
    (if (null wdir-in-clearcase)
	ad-do-it
      (let ((process-environment (vc-extract-environ-from-view process-environment tag)))
	(if (string= tag (vc-cc-pwv (getenv "CLEARCASE_ROOT")))
	    ad-do-it
	  (progn
	    (insert "setview " tag "\n")
	    (make-variable-buffer-local 'comint-file-name-prefix)
	    (setq comint-file-name-prefix (format "/view/%s" tag))

	    (setq ad-return-value
		  (start-process name buffer vc-cleartool-path
				 "setview" "-exec"
				 (vc-setview-arglist view-rel args)
				 tag))))))))

;;; Hook into jka-compr so it knows a little about clearcase
;;; version-extended names..

(defvar vc-jka-compr-hook-installed nil
  "Is hook for vc installed in jka-compr?")

(if (and (not vc-jka-compr-hook-installed)
         (fboundp 'jka-compr-installed-p)
         (jka-compr-installed-p))
    (progn
      (jka-compr-uninstall)
      (setq jka-compr-compression-info-list
            (append '(["\\.gz\\(@@.*\\)\\'"
                       "zipping" "gzip" ("-c" "-q")
                       "unzipping" "gzip" ("-c" "-q" "-d") nil t])
                    jka-compr-compression-info-list))
      (setq auto-mode-alist
            (append '(("\\.gz\\(~\\|\\.~[0-9]+~\\)?\\'" nil jka-compr))
                    auto-mode-alist))
      (setq vc-jka-compr-hook-installed t)
      (jka-compr-install)))

;;; vc.el ends here
