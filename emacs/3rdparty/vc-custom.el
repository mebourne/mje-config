;;; vc-custom.el --- custom support for vc package

;; Copyright (C) 1997 Free Software Foundation, Inc.

;; Author: Chris Felaco <felaco@ziplink.net>
;; Maintainer: Rod Whitby <rwhitby@geocities.com>
;; Keywords: custom vc

;; This file is not part of XEmacs.

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

;; This is a quick first attempt at adding custom support for the VC package.
;; Most variables are simple boolean switches.

;;; To Do:

;; - Find a better place to put the vc group
;; - Rename to vc-vars.el or something since it isn't necessarily for the
;;   custom package.

;;; Code:

(or (featurep 'custom)
    (load "custom" t)
    (progn
(defmacro defgroup (&rest args) "do nothing" nil)
(defmacro defcustom (var value doc &rest args) "just defvar"
  (list 'defvar var value doc))
(defmacro custom-declare-variable (symbol value doc &rest args)
  (list 'defvar (eval symbol) value doc))
))

(defgroup vc () "Version Control Options" :group 'tools :prefix "vc")

;; Vars already declared in vc-hooks do not need documentation
(custom-declare-variable 
 'vc-path 
 (if (file-exists-p "/usr/sccs") '("/usr/sccs") nil)
 nil
 :group 'vc
 :type '(repeat directory))

(defcustom vc-make-backup-files nil nil
  :group 'vc
  :type 'boolean)

(defcustom vc-follow-symlinks 'ask nil
  :group 'vc
  :type '(radio (const :tag "Never follow symlinks" nil) 
                (const :tag "Automatically follow symlinks" t) 
                (const :tag "Prompt before following symlinks" ask)))

(defcustom vc-display-status t nil
  :group 'vc
  :type 'boolean)

(defcustom vc-cc-display-branch t nil
  :group 'vc
  :type 'boolean)

(defcustom vc-auto-dired-mode t nil
  :group 'vc
  :type 'boolean)

(defcustom vc-dired-highlight t
  "If non-nil, highlight registered and reserved files in vc-dired buffers."
  :group 'vc
  :type 'boolean)

(defcustom vc-checkout-dir-on-register 'ask
  "*If t, automatically checkout the directory (if needed) when registering files.
If nil, don't checkout the directory and cancel the registration.
If `ask', prompt before checking out the directory.

This only applies to version control systems with versioned directories (namely
ClearCase."
  :group 'vc
  :type '(radio (const :tag "Never checkout dir on register" nil) 
                (const :tag "Automatically checkout dir on register" t) 
                (const :tag "Prompt to checkout dir on register" ask)))

(defcustom vc-alternate-lsvtree nil
  "Use an alternate external program instead of xlsvtree"
  :group 'vc
  :type '(radio (const :tag "Use default" nil)
                (string :tag "Command")))

(defcustom vc-diff-on-checkin nil
  "Display diff on checkin to help you compose the checkin comment."
  :group 'vc
  :type 'boolean)

;; General customization

(defcustom vc-default-back-end nil
  "Back-end actually used by this interface; may be SCCS or RCS.
The value is only computed when needed to avoid an expensive search."
  :group 'vc
  :type '(radio (const :tag "No default" nil)
                (const SCCS) (const RCS) (const CVS) 
                (const :tag "ClearCase" @@)))

(defcustom vc-suppress-confirm nil
  "If non-nil, treat user as expert; suppress yes-no prompts on some things."
  :group 'vc
  :type 'boolean)

(defcustom vc-keep-workfiles t
  "If non-nil, don't delete working files after registering changes.
If the back-end is CVS, workfiles are always kept, regardless of the
value of this flag."
  :group 'vc
  :type 'boolean)

(defcustom vc-initial-comment nil
  "Prompt for initial comment when a file is registered."
  :group 'vc
  :type 'boolean)

(defcustom vc-command-messages nil
  "Display run messages from back-end commands."
  :group 'vc
  :type 'boolean)

(defcustom vc-mistrust-permissions 'file-symlink-p
  "Don't assume that permissions and ownership track version-control status."
  :group 'vc
  :type '(radio (const :tag "Trust permissions" nil)
                (symbol :tag "Function")))

(defcustom vc-checkin-switches nil
  "Extra switches passed to the checkin program by \\[vc-checkin]."
  :group 'vc
  :type '(radio (const :tag "No extra switches" nil) 
                (string :tag "Switches")))

(defcustom vc-default-comment "[no seeded comment]"
  "Default comment for when no checkout comment is available, or
for those version control systems which don't support checkout comments."
  :group 'vc
  :type 'string)

(defcustom vc-checkin-on-register nil
  "If t, file will be checked-in when first registered.
This only applies to version control systems that by defaule leave files 
checked-out when first registered."
  :group 'vc
  :type 'boolean)

(defcustom vc-suppress-checkout-comments nil
  "Suppress prompts for checkout comments for those version control
systems which use them."
  :group 'vc
  :type 'boolean)

(defcustom vc-checkout-switches nil
  "Extra switches passed to the checkout program by \\[vc-checkout]."
  :group 'vc
  :type '(radio (const :tag "No extra switches" nil)
                (string :tag "Switches")))

(defcustom vc-directory-exclusion-list '("SCCS" "RCS" "lost+found")
  "Directory names ignored by functions that recursively walk file trees."
  :group 'vc
  :type '(repeat (string :tag "Subdirectory")))

(defcustom vc-checkin-hook nil
  "List of functions called after a checkin is done.  See `run-hooks'."
  :group 'vc
  :type 'hook)

(defcustom vc-before-checkin-hook nil
  "List of functions called before a checkin is done.  See `run-hooks'."
  :group 'vc
  :type 'hook)

(defcustom vc-register-hook nil
  "List of functions called after a register is done.  See `run-hooks'."
  :group 'vc
  :type '(repeat (symbol :tag "Function")))

(defcustom vc-before-register-hook nil
  "List of functions called before a register is done.  See `run-hooks'."
  :group 'vc
  :type '(repeat (symbol :tag "Function")))

(defcustom vc-make-buffer-writable-hook nil
  "List of functions called when a buffer is made writable.  See `run-hooks.'
This hook is only used when the version control system is CVS.  It
might be useful for sites who uses locking with CVS, or who uses link
farms to gold trees."
  :group 'vc
  :type 'hook)

;; Default is to be extra careful for super-user. 
(custom-declare-variable
 'vc-checkout-carefully
 (= (user-uid) 0)
 "Non-nil means be extra-careful in checkout.
Verify that the file really is not locked
and that its contents match what the master file says."
 :group 'vc
 :type 'boolean)

;; Header insertion stuff

(defcustom vc-header-alist '((SCCS "\%W\%") (RCS "\$Id\$") (CVS "\$Id\$"))
  "Header keywords to be inserted when `vc-insert-headers' is executed."
  :group 'vc
  :type '(repeat (group 
                  (radio :tag "VC Type" 
                          (const SCCS) (const RCS) (const CVS) 
                          (const :tag "ClearCase" @@))
                  (string :tag "Header String"))))

(defcustom vc-static-header-alist
  '(("\\.c$" .
    "\n#ifndef lint\nstatic char vcid[] = \"\%s\";\n#endif /* lint */\n"))
  "Associate static header string templates with file types.  A \%s in the
template is replaced with the first string associated with the file's
version-control type in `vc-header-alist'."
  :group 'vc
  :type '(repeat (cons (string :tag "File Pattern") 
                       (string :tag "Header String"))))

(defcustom vc-comment-alist
  '((nroff-mode ".\\\"" ""))
  "Special comment delimiters to be used in generating vc headers only.
Add an entry in this list if you need to override the normal comment-start
and comment-end variables.  This will only be necessary if the mode language
is sensitive to blank lines."
  :group 'vc
  :type '(repeat (group 
                  (symbol :tag "Mode")
                  (string :tag "Comment Start")
                  (string :tag "Comment End"))))

(defcustom vc-cc-use-normal-diff nil
  "If non-nil, use normal diff instead of cleardiff."
  :group 'vc
  :type 'boolean)

(defcustom vc-cleartool-path "/usr/atria/bin/cleartool"
  "Path to ClearCase cleartool"
  :group 'vc
  :type 'file)

;; Add items to Version Control menu for convenience and publicity
(defconst vc-custom-menu
  '(["Customize..." (customize 'vc) t])
  "Menubar entries to add to the VC menu when Customize is available.")

(provide 'vc-custom)
