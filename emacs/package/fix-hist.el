;;; fix-hist.el --- Fixes behaviour of minibuffer history

;; Copyright (C) 1997 Martin Ebourne, martin@galaxy.tcp.co.uk
;; All rights reserved
;; 07-02-97 - First version
;;
;; $Id: fix-hist.el 792 2003-09-22 11:47:18Z martin $

;;; Commentary:

;; Load this and from then on the minibuffer history will behave sensibly. ie. As Zsh behaves
;; when up/down are mapped to history-beginning-search. Specifically, up and down will always
;; find the next history line that matches everything to the left of the cursor. Down will also
;; return you to the original line you started with if appropriate. Entries are only ever
;; allowed once into the history - older duplicates are removed.

;;; Bugs:

;; If you move off an edited history line (or even the original line edited AFTER the history
;; has been invoked), the changes will be lost. This is not how Zsh behaves.

;;; Code:


;; Internal variables

(defvar minibuffer-temporary-entry nil)


;; Internal functions

(defadvice previous-complete-history-element (before save-current-line activate preactivate)
  "Store current input line when first called, so it can be returned to."
  (interactive "p")

  ;; If first call this time then insert buffer contents onto start of history, pretend
  ;; we were already at that entry, and note the entry so we can remove it later
  (if (zerop minibuffer-history-position)
      (progn
	(setq minibuffer-history-position 1)
	(setq minibuffer-temporary-entry (buffer-string))
	(set minibuffer-history-variable (cons minibuffer-temporary-entry
					       (symbol-value minibuffer-history-variable)))
	))
  )

(defun minibuffer-tidy-history ()
  "Tidy history up to remove any temporary entries, and ensure no entry is present twice."

  ;; If we have used the history then remove the temporary element we added
  (if (not (zerop minibuffer-history-position))
      (set minibuffer-history-variable
	   (delq minibuffer-temporary-entry (symbol-value minibuffer-history-variable))))

  ;; Remove duplicates of the new entry
  (setcdr (symbol-value minibuffer-history-variable)
	  (delete (car (symbol-value minibuffer-history-variable))
		  (cdr (symbol-value minibuffer-history-variable))))
  )

;; Re-bind the keys
(define-key minibuffer-local-map [up] 'previous-complete-history-element)
(define-key minibuffer-local-map [down] 'next-complete-history-element)
(define-key minibuffer-local-ns-map [up] 'previous-complete-history-element)
(define-key minibuffer-local-ns-map [down] 'next-complete-history-element)
(define-key minibuffer-local-completion-map [up] 'previous-complete-history-element)
(define-key minibuffer-local-completion-map [down] 'next-complete-history-element)
(define-key minibuffer-local-must-match-map [up] 'previous-complete-history-element)
(define-key minibuffer-local-must-match-map [down] 'next-complete-history-element)

;; Hook so we can tidy the history up
(add-hook 'minibuffer-exit-hook 'minibuffer-tidy-history)

(provide 'fix-hist)
