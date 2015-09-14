;; Emacs configuration file
;; Key bindings
;; Written by Martin Ebourne

;; Cursor movement
(global-set-key "\M-\C-z"     'scroll-other-window-down)
(global-set-key "\M-g"        'goto-line)
(global-set-key [home]        'beginning-of-buffer)
(global-set-key [end]         'end-of-buffer)

;; Text editing
(global-set-key "\C-m"        'newline-and-indent)
(global-set-key "\C-xw"       'copy-word-as-kill)
(global-set-key "\M-s"        'swap-case)
(global-set-key [C-backspace] 'kill-current-line)
(global-set-key "\M-p"        'call-last-kbd-macro)
(global-set-key "\M-["        'toggle-paren)
(global-set-key "\C-ca"       'align-regexp)
(global-set-key "\C-cA"       'align-columns)
(global-set-key "\C-ce"       'eliminate-trailing-spaces)
(global-set-key "\C-co"       'ff-find-other-file)
(global-set-key "\C-cs"       'sort-lines)
(global-set-key "\C-cy"       'yank-with-replace)

;; Searching
(global-set-key "\C-x%"       'query-replace-regexp)
(global-set-key "\C-x\M-%"    'tags-query-replace)
(global-set-key "\C-xt"       'tags-search)

;; Applications
(global-set-key "\C-xve"      'vc-ediff)
(global-set-key "\C-xv!"      'vc-version-diff)
(global-set-key "\C-xvz"      'cvs-update)
(global-set-key "\M-\C-x"     'compile)
(global-set-key "\C-x:"       'ielm)
;(global-set-key "\M-n"        'gnus)
(global-set-key "\M-#"        'calc)

;; Buffer operations
(global-set-key "\M-o"        'eval-current-buffer)
(global-set-key "\C-x\C-q"    'toggle-read-only)
(global-set-key [67108897]    'kill-this-buffer)      ; C-!
(global-set-key "\C-x\C-r"    'find-file-root)
(global-set-key [C-home]      'previous-buffer)
(global-set-key [C-end]       'next-buffer)
(global-set-key [M-up]        '(lambda () (interactive) (other-window -1)))
(global-set-key [M-down]      'other-window)
(global-set-key "\C-x4s"      'transpose-windows)
(global-set-key "\C-xc"       '(lambda () (interactive)
				 (switch-to-buffer-other-window "*scratch*")))
(global-set-key [up]          '(lambda () (interactive)
				 (let ((line-move-visual t))
				   (previous-line))))
(global-set-key [down]        '(lambda () (interactive)
                                 (let ((line-move-visual t))
                                   (next-line))))
(global-set-key "\C-cr"       'rename-file-and-buffer)
(global-set-key "\C-ct"       'toggle-truncate-lines)

;; Rectangle operations
(global-set-key "\C-xrw"      'copy-rectangle)
(global-set-key "\C-xr\\"     'close-left-rectangle)
(global-set-key "\C-xr/"      'close-rectangle)

;; Set up repeat key. Should only be on windowing version
(global-set-key "\C-z"        'repeat)
(setq repeat-on-final-keystroke "z")

;; Version control
(eval-after-load 'vc
  (define-key vc-prefix-map "i" '(lambda ()
                                   ;; Fix \Cxvi to work for git - ie. stage changes
				   (interactive)
				   (if (not (eq 'Git (vc-backend buffer-file-name)))
				       (vc-register)
				     (vc-git-register (list buffer-file-name))
				     (message "Staged changes.")))))


;; Minibuffer keys
(define-key minibuffer-local-must-match-map [M-tab]
  'file-cache-minibuffer-complete-on-filename)
(define-key minibuffer-local-must-match-map [M-return]
  'file-cache-minibuffer-complete-on-filename-and-exit)
(define-key minibuffer-local-must-match-map [C-return]
  'file-cache-minibuffer-complete-and-exit)
