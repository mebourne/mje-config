;; Emacs configuration file
;; Setup for hs
;; Written by Martin Ebourne

;;; (load-library "hideshow")

(defun hs-setup ()
  "Enables hideshow and binds some commands"
  (hs-minor-mode 1)
  (local-set-key "\C-ch" 'hs-hide-block)
  (local-set-key "\C-cs" 'hs-show-block)
  (local-set-key "\C-cH" 'hs-hide-all)
  (local-set-key "\C-cS" 'hs-show-all)
  (local-set-key "\C-cR" 'hs-show-region))
