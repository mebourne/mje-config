;; Focus windows on click

(define (focus-window-and-pass-through-click w)
  "Give input focus to the window, then pass the invoking pointer event
to the application."
  (if (equal w 'root) 
    (set-input-focus nil)
    (set-input-focus w))
  (allow-events 'replay-pointer)
  (unless (clicked-frame-part)
	(forget-button-press)))

(define (raise-and-focus-window w)
  "Raise the window and give it the input focus."
  (set-input-focus w)
  (raise-window w))

(define-command 'focus-window-and-pass-through-click
  focus-window-and-pass-through-click #:spec "%W")
(define-command 'raise-and-focus-window
  raise-and-focus-window #:spec "%W")

(provide 'click-focus)
