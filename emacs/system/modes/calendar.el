;; Emacs configuration file
;; Setup for calendar/diary etc
;; Written by Martin Ebourne
;; $Id: calendar.el,v 1.1 2001/05/11 17:31:38 mebourne Exp $

(setq appt-message-warning-time 0)
(setq appt-issue-message t)
(setq european-calendar-style t)
(add-hook 'diary-hook 'appt-make-list)
(add-hook 'diary-display-hook 'fancy-diary-display)
(setq calendar-time-display-form '(24-hours ":" minutes))
(setq calendar-longtitude -0.08)
(setq calendar-latitude 51.52)
(setq calendar-location-name "City, London")
