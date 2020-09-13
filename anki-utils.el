;;; anki/anki-utils.el -*- lexical-binding: t; -*-

(require 'anki-core)

(defun anki-decode-milliseconds (input)
  (if input
      (format-time-string "<%Y-%m-%d %a %H:%M:%S>"
                          (seconds-to-time
                           (/ (string-to-number input) 1000))) ))

(defun anki-decode-seconds (input)
  (if input
      (format-time-string "<%Y-%m-%d %a %H:%M:%S>"
                          (seconds-to-time (string-to-number input))) ))




(defun anki-find-card-at-point ()
  "Find card at point and return the list."
  (interactive)
  (if (eq major-mode 'anki-search-mode)
      (list (cdr (or (get-text-property (point) 'anki-entry nil)
                     (get-text-property (point) 'anki-detail nil)
                     (get-text-property (point) 'anki-compact nil))))
    (list (get-text-property (point-min) 'anki-entry nil) )))

(provide 'anki-utils)
