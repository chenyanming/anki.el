;;; anki/anki-card.el -*- lexical-binding: t; -*-

(require 'anki-core)

(defcustom anki-show-unique-buffers nil
  "TODO: When non-nil, every entry buffer gets a unique name.
This allows for displaying multiple show buffers at the same
time."
  :group 'anki
  :type 'boolean)

(define-derived-mode anki-show-mode fundamental-mode "anki-show"
  "Mode for displaying book entry details.
\\{anki-show-mode-map}"
  (setq buffer-read-only t)
  (buffer-disable-undo))

(defun anki-show--buffer-name (entry)
  "Return the appropriate buffer name for ENTRY.
The result depends on the value of `anki-show-unique-buffers'."
  (if anki-show-unique-buffers
      (format "*anki-entry-<%s>*"
              (gethash 'id entry))
    "*anki-entry*"))

(define-derived-mode anki-show-mode fundamental-mode "anki-show"
  "Mode for displaying book entry details.
\\{anki-show-mode-map}"
  (setq buffer-read-only t)
  (buffer-disable-undo))

(defcustom anki-show-entry-switch #'switch-to-buffer-other-window
  "Function used to display the calibre entry buffer."
  :group 'anki
  :type '(choice (function-item switch-to-buffer-other-window)
                 (function-item switch-to-buffer)
                 (function-item pop-to-buffer)
                 function))

(defun anki-show-entry (entry &optional switch)
  "Display ENTRY in the current buffer.
Optional argument SWITCH to switch to *anki-search* buffer to other window."
  (unless (eq major-mode 'anki-show-mode)
    (when (get-buffer (anki-show--buffer-name entry))
      (kill-buffer (anki-show--buffer-name entry))))
  (let* ((buff (get-buffer-create (anki-show--buffer-name entry)))
         (id (gethash 'id entry))       ; card id
         (flds (gethash 'flds entry))   ; note fields
         (original (point))
         (file-map (make-sparse-keymap))
         beg end)
    (let ((inhibit-read-only t) c-beg c-end)
      (with-current-buffer buff
        (define-key file-map [mouse-1] 'anki-file-mouse-1)
        (define-key file-map [mouse-3] 'anki-file-mouse-3)
        (erase-buffer)
        (setq beg (point))
        (insert id)
        (insert "\n")
        (setq end (point))
        (put-text-property beg end 'anki-entry entry)

        (setq c-beg (point))
        (dolist ( field (split-string flds "\037") )
          (insert field)
          (insert "<br><br>"))
        (setq c-end (point))

        (shr-render-region c-beg c-end)
        (insert "\n")
        ;; (setq end (point))
        (anki-show-mode)
        (setq anki-show-entry entry)
        (goto-char (point-min))))
    (unless (eq major-mode 'anki-show-mode)
      (funcall anki-show-entry-switch buff)
      (when switch
        (switch-to-buffer-other-window (set-buffer (anki-search--buffer-name)))
        (goto-char original)))))


(defun anki-entry-quit ()
  "Quit the *anki-entry*."
  (interactive)
  (when (eq major-mode 'anki-show-mode)
    (if (get-buffer "*anki-entry*")
        (kill-buffer "*anki-entry*"))))

(defun anki-preview-card ()
  (interactive)
  (anki-show-entry (caar (anki-find-card-at-point) )))

(provide 'anki-card)
