;;; anki/anki-card.el -*- lexical-binding: t; -*-

(require 'anki-core)

(defcustom anki-show-unique-buffers nil
  "TODO: When non-nil, every entry buffer gets a unique name.
This allows for displaying multiple show buffers at the same
time."
  :group 'anki
  :type 'boolean)

(defcustom anki-audio-player (or (executable-find "aplay")
                                         (executable-find "afplay"))
  "Music player used to play sounds."
  :group 'anki
  :type 'string)

(defvar anki-card-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" #'anki-card-quit)
    (define-key map "r" #'anki-replay-audio)
    map)
  "Keymap for `anki-card-mode'.")

(defvar anki-shr-map
  (let ((map (copy-keymap anki-card-mode-map)))
    (set-keymap-parent map shr-map)
    map))

(define-derived-mode anki-card-mode fundamental-mode "anki-card"
  "Mode for displaying book entry details.
\\{anki-card-mode-map}"
  (setq buffer-read-only t)
  (buffer-disable-undo))

(defun anki-show--buffer-name (entry)
  "Return the appropriate buffer name for ENTRY.
The result depends on the value of `anki-show-unique-buffers'."
  (if anki-show-unique-buffers
      (format "*anki-card-<%s>*"
              (gethash 'id entry))
    "*anki-card*"))

(define-derived-mode anki-card-mode fundamental-mode "anki-card"
  "Mode for displaying book entry details.
\\{anki-card-mode-map}"
  (setq buffer-read-only t)
  (buffer-disable-undo))

(defcustom anki-show-card-switch #'switch-to-buffer-other-window
  "Function used to display the calibre entry buffer."
  :group 'anki
  :type '(choice (function-item switch-to-buffer-other-window)
                 (function-item switch-to-buffer)
                 (function-item pop-to-buffer)
                 function))

(defun anki-show-card (entry &optional switch)
  "Display ENTRY in the current buffer.
Optional argument SWITCH to switch to *anki-search* buffer to other window."
  (unless (eq major-mode 'anki-card-mode)
    (when (get-buffer (anki-show--buffer-name entry))
      (kill-buffer (anki-show--buffer-name entry))))
  (let* ((buff (get-buffer-create (anki-show--buffer-name entry)))
         (id (gethash 'id entry))       ; card id
         (flds (gethash 'flds entry))   ; note fields
         (model (gethash 'mid entry)) ; model names
         (model-names (anki-models-names model))
         (original (point))
         (file-map (make-sparse-keymap))
         beg end)
    (let ((inhibit-read-only t) c-beg c-end)
      (with-current-buffer buff
        (define-key file-map [mouse-1] 'anki-file-mouse-1)
        (define-key file-map [mouse-3] 'anki-file-mouse-3)
        (erase-buffer)
        (setq beg (point))
        ;; (insert id)
        (insert "\n")
        (setq end (point))
        (put-text-property beg end 'anki-card entry)

        (setq c-beg (point))
        (dolist (field (mapcar* 'cons model-names (split-string flds "\037")))
          (let* ((mf (car field))
                 (cf (cdr field)) img-src)
            (insert (format "<h1>%s</h1><br><br>" mf))
            (insert (format "%s<br><br>" cf))
            (insert "<br><br>")))
        (setq c-end (point))

        (goto-char (point-min))
        (while (re-search-forward "src=\"\\(.*?\\)\"" nil t)
          (replace-match (format "src=\"%s%s\"" (concat (file-name-as-directory anki-collection-dir) "collection.media/") (match-string 1))))

        (goto-char (point-min))
        (while (re-search-forward "\\[sound:\\(.*?\\)\\]" nil t)
          (replace-match (format "<a href=\"%s%s\">Play</a>" (concat (file-name-as-directory anki-collection-dir) "collection.media/") (match-string 1))))


        (insert "\n")
        ;; (setq end (point))
        (anki-card-mode)
        ;; (shr-render-region (point-min) (point-max))
        (anki-render-html)
        (setq anki-show-card entry)
        (goto-char (point-min))))
    (unless (eq major-mode 'anki-card-mode)
      (funcall anki-show-card-switch buff)
      (when switch
        (switch-to-buffer-other-window (set-buffer (anki-search--buffer-name)))
        (goto-char original)))))

(defun anki-render-img (dom &optional url)
  "Custom <img> rendering function for DOM.
Uses `shr-tag-img' for external paths and `anki-insert-image' for
internal ones."
  (let ((url (or url (cdr (assq 'src (cadr dom)))))
        (alt (or (cdr (assq 'alt (cadr dom))) "")))
    (if (anki-external-url-p url)
        ;; HACK: avoid hanging in an infinite loop when using
        ;; `cl-letf' to override `shr-tag-img' with a function that
        ;; might call `shr-tag-img' again
        (funcall anki-original-shr-tag-img-function dom url)
      (setq url (expand-file-name (anki-urldecode url)))
      (anki-insert-image url alt))))

(defun anki-insert-image (path alt)
  "Insert an image for PATH at point, falling back to ALT.
This function honors `shr-max-image-proportion' if possible."
  (let ((type (if (or (and (fboundp 'image-transforms-p) (image-transforms-p))
                      (not (fboundp 'imagemagick-types)))
                  nil
                'imagemagick)))
    (if (not (display-graphic-p))
        (insert alt)
      (-let* (((x1 y1 x2 y2) (window-inside-pixel-edges
                              (get-buffer-window (current-buffer))))
              (image
               ;; `create-image' errors out for unsupported image types
               (ignore-errors
                 (create-image path type nil
                               :ascent 100
                               :max-width (truncate (* shr-max-image-proportion
                                                       (- x2 x1)))
                               :max-height (truncate (* shr-max-image-proportion
                                                        (- y2 y1)))))))
        (if image
            (insert-image image)
          (insert alt))))))

(defun anki-external-url-p (url)
  "Return t if URL refers to an external document."
  (and (url-type (url-generic-parse-url url)) t))

(defun anki-urldecode (string)
  "Return urldecoded version of STRING or nil."
  (when string
    (url-unhex-string string)))

(defvar anki-shr-rendering-functions
  '(;; default function uses url-retrieve and fails on local images
    (img . anki-render-img)
    ;; titles are rendered *inside* the document by default
    )
  "Alist of rendering functions used with `shr-render-region'.")

(defun anki-render-html ()
  "Render HTML in current buffer with shr."
  (run-hooks 'anki-pre-html-render-hook)
  (let (;; HACK: make buttons use our own commands
        (shr-map anki-shr-map)
        (shr-external-rendering-functions anki-shr-rendering-functions)
        (shr-use-fonts anki-variable-pitch))
    ;; HACK: `shr-external-rendering-functions' doesn't cover
    ;; every usage of `shr-tag-img'
    (cl-letf (((symbol-function 'shr-tag-img) 'anki-render-img))
      (if (eq anki-text-width t)
          (cl-letf (((symbol-function 'shr-fill-line) 'ignore))
            (shr-render-region (point-min) (point-max)))
        (let ((shr-width anki-text-width))
          (shr-render-region (point-min) (point-max))))))
  (run-hooks 'anki-post-html-render-hook))

(defcustom anki-variable-pitch t
  "Non-nil if a variable pitch face should be used.
Otherwise the default face is used."
  :type 'boolean
  :group 'anki)

(defcustom anki-text-width nil
  "Width filled text shall occupy.
An integer is interpreted as the number of columns.  If nil, use
the full window's width.  If t, disable filling completely.  Note
that this variable only has an effect in Emacs 25.1 or greater."
  :type '(choice (integer :tag "Fixed width in characters")
                 (const   :tag "Use the width of the window" nil)
                 (const   :tag "Disable filling" t))
  :group 'anki)

(defcustom anki-pre-html-render-hook nil
  "Hook run before `anki-render-html'."
  :type 'hook
  :group 'anki)

(defcustom anki-post-html-render-hook nil
  "Hook run after `anki-render-html'."
  :type 'hook
  :group 'anki)

(defun anki-card-quit ()
  "Quit the *anki-card*."
  (interactive)
  (when (eq major-mode 'anki-card-mode)
    (if (get-buffer "*anki-card*")
        (kill-buffer "*anki-card*"))))

(defun anki-preview-card ()
  (interactive)
  (anki-show-card (anki-find-card-at-point) ))

(defun anki-models-names (model)
  (cl-loop for name in (gethash "flds" model) collect
           (gethash "name" name)))

(defun anki-replay-audio (&optional external)
  "Open a visible link in an `eww-mode' buffer.
If EXTERNAL is single prefix, browse the URL using
`browse-url-secondary-browser-function'.
If EXTERNAL is double prefix, browse in new buffer."
  (interactive "P")
  (if (process-live-p (get-process "anki-audio-player"))
      (kill-process (get-process "anki-audio-player")))
   (let ((sound (nth 1 (car (anki-shr-audio-collect)))))
     (message (mapconcat 'identity
                         `(,anki-audio-player
                           ,@(delq nil (list (shell-quote-argument (expand-file-name sound)))))
                         " ") )
     (if (and anki-audio-player sound)
         (start-process-shell-command
          "anki-audio-player" nil
          (mapconcat 'identity
                     `(,anki-audio-player
                       ,@(delq nil (list (shell-quote-argument (expand-file-name sound)))))
                     " ")))) )

(defun anki-shr-audio-collect ()
  "Collect the positions of visible links in the current `anki-card' buffer."
  (save-excursion
    (let (beg end buf string url collected-list)
      (setq buf (current-buffer))
      (setq end
            (if (get-text-property (point) 'shr-url)
                (point)
              (text-property-any
               (point) (point-max) 'shr-url nil)))
      (while (setq beg (text-property-not-all
                        end (point-max) 'shr-url nil))
        (goto-char beg)
        (setq url (get-text-property beg 'shr-url))
        (setq beg (point))
        ;; Extract the current point text properties if it matched by giving
        ;; property `face', and insert it to `buf-name'
        (if (get-text-property (point) 'shr-url)
            (progn
              (setq end (next-single-property-change (point) 'shr-url nil (point-max)))
              ;; When link at the end of buffer, end will be set to nil.
              (if (not end)
                  (setq end (point-max)))

              (setq string (buffer-substring-no-properties beg end)) ; save the url title
              ;; only collect media files
              (if (string-match-p "\\.\\(mp3\\|wav\\|m4a\\|mp4\\)$" url)
                  (push (list string url beg end) collected-list)))))
      collected-list)))

(provide 'anki-card)
