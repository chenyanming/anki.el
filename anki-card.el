;;; anki-card.el -*- lexical-binding: t; -*-

;; Author: Damon Chan <elecming@gmail.com>

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'anki-core)

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

(defcustom anki-show-unique-buffers nil
  "TODO: When non-nil, every entry buffer gets a unique name.
This allows for displaying multiple show buffers at the same
time."
  :group 'anki
  :type 'boolean)

(defcustom anki-card-mode-parent-mode 'fundamental-mode
  "The Parent mode of Anki card Mode.
Support: `org-mode', `fundamental-mode', `shrface-mode'."
  :group 'anki
  :type 'symbol)

(defcustom anki-audio-player (or (executable-find "aplay")
                                         (executable-find "afplay"))
  "Music player used to play sounds."
  :group 'anki
  :type 'string)


(defcustom anki-pandoc-sleep-time 0.2
  "When testing Pandoc the first time it's used in a session, wait this long for Pandoc to start.
Normally this should not need to be changed, but if Pandoc takes
unusually long to start on your system (which it seems to on
FreeBSD, for some reason), you may need to increase this."
  :type 'float)

(defconst anki--pandoc-no-wrap-option nil
  "Option to pass to Pandoc to disable wrapping.
Pandoc >= 1.16 deprecates `--no-wrap' in favor of
`--wrap=none'.")

(defcustom anki-pandoc-replacements
  (list (cons (rx "") "")
        (cons (rx "%20") " ")
        (cons "^\n$" ""))
  "List of alists pairing regular expressions with a string that should replace each one.
Used to clean output from Pandoc."
  :type '(alist :key-type string
                :value-type string))

(defvar anki-shr-rendering-functions
  '(;; default function uses url-retrieve and fails on local images
    (img . anki-render-img)
    ;; titles are rendered *inside* the document by default
    )
  "Alist of rendering functions used with `shr-render-region'.")

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
  ;; (buffer-disable-undo)
  (cond ((eq anki-card-mode-parent-mode 'org-mode)
         (org-mode))
        ((eq anki-card-mode-parent-mode 'shrface-mode)
         (setq anki-shr-rendering-functions (append anki-shr-rendering-functions shr-external-rendering-functions))
         (shrface-mode))
        ((eq anki-card-mode-parent-mode 'fundamental-mode)
         (fundamental-mode))
        (t
         (fundamental-mode))))

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
    (let ((inhibit-read-only t))
      (with-current-buffer buff
        (anki-card-mode)
        (define-key file-map [mouse-1] 'anki-file-mouse-1)
        (define-key file-map [mouse-3] 'anki-file-mouse-3)
        (erase-buffer)
        (setq beg (point))
        ;; (insert id)
        (insert "\n")
        (setq end (point))
        (put-text-property beg end 'anki-card entry)

        (dolist (field (cl-mapcar 'cons model-names (split-string flds "\037")))
          (let* ((mf (car field))
                 (cf (cdr field)))
            (insert (format "<div><h1>%s</h1><p>%s</p></div>" mf cf))))

        (goto-char (point-min))
        (while (re-search-forward "src=\"\\(.*?\\)\"" nil t)
          (replace-match (format "src=\"%s%s\"" (concat (file-name-as-directory anki-collection-dir) "collection.media/") (match-string 1))))

        (goto-char (point-min))
        (while (re-search-forward "\\[sound:\\(.*?\\)\\]" nil t)
          (replace-match (format "<a href=\"%s%s\">Play</a>" (concat (file-name-as-directory anki-collection-dir) "collection.media/") (match-string 1))))


        (insert "\n")
        ;; (setq end (point))
        ;; (shr-render-region (point-min) (point-max))

        (if (eq anki-card-mode-parent-mode 'org-mode)
            (anki-render-org)
          (anki-render-html))

        (setq anki-show-card entry)
        (goto-char (point-min))))
    (unless (eq major-mode 'anki-card-mode)
      (funcall anki-show-card-switch buff)
      (when switch
        (switch-to-buffer-other-window (set-buffer (anki-search--buffer-name)))
        (goto-char original)))))

(defun anki-show-front (entry &optional switch)
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
         (ord (gethash 'ord entry))     ; template number
         (template (anki-decode-tmpls ord model))
         (original (point))
         (file-map (make-sparse-keymap))
         beg end)
    (let ((inhibit-read-only t))
      (with-current-buffer buff
        (anki-card-mode)
        (define-key file-map [mouse-1] 'anki-file-mouse-1)
        (define-key file-map [mouse-3] 'anki-file-mouse-3)
        (erase-buffer)
        (setq beg (point))
        ;; (insert id)
        (insert "\n")
        (setq end (point))
        (put-text-property beg end 'anki-card entry)

        ;; insert the question template
        (insert (nth 1 template))

        ;; replace the {{field}} based on https://docs.ankiweb.net/#/templates/fields?id=field-replacements
        (dolist (field (cl-mapcar 'cons model-names (split-string flds "\037")))
          (let* ((mf (car field))
                 (cf (cdr field)))
            (anki-field-replace-basic mf cf)))

        (anki-field-replace-media)

        (if (eq anki-card-mode-parent-mode 'org-mode)
            (anki-render-org)
          (anki-render-html))

        (setq anki-show-card entry)
        (goto-char (point-min))))
    (unless (eq major-mode 'anki-card-mode)
      (funcall anki-show-card-switch buff)
      (when switch
        (switch-to-buffer-other-window (set-buffer (anki-search--buffer-name)))
        (goto-char original)))))

(defun anki-preview-front ()
  (interactive)
  (anki-show-front (anki-find-card-at-point) :switch)
  (anki-play-audio))

(defun anki-show-back (entry &optional switch)
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
         (ord (gethash 'ord entry))     ; template number
         (template (anki-decode-tmpls ord model))
         (original (point))
         (file-map (make-sparse-keymap))
         beg end)
    (let ((inhibit-read-only t) question answer)
      (with-current-buffer buff
        (anki-card-mode)
        (define-key file-map [mouse-1] 'anki-file-mouse-1)
        (define-key file-map [mouse-3] 'anki-file-mouse-3)
        (erase-buffer)
        (setq beg (point))
        ;; (insert id)
        (insert "\n")
        (setq end (point))
        (put-text-property beg end 'anki-card entry)

        (setq question
              (with-temp-buffer
                ;; insert the question template
                (insert (nth 1 template))

                ;; replace the {{field}} based on https://docs.ankiweb.net/#/templates/fields?id=field-replacements
                (dolist (field (cl-mapcar 'cons model-names (split-string flds "\037")))
                  (let* ((mf (car field))
                         (cf (cdr field)))
                    (anki-field-replace-basic mf cf)))
                (buffer-string)))

        (setq answer
              (with-temp-buffer
                ;; insert the answer template
                (insert (nth 2 template))

                ;; replace the {{field}} based on https://docs.ankiweb.net/#/templates/fields?id=field-replacements
                (dolist (field (cl-mapcar 'cons model-names (split-string flds "\037")))
                  (let* ((mf (car field))
                         (cf (cdr field)))
                    (anki-field-replace-basic mf cf question)))
                (buffer-string)))

        (insert answer)

        (anki-field-replace-media)

        (if (eq anki-card-mode-parent-mode 'org-mode)
            (anki-render-org)
          (anki-render-html))

        (setq anki-show-card entry)
        (goto-char (point-min))))
    (unless (eq major-mode 'anki-card-mode)
      (funcall anki-show-card-switch buff)
      (when switch
        (switch-to-buffer-other-window (set-buffer (anki-search--buffer-name)))
        (goto-char original)))))

(defun anki-preview-back ()
  (interactive)
  (anki-show-back (anki-find-card-at-point) :switch)
  (anki-play-audio))

(defun anki-field-replace-basic (mf cf &optional question)
  "Replace the {{field}} based on https://docs.ankiweb.net/#/templates/fields?id=field-replacements."
  ;; Basic Replacements
  (goto-char (point-min))
  (while (re-search-forward (format "{{%s}}" mf) nil t)
    (replace-match cf))

  ;; Special Replacements
  (when question
    (goto-char (point-min))
    (while (re-search-forward (format "{{FrontSide}}" mf) nil t)
      (replace-match question)))

  ;; Hint Fields
  (goto-char (point-min))
  (while (re-search-forward (format "{{.*?:%s}}" mf) nil t)
    (replace-match cf))

  (goto-char (point-min))
  (while (re-search-forward (format "{{.%s}}" mf) nil t)
    (replace-match cf)))

(defun anki-field-replace-media ()
  ;; replace the src to point to local files
  (goto-char (point-min))
  (while (re-search-forward "src=\"\\(.*?\\)\"" nil t)
    (replace-match (format "src=\"%s%s\"" (concat (file-name-as-directory anki-collection-dir) "collection.media/") (match-string 1))))

  ;; replace the sound files to point to local files
  (goto-char (point-min))
  (while (re-search-forward "\\[sound:\\(.*?\\)\\]" nil t)
    (replace-match (format "<a href=\"%s%s\">Play</a>" (concat (file-name-as-directory anki-collection-dir) "collection.media/") (match-string 1)))))

(defun anki-render-org ()
  (unless (zerop (call-process-region (point-min) (point-max) "pandoc"
                                      t t nil
                                      (anki--pandoc-no-wrap-option)
                                      "-f" "html-raw_html-native_divs" "-t" "org"))
    ;; TODO: Add error output, see org-protocol-capture-html
    (error "Pandoc failed"))
  (anki--clean-pandoc-output))


(defun anki--pandoc-no-wrap-option ()
  "Return option `anki--pandoc-no-wrap-option', setting if unset."
  (or anki--pandoc-no-wrap-option
      (setq anki--pandoc-no-wrap-option (anki--check-pandoc-no-wrap-option))))

(defun anki--check-pandoc-no-wrap-option ()
  "Return appropriate no-wrap option string depending on Pandoc version."
  ;; Pandoc >= 1.16 deprecates the --no-wrap option, replacing it with
  ;; --wrap=none.  Sending the wrong option causes output to STDERR,
  ;; which `call-process-region' doesn't like.  So we test Pandoc to see
  ;; which option to use.
  (with-temp-buffer
    (let* ((limit 3)
           (checked 0)
           (process (start-process "test-pandoc" (current-buffer)
                                   "pandoc" "--dump-args" "--no-wrap")))
      (while (process-live-p process)
        (if (= checked limit)
            (progn
              ;; Pandoc didn't exit in time.  Kill it and raise an
              ;; error.  This function will return `nil' and
              ;; `anki--pandoc-no-wrap-option' will remain
              ;; `nil', which will cause this function to run again and
              ;; set the const when a capture is run.
              (set-process-query-on-exit-flag process nil)
              (error "Unable to test Pandoc.  Try increasing `anki-pandoc-sleep-time'.  If it still doesn't work, please report this bug! (Include the output of \"pandoc --dump-args --no-wrap\")"))
          (sleep-for anki-pandoc-sleep-time)
          (cl-incf checked)))
      (if (and (zerop (process-exit-status process))
               (not (string-match "--no-wrap is deprecated" (buffer-string))))
          "--no-wrap"
        "--wrap=none"))))

(defun anki--clean-pandoc-output ()
  "Remove unwanted things in current buffer of Pandoc output."

  (anki--remove-html-blocks)
  (anki--remove-custom_id_properties)
  (anki--remove-bad-characters))

(defun anki--remove-bad-characters ()
  "Remove unwanted characters from current buffer.
Bad characters are matched by `anki-pandoc-replacements'."
  (save-excursion
    (cl-loop for (re . replacement) in anki-pandoc-replacements
             do (progn
                  (goto-char (point-min))
                  (while (re-search-forward re nil t)
                    (replace-match replacement))))))

(defun anki--remove-html-blocks ()
  "Remove \"#+BEGIN_HTML...#+END_HTML\" blocks from current buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward (rx (optional "\n")
                                  "#+BEGIN_HTML"
                                  (minimal-match (1+ anything))
                                  "#+END_HTML"
                                  (optional "\n"))
                              nil t)
      (replace-match ""))))

(defun anki--remove-custom_id_properties ()
  "Remove property drawers containing CUSTOM_ID properties.
This is a blunt instrument: any drawer containing the CUSTOM_ID
property is removed, regardless of other properties it may
contain.  This seems to be the best course of action in current
Pandoc output."
  (let ((regexp (org-re-property "CUSTOM_ID" nil nil)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (when (org-at-property-p)
          (org-back-to-heading)
          ;; As a minor optimization, we don't bound the search to the current entry.  Unless the
          ;; current property drawer is malformed, which shouldn't happen in Pandoc output, it
          ;; should work.
          (re-search-forward org-property-drawer-re)
          (setf (buffer-substring (match-beginning 0) (match-end 0)) ""))))))

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


(defun anki-card-quit ()
  "Quit the *anki-card*."
  (interactive)
  (when (eq major-mode 'anki-card-mode)
    (if (get-buffer "*anki-card*")
        (kill-buffer "*anki-card*"))))

(defun anki-preview-card ()
  (interactive)
  (anki-show-card (anki-find-card-at-point) :switch)
  (anki-play-audio))

(defun anki-models-names (model)
  (cl-loop for name in (gethash "flds" model) collect
           (gethash "name" name)))

(defun anki-replay-audio ()
  "Replay Audio If Possible."
  (interactive)
  (if (process-live-p (get-process "anki-audio-player"))
      (kill-process (get-process "anki-audio-player")))
  (cond
   ;; if there is *anki-card* buffer
   ((get-buffer "*anki-card*")
    (anki-play-audio))
   ;; if no *anki-card* buffer
   ((equal major-mode 'anki-search-mode)
    (funcall 'anki-preview-card)
    (anki-play-audio))
   ;; if in *anki-card* buffer
   ((equal major-mode 'anki-card-mode)
    (anki-play-audio))))

(defun anki-play-audio ()
  "Collect and play the audio in current buffer"
  (interactive)
  ;; TODO: Play more than 1 audio
  (let ((sound (nth 1 (car (anki-shr-audio-collect)))))
    (when sound
      (message "Playing...")
      (if (and anki-audio-player sound)
          (start-process-shell-command
           "anki-audio-player" nil
           (mapconcat 'identity
                      `(,anki-audio-player
                        ,@(delq nil (list (shell-quote-argument (expand-file-name sound))))) " "))))))

(defun anki-shr-audio-collect ()
  "Collect the positions of visible links in the current `anki-card' buffer."
  (save-excursion
    (with-current-buffer (get-buffer "*anki-card*")
      (let (beg end string url collected-list)
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
        collected-list))))

(provide 'anki-card)
