;;; anki-search.el -*- lexical-binding: t; -*-

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

(defface anki-search-header-highlight-face
  '((t :inherit region :weight bold :underline t))
  "Face for the header at point."
  :group 'anki-faces)

(defvar anki-search-header-function #'anki-search-header
  "Function that returns the string to be used for the Calibredb search header.")

(defvar anki-search-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-3] #'anki-search-mouse)
    (define-key map (kbd "<RET>") #'anki-preview-card)
    (define-key map "j" #'anki-next-card)
    (define-key map "k" #'anki-previous-card)
    (define-key map "g" #'anki-list-decks)
    ;; (define-key map "n" #'anki-next-deck)
    ;; (define-key map "p" #'anki-previous-deck)
    (define-key map "r" #'anki-search-refresh)
    (define-key map "q" #'anki-search-quit)
    (define-key map "j" #'anki-next-card)
    (define-key map "k" #'anki-previous-card)
    (define-key map "\M-j" #'anki-preview-next-card)
    (define-key map "\M-k" #'anki-preview-previous-card)
    map)
  "Keymap for `anki-search-mode'.")

(defun anki-search-header ()
  "TODO: Return the string to be used as the Calibredb header.
Indicating the library you use."
  "Anki"
  ;; (format "%s: %s   %s"
  ;;         (propertize anki-virtual-library-name 'face font-lock-preprocessor-face)
  ;;         (propertize anki-root-dir 'face font-lock-type-face)
  ;;         (concat
  ;;          (propertize (format "Total: %s"
  ;;                              (if (equal anki-search-entries '(""))
  ;;                                  "0   "
  ;;                                (concat (number-to-string (length anki-search-entries)) "   "))) 'face font-lock-warning-face)
  ;;          (propertize (format "%s" (if (equal anki-search-filter "")
  ;;                                       ""
  ;;                                     (concat anki-search-filter "   "))) 'face font-lock-keyword-face)
  ;;          (propertize (let ((len (length (anki-find-marked-candidates))))
  ;;                        (if (> len 0)
  ;;                            (concat "Marked: " (number-to-string len)) "")) 'face font-lock-negation-char-face)))
  )
(defun anki-search--minibuffer-setup ()
  "Set up the minibuffer for live filtering."
  (when anki-search-filter-active
    (when (eq :live anki-search-filter-active)
      (add-hook 'post-command-hook 'anki-search--live-update nil :local))))

(defun anki-search--live-update ()
  "Update the anki-search buffer based on the contents of the minibuffer."
  (when (eq :live anki-search-filter-active)
    ;; (message "HELLO")
    (let ((buffer (anki-search-buffer))
          (current-filter (minibuffer-contents-no-properties)))
      (when buffer
        (with-current-buffer buffer
          (let ((anki-search-filter current-filter))
            (anki-search-update :force)))))))

(defun anki-search-buffer ()
  "Create buffer anki-search."
  (get-buffer-create "*anki-search*"))

(defcustom anki-search-unique-buffers nil
  "TODO: When non-nil, every entry buffer gets a unique name.
This allows for displaying multiple serch buffers at the same
time."
  :group 'anki
  :type 'boolean)

(defun anki-search--buffer-name ()
  "Return the appropriate buffer name for ENTRY.
The result depends on the value of `anki-search-unique-buffers'."
  (if anki-search-unique-buffers
      (format "*anki-search-<%s>*" anki-collection-dir)
    "*anki-search*"))

(defcustom anki-search-filter ""
  "Query string filtering shown entries."
  :group 'anki
  :type 'string)

(defun anki-search-update (&optional force)
  "Update the anki-search buffer listing to match the database.
When FORCE is non-nil, redraw even when the database hasn't changed."
  (interactive)
  (with-current-buffer (anki-search-buffer)
    (when force
      (let ((inhibit-read-only t)
            (standard-output (current-buffer)))
        (erase-buffer)
        ;; reset anki-virtual-library-name
        (unless (-contains? (mapcar 'cdr anki-virtual-library-alist) anki-search-filter)
          (setq anki-virtual-library-name anki-virtual-library-default-name))
        (anki-search--update-list)
        ;; (setq anki-search-entries (anki-candidates))
        (dolist (entry anki-search-entries)
          (funcall anki-search-print-entry-function entry)
          (insert "\n"))
        ;; (insert "End of entries.\n")
        (goto-char (point-min))         ; back to point-min after filtering
        (setf anki-search-last-update (float-time))))))

(defvar anki-search-filter-active nil
  "When non-nil, anki is currently reading a filter from the minibuffer.
When live editing the filter, it is bound to :live.")

(define-derived-mode anki-search-mode fundamental-mode "anki-search"
  "Major mode for listing calibre entries.
\\{anki-search-mode-map}"
  (setq truncate-lines t
        buffer-read-only t
        header-line-format '(:eval (funcall anki-search-header-function)))
  (buffer-disable-undo)
  (set (make-local-variable 'hl-line-face) 'anki-search-header-highlight-face)
  (hl-line-mode)
  (add-hook 'minibuffer-setup-hook 'anki-search--minibuffer-setup))

(defvar anki-search-entries nil
  "List of the entries currently on display.")

(defvar anki-full-entries nil
  "List of the all entries currently on library.")

(defun anki-browser ()
  "Enter calibre Search Buffer."
  (interactive)
  (let ((cand
         (if anki-search-entries
             anki-search-entries
           (progn
             (setq anki-search-entries (anki-format-cards))
             (setq anki-full-entries anki-search-entries)))
         ;; (progn
         ;;   (setq anki-search-entries (anki-format-cards))
         ;;   (setq anki-full-entries anki-search-entries))
         ))
    (cond ((not cand)
           (message "INVALID ANKI"))
          (t
           (when (get-buffer (anki-search-buffer))
             (kill-buffer (anki-search-buffer)))
           ;; Set virtual library name when the first time to launch anki
           ;; (if (equal anki-search-filter "")
           ;;     (setq anki-virtual-library-name anki-virtual-library-default-name))
           (switch-to-buffer (anki-search-buffer))
           (goto-char (point-min))
           (unless (equal cand '(""))   ; not empty library
             (dolist (item cand)
               (if (hash-table-p item)
                   (let (beg end)
                     (setq beg (point))
                     (insert
                      (concat
                       (if (stringp (gethash 'card-format item))
                           (gethash 'card-format item) "")))
                     ;; (anki-detail-view-insert-image item)
                     (setq end (point))
                     (put-text-property beg end 'anki-entry item)
                     ;; (require 'shr)
                     ;; (if (fboundp 'shr-render-region)
                     ;;     (shr-render-region beg end))
                     (insert "\n"))))
             (goto-char (point-min)))
           (unless (eq major-mode 'anki-search-mode)
             (anki-search-mode))))))

(defun anki-search-quit ()
  "Quit *anki-card* or *anki-search*."
  (interactive)
  (when (eq major-mode 'anki-search-mode)
    (cond ((get-buffer "*anki-card*")
           (pop-to-buffer "*anki-card*")
           (if (< (length (window-prev-buffers)) 2)
               (kill-buffer-and-window)
             (kill-buffer)))
          ((get-buffer "*anki-search*")
           (kill-buffer "*anki-search*")))))

(defun anki-search-refresh ()
  "Refresh anki."
  (interactive)
  (setq anki-search-entries (anki-format-cards))
  (setq anki-full-entries anki-search-entries)
  (anki-browser))

(defun anki-next-card ()
  "Move to next card."
  (interactive)
  (let ((ori "") (new ""))
    (while (and (equal new ori) new ori)
      (setq ori (gethash 'id (anki-find-card-at-point)))
      (forward-line 1)
      (setq new (gethash 'id (anki-find-card-at-point))))))

(defun anki-previous-card ()
  (interactive)
  (let ((ori "") (new ""))
    (while (and (equal new ori) new ori (> (line-number-at-pos) 1))
      (forward-line -1)
      (save-excursion
        (setq ori (gethash 'id (anki-find-card-at-point)))
        (forward-line -1)
        (setq new (gethash 'id (anki-find-card-at-point)))))))

(defun anki-preview-next-card ()
  "Preview next card."
  (interactive)
  (anki-next-card)
  (anki-show-card (anki-find-card-at-point) :switch))

(defun anki-preview-previous-card ()
  "Preview previous card."
  (interactive)
  (anki-previous-card)
  (anki-show-card (anki-find-card-at-point) :switch))

(provide 'anki-search)
