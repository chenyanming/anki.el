;;; anki.el --- Yet another Anki client -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Damon Chan

;; Author: Damon Chan <elecming@gmail.com>
;; URL: https://github.com/chenyanming/calibredb.el
;; Keywords: tools
;; Created: 14 Sep 2020
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))

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

;; Yet another [[https://apps.ankiweb.net/][Anki]] Anki client.


;;; Code:
(require 'anki-core)
(require 'anki-search)
(require 'anki-card)
(require 'anki-utils)

(defcustom anki-in-sequence t
  "Set nil random or t in sequence when enter *anki*."
  :group 'anki
  :type 'boolean)
(defvar anki-number 0)
(defvar anki-last-number 0)
(defvar anki-loop-speed 1.0)
(defvar anki-loop-toggle nil)
(defvar anki-header-function #'anki-header)

(defvar anki-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map "v" #'anki-validate)
    ;; (define-key map "s" #'anki-play-audio)
    (define-key map "p" #'anki-previous)
    (define-key map "n" #'anki-next)
    ;; (define-key map "t" #'anki-toggle-anki)
    (define-key map "r" #'anki-play-audio)
    (define-key map "l" #'anki-list-decks)
    ;; (define-key map "]" #'anki-loop-inc)
    ;; (define-key map "[" #'anki-loop-dec)
    ;; (define-key map "a" #'anki-first)
    ;; (define-key map "j" #'anki-jump)
    (define-key map "q" #'anki-quit)
    ;; (define-key map "d" #'anki-details)
    map)
  "Keymap for `anki-mode'.")

(defun anki-mode ()
  "Major mode for anki.
\\{anki-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map anki-mode-map)
  (setq major-mode 'anki-mode
        mode-name "anki-mode"
        truncate-lines t
        buffer-read-only t
        header-line-format '(:eval (funcall anki-header-function)))
  (buffer-disable-undo)
  ;; (add-hook 'kill-buffer-hook '(lambda () (when anki-loop-toggle (setq anki-loop-toggle nil) (anki-loop-stop))) nil :local)
  (run-mode-hooks 'anki-mode-hook))

(defun anki-header ()
  "Header function for *anki* buffer."
  (format "%s  %s  %s  %s  %s  %s  %s"
          anki-current-deck
          ;; (concat (propertize "r" 'face 'bold) (if anki-in-sequence ":in sequence" ":random"))
          (concat (propertize "l" 'face 'bold) "ist decks")
          (concat (propertize "r" 'face 'bold) "eplay")
          ;; (if anki-loop-toggle (concat "(+" (number-to-string anki-loop-speed) "s) ") "")
          ;; (concat (propertize "v" 'face 'bold) "alidate")
          ;; (concat (propertize "s" 'face 'bold) "ay")
          (concat (propertize "n" 'face 'bold) "ext")
          (concat (propertize "p" 'face 'bold) "revious")
          (concat (propertize "q" 'face 'bold) "uit")
          (concat (number-to-string anki-number))))

(defun anki (&optional index)
  "Start to learn anki.
Optional argument INDEX is the number of anki in the list."
  (interactive)
  ;; Get All Cards
  (if anki-search-entries
      anki-search-entries
    (progn
      (setq anki-search-entries (anki-format-cards))
      (setq anki-full-entries anki-search-entries)))
  (switch-to-buffer (get-buffer-create "*anki*"))
  (setq buffer-read-only nil)
  (erase-buffer)
  (setq anki-last-number anki-number)
  (let* ((item (or (nth (or index anki-number) anki-search-entries)
                   (nth (setq anki-number 0) anki-search-entries)))
         (card (anki-get-card item))
         (question (nth 0 card))
         (answer (nth 1 card))
         (number (or index (if anki-in-sequence
                               anki-number
                             (random (1- (length anki-search-entries))))))
         beg end)
    (setq anki-number number)

    (setq beg (point))
    (insert "<h1>Question</h1>")
    (insert question)
    (setq end (point))
    (put-text-property beg end 'question question)

    (setq beg (point))
    (insert "<h1>Answer</h1>")
    (insert answer)
    (setq end (point))
    (put-text-property beg end 'answer answer)

    (anki-render-html)

    (goto-char (point-min))             ; cursor is always in the (point-min)
    ;; (shrface-mode)
    (anki-play-audio))
  (setq buffer-read-only t)
  (unless (eq major-mode 'anki-mode)
    (anki-mode)))

(defun anki-next ()
  "Next anki."
  (interactive)
  (if anki-in-sequence
      (progn
        (if (< anki-number (1- (length anki-search-entries)))
            (setq anki-number (1+ anki-number))
          (setq anki-number 0))
        (anki anki-number))
    (anki)))

(defun anki-previous ()
  "Previous anki."
  (interactive)
  (if anki-in-sequence
      (progn
        (if (> anki-number 0)
            (setq anki-number (1- anki-number))
          (setq anki-number (1- (length anki-search-entries))))
        (anki anki-number))
    (anki anki-last-number)))

(defun anki-quit ()
  "Quit anki."
  (interactive)
  (when (eq major-mode 'anki-mode)
    (if (get-buffer "*anki*")
        (kill-buffer "*anki*"))
    (let ((process (get-process "anki-audio-player")))
      (if (process-live-p process)
          (delete-process process)))))
(provide 'anki)
