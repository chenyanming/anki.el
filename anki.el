;;; anki.el --- Yet another Anki client -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Damon Chan

;; Author: Damon Chan <elecming@gmail.com>
;; URL: https://github.com/chenyanming/calibredb.el
;; Keywords: tools
;; Created: 14 Sep 2020
;; Version: 0.2.0
;; Package-Requires: ((emacs "25.1") (emacsql "3.0.0"))

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
(require 'anki-learn)
(require 'anki-utils)
(require 'anki-db)

(defcustom anki-in-sequence t
  "Set nil random or t in sequence when enter *anki*."
  :group 'anki
  :type 'boolean)
(defvar anki-number 0)
(defvar anki-last-number 0)
(defvar anki-loop-speed 1.0)
(defvar anki-loop-toggle nil)
(defvar anki-header-function #'anki-header)
(defvar anki-front-or-back t)
(defvar anki-front nil)
(defvar anki-back nil)
(defvar anki-mock-due-date nil)
(defvar anki-current-card nil)
(defvar anki-new-card-p nil)
(defvar anki-again-card-p nil)

(defvar anki-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map "v" #'anki-validate)
    ;; (define-key map "s" #'anki-play-audio)
    (define-key map "p" #'anki-previous)
    (define-key map "n" #'anki-next)
    ;; (define-key map "t" #'anki-toggle-anki)
    (define-key map "r" #'anki-play-audio)
    (define-key map "l" #'anki-list-decks)
    (define-key map "0" #'anki-answer)
    (define-key map "1" #'anki-answer)
    (define-key map "2" #'anki-answer)
    (define-key map "3" #'anki-answer)
    (define-key map "4" #'anki-answer)
    (define-key map "5" #'anki-answer)
    (define-key map "f" #'anki-flip-card)
    (define-key map (kbd "<RET>") #'anki-flip-card)
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
  (format "%s: %s  %s/%s/%s  %s  %s  %s  %s  %s  %s"
          (propertize "Anki" 'face font-lock-warning-face)
          anki-core-current-deck
          (if anki-new-card-p
              (propertize (number-to-string (- (anki-db-current-deck-total-card-number) (anki-db-current-deck-total-review-card-number) )) 'face '(:foreground "DeepSkyBlue" :underline t))
            (propertize (number-to-string (- (anki-db-current-deck-total-card-number) (anki-db-current-deck-total-review-card-number) )) 'face '(:foreground "DeepSkyBlue")))

          (if (and anki-again-card-p (not anki-new-card-p))
              (propertize (number-to-string (anki-db-current-deck-total-again-number)) 'face '(:foreground "OrangeRed" :underline t))
            (propertize (number-to-string (anki-db-current-deck-total-again-number)) 'face '(:foreground "OrangeRed")))

          (if (or anki-again-card-p anki-new-card-p)
              (propertize (number-to-string (- (anki-db-current-deck-total-due-card-number) (anki-db-current-deck-total-again-number))) 'face '(:foreground "green"))
            (propertize (number-to-string (- (anki-db-current-deck-total-due-card-number) (anki-db-current-deck-total-again-number))) 'face '(:foreground "green" :underline t)))

          ;; (concat (propertize "r" 'face 'bold) (if anki-in-sequence ":in sequence" ":random"))
          (concat (propertize "l" 'face 'bold) "ist")
          (concat (propertize "r" 'face 'bold) "eplay")
          ;; (if anki-loop-toggle (concat "(+" (number-to-string anki-loop-speed) "s) ") "")
          ;; (concat (propertize "v" 'face 'bold) "alidate")
          ;; (concat (propertize "s" 'face 'bold) "ay")
          (concat (propertize "f" 'face 'bold) "lip")
          (concat (propertize "n" 'face 'bold) "ext")
          (concat (propertize "p" 'face 'bold) "revious")
          (concat (propertize "q" 'face 'bold) "uit")
          ;; (concat (number-to-string anki-number))
          ))

;;;###autoload
(defun anki (&optional index)
  "Start to learn anki.
Optional argument INDEX is the number of anki in the list."
  (interactive)
  (anki-core-db)
  ;; Get All Cards
  (if anki-search-entries
      anki-search-entries
    (progn
      (setq anki-search-entries (anki-core-cards-list))
      (setq anki-full-entries anki-search-entries)))
  (switch-to-buffer (get-buffer-create "*anki*"))
  (setq buffer-read-only nil)
  (erase-buffer)
  (setq anki-last-number anki-number)
  (let* ((item (anki-db-latest-due-card-or-random-card))
         ;; (item (or (nth (or index anki-number) anki-search-entries)
         ;;           (nth (setq anki-number 0) anki-search-entries)))
         (id (gethash 'id item))
         (card (anki-get-card item))
         (question (nth 0 card))
         (answer (nth 1 card))
         (due-days (car (anki-learn-get-due-date id)))
         (due-date (cdr (anki-learn-get-due-date id)))
         (mock-due-date (anki-learn-mock-smart-reschedule id))
         (number (or index (if anki-in-sequence
                               anki-number
                             (random (1- (length anki-search-entries))))))
         real-due-days
         beg end)
    (setq anki-number number)
    (setq anki-front question)
    (setq anki-back answer)
    (setq anki-mock-due-date mock-due-date)
    ;; save current card information to global variable
    (setq anki-current-card item)
    ;; cal the real due days
    (if due-date
        (setq real-due-days
              (/ (- (time-convert (current-time) 'integer )
                    (time-convert (encode-time (parse-time-string due-date) ) 'integer)) (* 24 3600.0))) )
    ;; print the card info
    (message "Card id: %s, Due: %s, Expired: %f days"
             id
             (if due-date due-date "NEW CARD")
             (if due-date real-due-days 0))

    (if real-due-days
        (if (< real-due-days 1)
            (setq anki-again-card-p t)
          (setq anki-again-card-p nil))
      (setq anki-again-card-p nil))

    (if due-date
        (setq anki-new-card-p nil)
      (setq anki-new-card-p t))

    ;; only show question
    (anki-show-question question)
    ;; (shrface-mode)
    )
  (setq buffer-read-only t)
  (unless (eq major-mode 'anki-mode)
    (anki-mode)))

(defun anki-flip-card ()
  "Flip Card."
  (interactive)
  (let ((inhibit-read-only t))
    (goto-char (point-min))
    (delete-region (point) (point-max))
    (goto-char (point-min))
    (if anki-front-or-back
        (anki-show-question anki-front)
      (anki-show-answer anki-back))))

(defun anki-show-buttons (mock-due-date)
    ;; insert answer button
    (let ((answer-map (make-sparse-keymap)))
      (define-key answer-map [mouse-1] 'anki-answer-mouse-1)
      ;; (insert (concat (propertize "CHALLENGING"
      ;;                             'face font-lock-warning-face
      ;;                             'mouse-face 'mode-line-highlight
      ;;                             'keymap answer-map) " " ))
      ;; (insert (concat (propertize "DIFFICULT"
      ;;                             'face font-lock-warning-face
      ;;                             'mouse-face 'mode-line-highlight
      ;;                             'keymap answer-map) " " ))
      (anki-insert-justify-text
       (format "%s%s%s%s%s%s%s%s"
               (propertize (format "%s" "AGAIN")
                           'face '(:background "orange red" :height 1.5)
                           'mouse-face 'mode-line-highlight
                           'keymap answer-map)
               (propertize (format " %s " (nth 0 mock-due-date))
                           'face 'bold)
               (propertize (format "%s"  "HARD")
                                   'face '(:background "grey"  :height 1.5)
                                   'mouse-face 'mode-line-highlight
                                   'keymap answer-map)
               (propertize (format " %s " (nth 1 mock-due-date))
                                   'face 'bold)
               (propertize (format "%s"  "GOOD")
                                   'face '(:background "green"  :height 1.5)
                                   'mouse-face 'mode-line-highlight
                                   'keymap answer-map)
               (propertize (format " %s " (nth 3 mock-due-date))
                                   'face 'bold)
               (propertize (format "%s"  "EASY")
                                   'face '(:background "light sky blue" :height 1.5)
                                   'mouse-face 'mode-line-highlight
                                   'keymap answer-map)
               (propertize (format " %s " (nth 5 mock-due-date))
                                   'face 'bold)))

      ;; (insert (propertize (format "%s" "AGAIN")
      ;;                     'face '(:background "orange red" :height 1.5)
      ;;                     'mouse-face 'mode-line-highlight
      ;;                     'keymap answer-map))
      ;; (insert (propertize (format " %s " (nth 0 mock-due-date))
      ;;                     'face 'bold))

      ;; (insert (propertize (format "%s" "CHALLENGING")
      ;;                     'face '(:background "orange red" :height 1.5)
      ;;                     'mouse-face 'mode-line-highlight
      ;;                     'keymap answer-map))
      ;; (insert (propertize (format " %s " (nth 1 mock-due-date))
      ;;                     'face 'bold))

      ;; (insert (propertize (format "%s" "DIFFICULT")
      ;;                     'face '(:background "orange red" :height 1.5)
      ;;                     'mouse-face 'mode-line-highlight
      ;;                     'keymap answer-map))
      ;; (insert (propertize (format " %s " (nth 1 mock-due-date))
      ;;                     'face 'bold))

      ;; (insert (propertize (format "%s"  "HARD")
      ;;                     'face '(:background "grey"  :height 1.5)
      ;;                     'mouse-face 'mode-line-highlight
      ;;                     'keymap answer-map))
      ;; (insert (propertize (format " %s " (nth 1 mock-due-date))
      ;;                     'face 'bold))
      ;; (insert (propertize (format "%s"  "GOOD")
      ;;                     'face '(:background "green"  :height 1.5)
      ;;                     'mouse-face 'mode-line-highlight
      ;;                     'keymap answer-map))
      ;; (insert (propertize (format " %s " (nth 3 mock-due-date))
      ;;                     'face 'bold))
      ;; (insert (propertize (format "%s"  "EASY")
      ;;                     'face '(:background "light sky blue" :height 1.5)
      ;;                     'mouse-face 'mode-line-highlight
      ;;                     'keymap answer-map))
      ;; (insert (propertize (format " %s " (nth 5 mock-due-date))
      ;;                     'face 'bold))

      ))

(defun anki-insert-justify-text (text)
  (let (beg end)
    (setq beg (point))
    (insert text)
    (setq end (point))
    (set-justification-left beg end)))

(defun anki-show-answer-button ()
  ;; insert show answer button
  (let ((show-answer-map (make-sparse-keymap)))
    (define-key show-answer-map [mouse-1] 'anki-show-answer-mouse-1)
    (anki-insert-justify-text (concat (propertize "SHOW ANSWER"
                                'face '(:background "grey" :height 1.5)
                                'mouse-face 'mode-line-highlight
                                'keymap show-answer-map) " " ))))


(defun anki-show-question (question)
  (setq anki-front-or-back nil)
  (let (beg end)
    (anki-show-answer-button)
    (setq beg (point))
    ;; (insert "<h1>Question</h1>")
    (insert "<hr>")
    (insert question)
    (setq end (point))
    (put-text-property beg end 'question question)
    (anki-render-region beg end)
    ;; save card information to text-property for later anki-find-card-at-point extraction
    (put-text-property (point-min) (+ 1 (point-min) ) 'anki-entry anki-current-card)
    (goto-char (point-min))
    (anki-play-audio))

  ;; insert due date
  ;; (insert (concat "\n" (propertize (or (anki-learn-get-due-date id) "New Card")
  ;;                             'face font-lock-keyword-face
  ;;                             'mouse-face 'mode-line-highlight) "\n"))

  )

(defun anki-show-answer (answer)
  (setq anki-front-or-back t)
  (let (beg end)
    ;; show button
    (anki-show-buttons anki-mock-due-date)

    (setq beg (point))
    ;; (insert "<h1>Answer</h1>")
    (insert "<hr>")
    (insert answer)
    (setq end (point))
    (put-text-property beg end 'answer answer)
    (anki-render-region beg end)
    ;; save card information to text-property for later anki-find-card-at-point extraction
    (put-text-property (point-min) (+ 1 (point-min) ) 'anki-entry anki-current-card)
    (goto-char (point-min))
    (anki-play-audio)))


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

(defun anki-answer ()
  "TODO: This function automatically recognizes the number"
  (interactive)
  (let* ((event last-command-event)
         (key (make-vector 1 event))
         (key-desc (key-description key)))
    (anki-learn-smart-reschedule (string-to-number (car (last (split-string key-desc "-")))))
    (anki)))

(defun anki-answer-mouse-1 (event)
  "Visit the location click on.
Argument EVENT mouse event."
  (interactive "e")
  (let ((window (posn-window (event-end event)))
        (pos (posn-point (event-end event))))
    (if (not (windowp window))
        (error "No tag chosen"))
    (with-current-buffer (window-buffer window)
      (goto-char pos)
      (anki-learn-smart-reschedule (let ((level (word-at-point t)))
                                     (cond
                                      ((equal level "AGAIN") 0)
                                      ;; ((equal level "CHALLENGING") 1)
                                      ;; ((equal level "DIFFICULT") 1)
                                      ((equal level "HARD") 1)
                                      ((equal level "GOOD") 3)
                                      ((equal level "EASY") 5))))
      (anki))))

(defun anki-show-answer-mouse-1 (event)
  "TODO: Visit the location click on.
Argument EVENT mouse event."
  (interactive "e")
  (let ((window (posn-window (event-end event)))
        (pos (posn-point (event-end event))))
    (if (not (windowp window))
        (error "No tag chosen"))
    (with-current-buffer (window-buffer window)
      (goto-char pos)
      (anki-flip-card))))

;;;###autoload
(defun anki-list-decks ()
  "List decks."
  (interactive)
  (anki-core-db)
  (let* ((deck (let* ((pdecks (anki-core-parse-decks))
                      (vdecks (hash-table-values pdecks)))
                 (cl-loop for deck in vdecks collect
                          (cons (gethash "name" deck) (gethash "id" deck)))))
         (deck-name (setq anki-core-current-deck (completing-read "Decks: " deck)))
         (selected-did (setq anki-core-current-deck-id (cdr (assoc deck-name deck)) )))
    ;; Get all cards
    (if anki-search-entries
        anki-search-entries
      (progn
        (setq anki-search-entries (anki-core-cards-list))
        (setq anki-full-entries anki-search-entries)))
    ;; Filter cards
    (setq anki-search-entries
          (cl-loop for entry in anki-full-entries
                   when (hash-table-p entry)
                   for table-did = (anki-core-get-deck (gethash 'did entry))
                   if (hash-table-p table-did)
                   if (equal selected-did (gethash "id" table-did))
                   collect entry)))
  (cond ((eq major-mode 'anki-search-mode)
         (anki-browser))
        (t
         (anki 0))))

(provide 'anki)
