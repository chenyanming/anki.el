;;; anki-connect.el -*- lexical-binding: t; -*-

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
;;; Essentially copied from `anki-editor.el', but modified to support more requests.
;;; This is only for experiment.

;;; Code:

(require 'request)

(defcustom anki-connect-listening-address
  "127.0.0.1"
  "The network address AnkiConnect is listening.")

(defcustom anki-connect-listening-port
  "8765"
  "The port number AnkiConnect is listening.")

(defun anki-connect-action (action &optional params version)
  (let (a)
    (when version
      (push `(version . ,version) a))
    (when params
      (push `(params . ,params) a))
    (push `(action . ,action) a)))

(defun anki-connect-invoke-queue ()
  (let (action-queue)
    (lambda (&optional action params handler)
      (if action
          (push (cons (anki-connect-action action params) handler) action-queue)
        (when action-queue
          (apply #'anki-connect-invoke-multi (nreverse action-queue))
          (setq action-queue nil))))))

(defun anki-connect-invoke (action &optional params)
  "Invoke AnkiConnect with ACTION and PARAMS."
  (let ((request-body (json-encode (anki-connect-action action params 5)))
        (request-backend 'curl)
        (json-array-type 'list)
        reply err)

    (let ((response (request (format "http://%s:%s"
                                     anki-connect-listening-address
                                     anki-connect-listening-port)
                      :type "POST"
                      :parser 'json-read
                      :data request-body
                      :success (cl-function (lambda (&key data &allow-other-keys)
                                              (setq reply data)))
                      :error (cl-function (lambda (&key _ &key error-thrown &allow-other-keys)
                                            (setq err (string-trim (cdr error-thrown)))))
                      :sync t)))

      ;; HACK: With sync set to t, `request' waits for curl process to
      ;; exit, then response data becomes available, but callbacks
      ;; might not be called right away but at a later time, that's
      ;; why here we manually invoke callbacks to receive the result.
      (unless (request-response-done-p response)
        (request--curl-callback (get-buffer-process (request-response--buffer response)) "finished\n")))

    (when err (error "Error communicating with AnkiConnect using cURL: %s" err))
    (or reply (error "Got empty reply from AnkiConnect"))))

(defmacro anki-connect-invoke-result (&rest args)
  "Invoke AnkiConnect with ARGS, return the result from response or raise an error."
  `(let-alist (anki-connect-invoke ,@args)
     (when .error (error .error))
     .result))

(defun anki-connect-invoke-multi (&rest actions)
  (-zip-with (lambda (result handler)
               (when-let ((_ (listp result))
                          (err (alist-get 'error result)))
                 (error err))
               (and handler (funcall handler result)))
             (anki-connect-invoke-result
              "multi" `((actions . ,(mapcar #'car actions))))
             (mapcar #'cdr actions)))


(defun anki-connect-get-collection-statas ()
  "Get all tags from Anki."
  (anki-connect-invoke-result "getCollectionStatsHTML" ))

(defun anki-connect-gui-current-card ()
  "Get all tags from Anki."
  (anki-connect-invoke-result "guiCurrentCard" ))

(defun anki-connect-gui-start-card-timer ()
  "Get all tags from Anki."
  (anki-connect-invoke-result "guiStartCardTimer" ))

(defun anki-connect-gui-show-question ()
  "Get all tags from Anki."
  (anki-connect-invoke-result "guiShowQuestion" ))

(defun anki-connect-gui-show-answer ()
  "Get all tags from Anki."
  (anki-connect-invoke-result "guiShowAnswer" ))

(defun anki-connect-gui-answer-card (number)
  "Get all tags from Anki."
  (anki-connect-invoke-result "guiAnswerCard"
                              `((ease . ,number))))

(defun anki-connect-gui-deck-overview ()
  "Get all tags from Anki."
  (anki-connect-invoke-result "guiDeckOverview"
                              `((name . "新概念英语::新概念英语第二册（美音）"))))

(defun anki-connect-gui-deck-review ()
  "Get all tags from Anki."
  (let* ((deck (let* ((pdecks (anki-core-parse-decks))
                      (vdecks (hash-table-values pdecks)))
                 (cl-loop for deck in vdecks collect
                          (cons (gethash "name" deck) (gethash "id" deck)))))
         (deck-name (setq anki-core-current-deck (completing-read "Decks: " deck)))
         (selected-did (cdr (assoc deck-name deck)))
         current-card)
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
                   collect entry))

    (anki-connect-invoke-result "guiDeckReview"
                                `((name . ,deck-name)))

    (let* ((current-card (anki-connect-gui-current-card))
           (number (seq-position
                    (mapcar
                     (lambda (x) (gethash 'id x)) anki-search-entries)
                    (number-to-string (cdr (assoc 'cardId current-card))))))
      (cond ((eq major-mode 'anki-search-mode)
             (anki-browser))
            (t
             (anki number current-card))))))


(defun anki-connect-decode-current-card (current-card)
  (let ((next-reviews (assoc 'nextReviews current-card))
        (buttons (assoc 'buttons current-card)))
    (cdr (cl-mapcar 'cons buttons next-reviews)))) ; delete the car


