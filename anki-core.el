;;; anki-core.el -*- lexical-binding: t; -*-

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

(require 'shr)
(require 'json)
(require 'cl-lib)

(eval-when-compile (defvar sql-sqlite-program))

(defvar anki-core-query-revlog "
WITH d AS
(SELECT cards.*, notes.*
FROM cards
LEFT JOIN  notes
ON cards.nid = notes.id
)

SELECT d.*, revlog.*
FROM d
LEFT JOIN revlog
ON d.id = revlog.cid "

  "TODO anki database query statement.")

(defvar anki-core-query-cards "
SELECT cards.*, notes.*
FROM cards
LEFT JOIN  notes
ON cards.nid = notes.id "



  "TODO anki database query statement.")

(defvar anki-core-query-decks "SELECT decks FROM col")
(defvar anki-core-query-models "SELECT models FROM col")

(defcustom anki-sql-separator "\3"
  "SQL separator, used in parsing SQL result into list."
  :group 'anki
  :type 'string)

(defcustom anki-sql-newline "\2"
  "SQL newline, used in parsing SQL result into list."
  :group 'anki
  :type 'string)

(defcustom anki-collection-dir "~/Library/Application Support/Anki2/Default"
  "SQL separator, used in parsing SQL result into list."
  :group 'anki
  :type 'string)

(defvar anki-current-deck ""
  "Current deck.")

(defun anki-core-query (sql-query)
  "Query calibre databse and return the result.
Argument SQL-QUERY is the sqlite sql query string."
  (interactive)
  (let ((file (concat (file-name-as-directory anki-collection-dir) "collection.anki2"))
        (temp (concat (file-name-as-directory temporary-file-directory) "collection.anki2")))
    (if (file-exists-p file)
        (progn
          ;; TODO Copy to temp file to avoid database collision, but not ideal solution.
          (let ((cmd (format "cp %s %s && %s -separator %s -newline %s -list -nullvalue \"\" -noheader %s \"%s\""
                             (shell-quote-argument (expand-file-name file))
                             (shell-quote-argument (expand-file-name temp))
                             sql-sqlite-program
                             anki-sql-separator
                             anki-sql-newline
                             (shell-quote-argument (expand-file-name temp))
                             sql-query)))
            (shell-command-to-string cmd))) nil)))

(defun anki-core-parse-decks ()
  (let ((decks (let* ((json-array-type 'list) ;;;;;;;;; 'vector is the default
                      (json-object-type 'hash-table)
                      (json-false nil))
                 (json-read-from-string (anki-core-query anki-core-query-decks))))
        result)
    decks
    ;; (dolist (deck decks result)
    ;;   (let* ((id (alist-get 'id deck))
    ;;          (mod (alist-get 'mod deck))
    ;;          (name (alist-get 'name deck))
    ;;          (usn (alist-get 'usn deck))
    ;;          (lrnToday (alist-get 'lrnToday deck))
    ;;          (revToday (alist-get 'revToday deck))
    ;;          (newToday (alist-get 'newToday deck))
    ;;          (timeToday (alist-get 'timeToday deck))
    ;;          (collapsed (alist-get 'collapsed deck))
    ;;          (desc (alist-get 'desc deck))
    ;;          (dyn (alist-get 'dyn deck))
    ;;          (dyn (alist-get 'dyn deck))
    ;;          (conf (alist-get 'conf deck))
    ;;          (conf (alist-get 'conf deck))
    ;;          (extendNew (alist-get 'extendNew deck))
    ;;          (extendRev (alist-get 'extendRev deck)))
    ;;     (push id result)))

    ))

(defun anki-core-parse-models ()
  (let ((models (let* ((json-array-type 'list) ;;;;;;;;; 'vector is the default
                      (json-object-type 'hash-table)
                      (json-false nil))
                 (json-read-from-string (anki-core-query anki-core-query-models))))) models))

(defun anki-core-parse-cards ()
  (let* ((query-result (anki-core-query anki-core-query-cards))
         (lines (if query-result (split-string query-result anki-sql-newline)))
         (decks (anki-core-parse-decks))
         (models (anki-core-parse-models)))
    (cond ((equal "" query-result) '(""))
          (t
           ;; (let (result)
           ;;   (dolist (line lines result)
           ;;     ;; decode and push to result
           ;;     (push (anki-core-parse-card-to-hash-table line decks) result)
           ;;     ;; (push (anki-core-parse-card-to-plist line decks) result)
           ;;     ))

           (cl-loop for line in lines collect
                    (unless (equal line "")            ; avoid empty line
                        (anki-core-parse-card-to-hash-table line decks models)))

           ))))

(defun anki-core-parse-card-to-plist (query-result decks)
  "Builds alist out of a full `anki-core-query' query record result.
Argument QUERY-RESULT is the query result generate by sqlite."
  (if query-result
      (let ((spl-query-result (split-string query-result anki-sql-separator)))
        `(:id              ,(anki-decode-milliseconds (nth 0 spl-query-result))
          :nid            ,(nth 1 spl-query-result)
          :did            ,(anki-core-decode-did (nth 2 spl-query-result) decks)
          :ord            ,(nth 3 spl-query-result)
          :mod            ,(anki-decode-seconds (nth 4 spl-query-result))
          :usn            ,(nth 5 spl-query-result)
          :type            ,(nth 6 spl-query-result)
          :queue            ,(nth 7 spl-query-result)
          :due            ,(nth 8 spl-query-result)
          :ivi            ,(nth 9 spl-query-result)
          :factor            ,(nth 10 spl-query-result)
          :reps            ,(nth 11 spl-query-result)
          :lapses            ,(nth 12 spl-query-result)
          :left            ,(nth 13 spl-query-result)
          :odue            ,(nth 14 spl-query-result)
          :odid            ,(nth 15 spl-query-result)
          :flags            ,(nth 16 spl-query-result)
          :data            ,(nth 17 spl-query-result)
          :id-1            ,(nth 18 spl-query-result)
          :guid            ,(nth 19 spl-query-result)
          :mid            ,(nth 20 spl-query-result)
          :mod-1            ,(nth 21 spl-query-result)
          :usn-1            ,(nth 22 spl-query-result)
          :tags            ,(nth 23 spl-query-result)
          :flds            ,(nth 24 spl-query-result)
          :sfld            ,(nth 25 spl-query-result)
          :csum            ,(nth 26 spl-query-result)
          :flags-1            ,(nth 27 spl-query-result)
          :data-1            ,(nth 28 spl-query-result)))))

(defun anki-core-parse-card-to-hash-table (query-result decks models)
  "Builds alist out of a full `anki-core-query' query record result.
Argument QUERY-RESULT is the query result generate by sqlite."
  (let ((card-hash-table (make-hash-table :test 'equal)))
    (if query-result
        (let ((spl-query-result (split-string query-result anki-sql-separator)))
          ;; (puthash 'id              (anki-decode-milliseconds (nth 0 spl-query-result)) card-hash-table )
          (puthash 'id              (nth 0 spl-query-result) card-hash-table )
          (puthash 'nid            (nth 1 spl-query-result) card-hash-table )
          (puthash 'did            (anki-core-decode-did (nth 2 spl-query-result) decks) card-hash-table)
          (puthash 'ord            (nth 3 spl-query-result) card-hash-table )
          ;; (puthash 'mod            (anki-decode-seconds (nth 4 spl-query-result)) card-hash-table )
          (puthash 'mod            (nth 4 spl-query-result) card-hash-table )
          (puthash 'usn            (nth 5 spl-query-result) card-hash-table )
          (puthash 'type            (nth 6 spl-query-result) card-hash-table )
          (puthash 'queue            (nth 7 spl-query-result) card-hash-table )
          (puthash 'due            (nth 8 spl-query-result) card-hash-table )
          (puthash 'ivi            (nth 9 spl-query-result) card-hash-table )
          (puthash 'factor            (nth 10 spl-query-result) card-hash-table )
          (puthash 'reps            (nth 11 spl-query-result) card-hash-table )
          (puthash 'lapses            (nth 12 spl-query-result) card-hash-table )
          (puthash 'left            (nth 13 spl-query-result) card-hash-table )
          (puthash 'odue            (nth 14 spl-query-result) card-hash-table )
          (puthash 'odid            (nth 15 spl-query-result) card-hash-table )
          (puthash 'flags            (nth 16 spl-query-result) card-hash-table )
          (puthash 'data            (nth 17 spl-query-result) card-hash-table )
          (puthash 'id-1            (nth 18 spl-query-result) card-hash-table )
          (puthash 'guid            (nth 19 spl-query-result) card-hash-table )
          (puthash 'mid            (anki-core-decode-mid (nth 20 spl-query-result) models) card-hash-table )
          (puthash 'mod-1            (nth 21 spl-query-result) card-hash-table )
          (puthash 'usn-1            (nth 22 spl-query-result) card-hash-table )
          (puthash 'tags            (nth 23 spl-query-result) card-hash-table )
          (puthash 'flds            (nth 24 spl-query-result) card-hash-table )
          (puthash 'sfld            (nth 25 spl-query-result) card-hash-table )
          (puthash 'csum            (nth 26 spl-query-result) card-hash-table )
          (puthash 'flags-1            (nth 27 spl-query-result) card-hash-table )
          (puthash 'data-1            (nth 28 spl-query-result) card-hash-table)))
    card-hash-table))

(defun anki-core-cards-list ()
  "Return all cards as a list of hashtables."
  (hash-table-values (anki-core-cards)))

(defun anki-core-cards ()
  "Return one hash table of all cards."
  (let ((cards (make-hash-table :test 'equal)))
    (cl-loop for card in (anki-core-parse-cards)
             if (hash-table-p card)
             do (progn
                  (puthash 'card-format (anki-core-format-card-hash-table card) card)
                  (puthash (gethash 'id card) card cards)))
    cards)

  ;; (let ((cards (anki-core-parse-cards)) result)
  ;;   (dolist (card cards result)
  ;;     ;; (push (list (anki-core-format-card-plist card) card) result)
  ;;     (push (list (anki-core-format-card-hash-table card) card) result)))
  )

(defun anki-core-format-card-plist (card)
  "Format one card ITEM."
  (let* ((flds (plist-get card :flds))
         (sfld (plist-get card :sfld)))
    (format "%s          %s" sfld (replace-regexp-in-string "\037" "\n" flds))))

(defun anki-core-format-card-hash-table (card)
  "Format one card ITEM."
  (if (hash-table-p card)
      (let* ((ord (gethash 'ord card))
             (sfld (gethash 'sfld card))
             (flds (gethash 'flds card))
             ;; (due (anki-core-decode-due card))
             (did (gethash 'did card))
             (deck-name (gethash "name" did))
             (mid (gethash 'mid card))
             (model-name (gethash "name" mid))
             (model-flds (gethash "flds" mid))
             (template (anki-core-decode-tmpls ord mid)))
        ;; (format "%s  %s" deck-name (replace-regexp-in-string "\037" "   " flds))
        (format "%s  %s  %s" deck-name (car template) sfld))))

(defun anki-core-decode-tmpls (cord mid)
  (let (result)
    (dolist (tmpls (gethash "tmpls" mid))
      (let ((ord (gethash "ord" tmpls))
            (name (gethash "name" tmpls))
            (afmt (gethash "afmt" tmpls))
            (qfmt (gethash "qfmt" tmpls)))
        (if (and (equal (number-to-string ord) cord) name qfmt afmt)
            (setq result (list name qfmt afmt)))))
    result))

(defun anki-core-decode-due (card)
  (let ((type (gethash 'type card))
        (due (gethash 'due card)))
    (cond ((equal type "0")             ; new
           (concat "New #" due))
          ((equal type "1")             ; learning
           (anki-decode-seconds due))
          ((equal type "2")             ; review
           (concat "Review #" due))
          ((equal type "3")             ; relearning
           (concat "Relearning: " due ))
          (t ""))))

(defun anki-core-decode-did (input decks)
  (if input
      (gethash input decks)))

(defun anki-core-decode-mid (input models)
  (if input
      (gethash input models)))

;;;###autoload
(defun anki-list-decks ()
  (interactive)
  (let* ((deck (let* ((pdecks (anki-core-parse-decks))
                      (vdecks (hash-table-values pdecks)))
                 (cl-loop for deck in vdecks collect
                          (cons (gethash "name" deck) (gethash "id" deck)))))
         (deck-name (setq anki-current-deck (completing-read "Decks: " deck)))
         (selected-did (cdr (assoc deck-name deck))))
    ;; Get all cards
    (if anki-search-entries
        anki-search-entries
      (progn
        (setq anki-search-entries (hash-table-values (anki-core-cards-list)))
        (setq anki-full-entries anki-search-entries)))
    ;; Filter cards
    (setq anki-search-entries
          (cl-loop for entry in anki-full-entries
                   when (hash-table-p entry)
                   for table-did = (gethash 'did entry)
                   if (hash-table-p table-did)
                   if (equal selected-did (gethash "id" table-did))
                   collect entry)))
  (cond ((eq major-mode 'anki-search-mode)
         (anki-browser))
        (t
         (anki 0))))

(defun anki-core-list-models ()
  (interactive)
  (completing-read "Models: "
                   (let* ((pmodels (anki-core-parse-models))
                          (vmodels (hash-table-values pmodels)))
                     (cl-loop for model in vmodels collect
                              (cons (gethash "name" model) (gethash "id" model))))))

(defun anki-core-write (file data)
  (with-temp-file file
    (prin1 data (current-buffer))))

(defun anki-core-read (file symbol)
  (when (boundp symbol)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (set symbol (read (current-buffer))))))

(provide 'anki-core)
