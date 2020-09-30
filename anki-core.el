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
(require 'emacsql)
(require 'emacsql-sqlite)

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
  "NOT USED anki database query review logs statement.")

(defvar anki-core-query-cards "
SELECT cards.*, notes.*
FROM cards
LEFT JOIN  notes
ON cards.nid = notes.id "
  "TODO anki database query cards statement.")

(defvar anki-core-query-decks "SELECT decks FROM col"
  "TODO anki database query decks statement.")
(defvar anki-core-query-models "SELECT models FROM col"
  "TODO anki database query models statement.")

(defcustom anki-sql-separator "\3"
  "SQL separator, used in parsing SQL result into list."
  :group 'anki
  :type 'string)

(defcustom anki-sql-newline "\2"
  "SQL newline, used in parsing SQL result into list."
  :group 'anki
  :type 'string)

(defcustom anki-collection-dir
  (cond
   ((eq system-type 'darwin)
    "~/Library/Application Support/Anki2/User 1")
   ((eq system-type 'gun/linux)
    "~/.local/share/Anki2/User 1")
   (t
    "~/AppData/Roaming/Anki2/User 1"))
  "Anki Collection Direcotry."
  :group 'anki
  :type 'string)

(defvar anki-core-current-deck ""
  "Current deck.")

(defvar anki-core-current-deck-id ""
  "Current deck id.")

(defvar anki-core-decks-hash-table ""
  "Parsed decks information.")

(defvar anki-core-models-hash-table ""
  "Parsed models information.")

(defvar anki-core-hash-table
  (make-hash-table :test 'equal))

(defconst anki-core-db-version 1)

(defcustom anki-core-database-file
  (expand-file-name "anki-database.sqlite"  anki-collection-dir)
  "The file used to store the anki.el database."
  :group 'anki
  :type 'file)

(defvar anki-core-db-connection nil
  "The EmacSQL database connection.")

(defun anki-core-db ()
  "Connect or create database."
  (unless (and anki-core-db-connection (emacsql-live-p anki-core-db-connection))
    (setq anki-core-db-connection (emacsql-sqlite (expand-file-name "anki-database.sqlite"  anki-collection-dir)))

    ;; create id table
    ;; (emacsql anki-core-db-connection [:create-table :if-not-exists id ([id])])

    ;; create revlog table
    (emacsql anki-core-db-connection [:create-table :if-not-exists revlog ([id did learn-data due-days due-date])])

    ;; create version table
    (emacsql anki-core-db-connection [:create-table :if-not-exists version ([user-version])])

    (let* ((db anki-core-db-connection)
           (version (anki-core-db-maybe-update db anki-core-db-version)))
      (cond
       ((> version anki-core-db-version)
        (emacsql-close db)
        (user-error
         "The anki database was created with a newer Anki version.  %s"
         "You need to update the Anki package."))
       ((< version anki-core-db-version)
        (emacsql-close db)
        (error "BUG: The Anki database scheme changed %s"
               "and there is no upgrade path")))))
  anki-core-db-connection)

(defun anki-core-db-maybe-update (db version)
  (if (emacsql-live-p db)
    (cond ((eq version 1)
           (anki-core-db-set-version db (setq version 1))
           (message "Anki database is version 1...done"))
          ((eq version 2)
           (message "Upgrading Anki database from version 2 to 3...")
           (anki-core-db-set-version db (setq version 3))
           (message "Upgrading Anki database from version 2 to 3...done"))
          (t (setq version anki-core-db-version))))
    version)

(defun anki-core-db-get-version (db)
  (caar (emacsql db [:select user-version :from version])))

(defun anki-core-db-set-version (db dbv)
  "Insert user-version if not exists."
  (cl-assert (integerp dbv))
  (if (anki-core-db-get-version db)
      (emacsql db `[:update version :set  (= user-version ,dbv)])
    (emacsql db `[:insert :into version :values ([(,@anki-core-db-version)])])))

(defun anki-core-sql (sql &rest args)
  (if (stringp sql)
      (emacsql (anki-core-db) (apply #'format sql args))
    (apply #'emacsql (anki-core-db) sql args)))

(defvar anki-core-database-index '()
  "Anki core database cards index.")

(defvar anki-core-database-review-logs '()
  "Anki core database cards review logs.")

(defun anki-core-query (sql-query)
  "Query collections.anki2 databse and return the result.
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
  "Parse decks and save to `anki-core-decks-hash-table'."
  (let ((decks (let* ((json-array-type 'list)
                      (json-object-type 'hash-table)
                      (json-false nil))
                 (json-read-from-string (anki-core-query anki-core-query-decks)))))
    (setq anki-core-decks-hash-table decks)
    decks))

(defun anki-core-parse-models ()
  "Parse models and save to `anki-core-models-hash-table'."
  (let ((models (let* ((json-array-type 'list)
                       (json-object-type 'hash-table)
                       (json-false nil))
                  (json-read-from-string (anki-core-query anki-core-query-models)))))
    (setq anki-core-models-hash-table models)
    models))

(defun anki-core-parse-cards ()
  "Parse cards."
  (let* ((query-result (anki-core-query anki-core-query-cards))
         (lines (if query-result (split-string query-result anki-sql-newline))))
    (anki-core-parse-models)            ; parse models
    (anki-core-parse-decks)             ; parse decks
    (cond ((equal "" query-result) '(""))
          (t
           ;; collect lines of sql result into hash tables
           (cl-loop for line in lines
                    if (not (equal line "" ))            ; avoid empty line
                    collect (anki-core-parse-card-to-hash-table line))))))

(defun anki-core-parse-card-to-hash-table (query-result)
  "Builds hash table out of a full `anki-core-query' query record result.
Argument QUERY-RESULT is the query result generate by sqlite."
  (let ((card-hash-table (make-hash-table :test 'equal)))
    (if query-result
        (let ((spl-query-result (split-string query-result anki-sql-separator)))
          (puthash 'id              (nth 0 spl-query-result) card-hash-table )
          (puthash 'nid            (nth 1 spl-query-result) card-hash-table )
          ;; (puthash 'did            (anki-core-get-deck (nth 2 spl-query-result) decks) card-hash-table)
          (puthash 'did            (nth 2 spl-query-result) card-hash-table)
          (puthash 'ord            (nth 3 spl-query-result) card-hash-table )
          ;; (puthash 'mod            (nth 4 spl-query-result) card-hash-table )
          ;; (puthash 'usn            (nth 5 spl-query-result) card-hash-table )
          ;; (puthash 'type            (nth 6 spl-query-result) card-hash-table )
          ;; (puthash 'queue            (nth 7 spl-query-result) card-hash-table )
          ;; (puthash 'due            (nth 8 spl-query-result) card-hash-table )
          ;; (puthash 'ivi            (nth 9 spl-query-result) card-hash-table )
          ;; (puthash 'factor            (nth 10 spl-query-result) card-hash-table )
          ;; (puthash 'reps            (nth 11 spl-query-result) card-hash-table )
          ;; (puthash 'lapses            (nth 12 spl-query-result) card-hash-table )
          ;; (puthash 'left            (nth 13 spl-query-result) card-hash-table )
          ;; (puthash 'odue            (nth 14 spl-query-result) card-hash-table )
          ;; (puthash 'odid            (nth 15 spl-query-result) card-hash-table )
          ;; (puthash 'flags            (nth 16 spl-query-result) card-hash-table )
          ;; (puthash 'data            (nth 17 spl-query-result) card-hash-table )
          ;; (puthash 'id-1            (nth 18 spl-query-result) card-hash-table )
          ;; (puthash 'guid            (nth 19 spl-query-result) card-hash-table )
          ;; (puthash 'mid            (anki-core-get-model (nth 20 spl-query-result) models) card-hash-table )
          (puthash 'mid            (nth 20 spl-query-result) card-hash-table )
          ;; (puthash 'mod-1            (nth 21 spl-query-result) card-hash-table )
          ;; (puthash 'usn-1            (nth 22 spl-query-result) card-hash-table )
          ;; (puthash 'tags            (nth 23 spl-query-result) card-hash-table )
          (puthash 'flds            (nth 24 spl-query-result) card-hash-table )
          (puthash 'sfld            (nth 25 spl-query-result) card-hash-table )
          ;; (puthash 'csum            (nth 26 spl-query-result) card-hash-table )
          ;; (puthash 'flags-1            (nth 27 spl-query-result) card-hash-table )
          ;; (puthash 'data-1            (nth 28 spl-query-result) card-hash-table)
          ))
    card-hash-table))

(defun anki-core-cards-list ()
  "Return all cards as a list of hash tables."
  (hash-table-values (anki-core-cards)))

(defun anki-core-cards ()
  "Return one hash table of all cards."
  (let ((cards (make-hash-table :test 'equal)))
    (cl-loop for card in (anki-core-parse-cards)
             if (hash-table-p card)
             do (let ((id (gethash 'id card)))
                  (puthash 'card-format (anki-core-format-card-hash-table card) card)
                  (push id anki-core-database-index)
                  (puthash id card cards)))
    (setq anki-core-hash-table cards)
    cards))

(defun anki-core-format-card-hash-table (card)
  "Format CARD to one string which used in *anki-search*."
  (if (hash-table-p card)
      (let* (;; (id (gethash 'id card))
             (ord (gethash 'ord card))
             (sfld (gethash 'sfld card))
             ;; (flds (gethash 'flds card))
             ;; (due (anki-core-decode-due card))
             (did (anki-core-get-deck (gethash 'did card)))
             (deck-name (gethash "name" did))
             (mid (anki-core-get-model (gethash 'mid card)))
             ;; (model-name (gethash "name" mid))
             ;; (model-flds (gethash "flds" mid))
             (template (anki-core-decode-tmpls ord mid))
             ;; (due-date (anki-learn-get-due-date id)); get due date take a lot of time, disable first
             )
        ;; (format "%s  %s" deck-name (replace-regexp-in-string "\037" "   " flds))
        ;; (format "%s  %s  %s  %s" deck-name (car template) sfld (or due-date ""))
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

(defun anki-core-get-deck (did)
  "Get Deck based on DID."
  (if did
      (gethash did anki-core-decks-hash-table)))

(defun anki-core-get-model (mid)
  "Get the model based on MID."
  (if mid
      (gethash mid anki-core-models-hash-table)))

(defun anki-core-list-models ()
  "List all models."
  (interactive)
  (completing-read "Models: "
                   (let* ((pmodels (anki-core-parse-models))
                          (vmodels (hash-table-values pmodels)))
                     (cl-loop for model in vmodels collect
                              (cons (gethash "name" model) (gethash "id" model))))))

(defun anki-core-write (file data)
  "NOT USED."
  (with-temp-file file
    (prin1 data (current-buffer))))

(defun anki-core-read (file symbol)
  "NOT USED."
  (when (boundp symbol)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (set symbol (read (current-buffer))))))

(defun anki-core-get-latest-due-card-or-random-card ()
  "Get latest due card or a random card if no review logs."
  (let* ((result (car (anki-core-sql `[:select [id did (funcall min due_days) due_date]
                                     :from [:select *
                                            :from [:select :distinct [id did due_days due_date] :from revlog :where (= did ,anki-core-current-deck-id) :order-by ROWID :desc]
                                            :group-by id]])))
        (id (nth 0 result))
        ;; (due-days (nth 1 result))
        (due-date (if id (encode-time (parse-time-string (nth 3 result))))))
    (if (and id (time-less-p due-date (current-time))) ; the lastest card due-date is less than current date
        (gethash id anki-core-hash-table)
      (nth (random (1- (length anki-search-entries))) anki-search-entries))))

(provide 'anki-core)
