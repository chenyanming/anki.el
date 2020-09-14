;;; anki/anki-core.el -*- lexical-binding: t; -*-
(require 'shr)
(require 'json)

(defvar anki-query-revlog "
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

(defvar anki-query-cards "
SELECT cards.*, notes.*
FROM cards
LEFT JOIN  notes
ON cards.nid = notes.id
"


  "TODO anki database query statement.")

(defvar anki-query-decks "SELECT decks FROM col")
(defvar anki-query-models "SELECT models FROM col")

(defcustom anki-sql-separator "\3"
  "SQL separator, used in parsing SQL result into list."
  :group 'anki
  :type 'string)

(defcustom anki-sql-newline "\2"
  "SQL newline, used in parsing SQL result into list."
  :group 'anki
  :type 'string)

(defcustom anki-collection-dir "/Users/damonchan/Library/Application Support/Anki2/Android & Mac"
  "SQL separator, used in parsing SQL result into list."
  :group 'anki
  :type 'string)

(defun anki-query (sql-query)
  "Query calibre databse and return the result.
Argument SQL-QUERY is the sqlite sql query string."
  (interactive)
  (if (file-exists-p (concat (file-name-as-directory anki-collection-dir) "collection.anki2"))
      (let ((cmd (format "%s -separator %s -newline %s -list -nullvalue \"\" -noheader %s \"%s\""
                         sql-sqlite-program
                         anki-sql-separator
                         anki-sql-newline
                         (shell-quote-argument (expand-file-name (concat (file-name-as-directory anki-collection-dir) "collection.anki2")))
                         sql-query)))
        (shell-command-to-string cmd)) nil))

(setq sql-sqlite-program "/usr/bin/sqlite3")

;; (pp-eval-expression
;;  '(anki-parse-decks))

(defun anki-parse-decks ()
  (let ((decks (let* ((json-array-type 'list) ;;;;;;;;; 'vector is the default
                      (json-object-type 'hash-table)
                      (json-false nil))
                 (json-read-from-string (anki-query anki-query-decks))))
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

(defun anki-parse-models ()
  (let ((models (let* ((json-array-type 'list) ;;;;;;;;; 'vector is the default
                      (json-object-type 'hash-table)
                      (json-false nil))
                 (json-read-from-string (anki-query anki-query-models))))) models))

(defun anki-parse-cards ()
  (let* ((query-result (anki-query anki-query-cards))
         (lines (if query-result (split-string query-result anki-sql-newline)))
         (decks (anki-parse-decks))
         (models (anki-parse-models)))
    (cond ((equal "" query-result) '(""))
          (t
           ;; (let (result)
           ;;   (dolist (line lines result)
           ;;     ;; decode and push to result
           ;;     (push (anki-parse-card-to-hash-table line decks) result)
           ;;     ;; (push (anki-parse-card-to-plist line decks) result)
           ;;     ))

           (cl-loop for line in lines collect
                    (unless (equal line "")            ; avoid empty line
                        (anki-parse-card-to-hash-table line decks models)))

           ))))

(defun anki-parse-card-to-plist (query-result decks)
  "Builds alist out of a full `anki-query' query record result.
Argument QUERY-RESULT is the query result generate by sqlite."
  (if query-result
      (let ((spl-query-result (split-string query-result anki-sql-separator)))
        `(:id              ,(anki-decode-milliseconds (nth 0 spl-query-result))
          :nid            ,(nth 1 spl-query-result)
          :did            ,(anki-decode-did (nth 2 spl-query-result) decks)
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

(defun anki-parse-card-to-hash-table (query-result decks models)
  "Builds alist out of a full `anki-query' query record result.
Argument QUERY-RESULT is the query result generate by sqlite."
  (let ((card-hash-table (make-hash-table :test 'equal)))
    (if query-result
        (let ((spl-query-result (split-string query-result anki-sql-separator)))
          ;; (puthash 'id              (anki-decode-milliseconds (nth 0 spl-query-result)) card-hash-table )
          (puthash 'id              (nth 0 spl-query-result) card-hash-table )
          (puthash 'nid            (nth 1 spl-query-result) card-hash-table )
          (puthash 'did            (anki-decode-did (nth 2 spl-query-result) decks) card-hash-table)
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
          (puthash 'mid            (anki-decode-mid (nth 20 spl-query-result) models) card-hash-table )
          (puthash 'mod-1            (nth 21 spl-query-result) card-hash-table )
          (puthash 'usn-1            (nth 22 spl-query-result) card-hash-table )
          (puthash 'tags            (nth 23 spl-query-result) card-hash-table )
          (puthash 'flds            (nth 24 spl-query-result) card-hash-table )
          (puthash 'sfld            (nth 25 spl-query-result) card-hash-table )
          (puthash 'csum            (nth 26 spl-query-result) card-hash-table )
          (puthash 'flags-1            (nth 27 spl-query-result) card-hash-table )
          (puthash 'data-1            (nth 28 spl-query-result) card-hash-table)))
    card-hash-table))

(defun anki-format-cards ()
  "Format all cards."
  (cl-loop for card in (anki-parse-cards) collect
           ;; (hash-table-p card)
           (if (hash-table-p card)
               (let ((table card))
                 (puthash 'card-format (anki-format-card-hash-table card) card)
                 table)))
  ;; (let ((cards (anki-parse-cards)) result)
  ;;   (dolist (card cards result)
  ;;     ;; (push (list (anki-format-card-plist card) card) result)
  ;;     (push (list (anki-format-card-hash-table card) card) result)))
  )

(defun anki-format-card-plist (card)
  "Format one card ITEM."
  (let* ((flds (plist-get card :flds))
         (sfld (plist-get card :sfld)))
    (format "%s          %s" sfld (replace-regexp-in-string "\037" "\n" flds))))

(defun anki-format-card-hash-table (card)
  "Format one card ITEM."
  (if (hash-table-p card)
      (let* ((sfld (gethash 'sfld card))
             (flds (gethash 'flds card))
             (did (gethash 'did card))
             (deck-name (gethash "name" did))
             (mid (gethash 'mid card))
             (model-name (gethash "name" mid))
             (model-flds (gethash "flds" mid)))
        ;; (format "%s  %s" deck-name (replace-regexp-in-string "\037" "   " flds))
        (format "%s  %s  %s" deck-name model-name sfld))))

(defun anki-decode-did (input decks)
  (if input
      (gethash input decks)))

(defun anki-decode-mid (input models)
  (if input
      (gethash input models)))

(defun anki-list-decks ()
  (interactive)
  (let* ((deck (let* ((pdecks (anki-parse-decks))
                      (vdecks (hash-table-values pdecks)))
                 (cl-loop for deck in vdecks collect
                          (cons (gethash "name" deck) (gethash "id" deck)))))
         (selected-did (cdr (assoc (completing-read "Decks: " deck) deck))))
    (setq anki-search-entries (cl-loop for entry in anki-full-entries collect
             (if (hash-table-p entry)
                 (let ((table-did (gethash 'did entry)))
                   (if (hash-table-p table-did)
                       (if (equal selected-did (gethash "id" table-did))
                           entry))))) ))
  (anki-browser))

(defun anki-list-models ()
  (interactive)
  (completing-read "Models: "
                   (let* ((pmodels (anki-parse-models))
                          (vmodels (hash-table-values pmodels)))
                     (cl-loop for model in vmodels collect
                              (cons (gethash "name" model) (gethash "id" model))))))

(provide 'anki-core)
