;;; anki/tests/anki-draft.el -*- lexical-binding: t; -*-



;; (time-less-p  (encode-time (parse-time-string (nth 2 (car (anki-core-sql [:select [id, (funcall min due_days), due_date]
;;                      :from [:select *
;;                             :from [:select :distinct [id due_days due_date] :from revlog :order-by ROWID :asc]
;;                             :group-by id]])) ) ) ) (current-time)  )

;; SELECT id, MIN(due_days), due_date FROM (SELECT * FROM (SELECT DISTINCT id, due_days, due_date from revlog ORDER BY ROWID DESC) GROUP BY id)
;; SELECT id, did, MIN(due_days), due_date FROM
;; (SELECT * FROM (SELECT DISTINCT id, did, due_days, due_date from revlog WHERE did = 1549898228880 ORDER BY ROWID DESC) GROUP BY id )

;; (defun anki-core-backup-database (&rest rest)
;;   "TODO: Backup the anki database to a text file, except media files."
;;   (interactive)
;;   (let* ((date (format-time-string "%Y-%m-%d"))
;;         (file (read-file-name "Save Anki Database to: " "~" (concat "Anki_Backup_" date ".txt") (pop rest))))
;;     (anki-core-write (shell-quote-argument (expand-file-name file)) (anki-core-cards))))

;; (defun anki-core-load-database ()
;;   "TODO: Load the anki database which is genereated by `anki-core-backup'."
;;   (interactive)
;;   (let* ((file (read-file-name "Load Anki Database: " "~" )))
;;     (anki-core-read (shell-quote-argument (expand-file-name file)) 'anki-core-database-index)
;;     (setq anki-full-entries (hash-table-values anki-core-database-index))
;;     (setq anki-search-entries (hash-table-values anki-core-database-index))))

;; (defun anki-core-backup-learn-data ()
;;   "TODO: Backup the anki learn data to a text file."
;;   (interactive)
;;   (anki-core-write (expand-file-name (concat (file-name-as-directory anki-collection-dir) "learn.txt")) anki-core-database-review-logs))

;; (defun anki-core-load-learn-data ()
;;   "TODO: Load the anki learn data."
;;   (interactive)
;;   (let ((file (expand-file-name (concat (file-name-as-directory anki-collection-dir) "learn.txt"))))
;;     (if (file-exists-p file)
;;         (anki-core-read file 'anki-core-database-review-logs))))

(defun anki-core-parse-card-to-plist (query-result decks)
  "Builds alist out of a full `anki-core-query' query record result.
Argument QUERY-RESULT is the query result generate by sqlite."
  (if query-result
      (let ((spl-query-result (split-string query-result anki-core-sql-separator)))
        `(:id              ,(anki-core-decode-milliseconds (nth 0 spl-query-result))
          :nid            ,(nth 1 spl-query-result)
          :did            ,(anki-core-get-deck (nth 2 spl-query-result) decks)
          :ord            ,(nth 3 spl-query-result)
          :mod            ,(anki-core-decode-seconds (nth 4 spl-query-result))
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


(defun anki-core-format-card-plist (card)
  "NOT USED: Format one card ITEM."
  (let* ((flds (plist-get card :flds))
         (sfld (plist-get card :sfld)))
    (format "%s          %s" sfld (replace-regexp-in-string "\037" "\n" flds))))
