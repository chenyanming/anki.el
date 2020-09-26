;;; anki-learn.el -*- lexical-binding: t; -*-

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
;;; Essentially copied from `org-learn.el', but modified to work on hash table.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(defvar initial-repetition-state '(-1 1 2.5 nil))

(defcustom anki-learn-always-reschedule t
  "If non-nil, always reschedule items, even if retention was \"perfect\"."
  :type 'boolean
  :group 'anki)

(defcustom anki-learn-fraction 0.5
  "Controls the rate at which EF is increased or decreased.
Must be a number between 0 and 1 (the greater it is the faster
the changes of the OF matrix)."
  :type 'float
  :group 'anki)

(defcustom anki-learn-sm5-initial-interval
  0.1
  "In the SM5 algorithm, the initial interval after the first
successful presentation of an item is always 4 days. If you wish to change
this, you can do so here."
  :group 'anki
  :type 'float)


(defun initial-optimal-factor (n ef)
  (if (= 1 n)
      anki-learn-sm5-initial-interval
    ef))

(defun get-optimal-factor (n ef of-matrix)
  (let ((factors (assoc n of-matrix)))
    (or (and factors
             (let ((ef-of (assoc ef (cdr factors))))
               (and ef-of (cdr ef-of))))
        (initial-optimal-factor n ef))))

(defun set-optimal-factor (n ef of-matrix of)
  (let ((factors (assoc n of-matrix)))
    (if factors
        (let ((ef-of (assoc ef (cdr factors))))
          (if ef-of
              (setcdr ef-of of)
            (push (cons ef of) (cdr factors))))
      (push (cons n (list (cons ef of))) of-matrix)))
  of-matrix)

(defun inter-repetition-interval (n ef &optional of-matrix)
  (let ((of (get-optimal-factor n ef of-matrix)))
    (if (= 1 n)
        of
      (* of (inter-repetition-interval (1- n) ef of-matrix)))))

(defun modify-e-factor (ef quality)
  "EF: Efactor, QUALITY: 0-5.
5. After each repetition modify the E-Factor of the recently repeated item according to the formula:
EF':=EF+(0.1-(5-q)*(0.08+(5-q)*0.02))
where:
EF' - new value of the E-Factor,
EF - old value of the E-Factor,
q - quality of the response in the 0-5 grade scale.
If EF is less than 1.3 then let EF be 1.3."
  (if (< ef 1.3)
      1.3
    (+ ef (- 0.1 (* (- 5 quality) (+ 0.08 (* (- 5 quality) 0.02)))))))

(defun modify-of (of q fraction)
  (let ((temp (* of (+ 0.72 (* q 0.07)))))
    (+ (* (- 1 fraction) of) (* fraction temp))))


(defun determine-next-interval (n ef quality of-matrix)
  (assert (> n 0))
  (assert (and (>= quality 0) (<= quality 5)))
  (if (< quality 3)
      (list (inter-repetition-interval n ef) (1+ n) ef nil)
    (let ((next-ef (modify-e-factor ef quality)))
      (setq of-matrix
            (set-optimal-factor n next-ef of-matrix
                                (modify-of (get-optimal-factor n ef of-matrix)
                                           quality anki-learn-fraction))
            ef next-ef)
      ;; For a zero-based quality of 4 or 5, don't repeat
      (if (and (>= quality 4)
               (not anki-learn-always-reschedule))
          (list 0 (1+ n) ef of-matrix)
        (list (inter-repetition-interval n ef of-matrix) (1+ n)
              ef of-matrix)))))


;; (defun org-smart-reschedule (quality)
;;   (interactive "nHow well did you remember the information (on a scale of 0-5)? ")
;;   (let* ((learn-str (org-entry-get (point) "LEARN_DATA"))
;; 	 (learn-data (or (and learn-str
;; 			      (read learn-str))
;; 			 (copy-list initial-repetition-state)))
;; 	 closed-dates)
;;     (setq learn-data
;; 	  (determine-next-interval (nth 1 learn-data)
;; 				   (nth 2 learn-data)
;; 				   quality
;; 				   (nth 3 learn-data)))
;;     (org-entry-put (point) "LEARN_DATA" (prin1-to-string learn-data))
;;     (if (= 0 (nth 0 learn-data))
;; 	(org-schedule t)
;;       (org-schedule nil (time-add (current-time)
;; 				  (days-to-time (nth 0 learn-data)))))))

(defun anki-learn-smart-reschedule (quality)
  "TODO: "
  (interactive "nHow well did you remember the information (on a scale of 0-5)? ")
  (let* ((id (anki-find-card-id-at-point))
         (learn-index (anki-learn-get-card id))
         (learn-data (anki-learn-get-learn-data id))
         due-date)
    ;; next interval - learn data
    (setq learn-data
          (determine-next-interval-sm2 (nth 0 learn-data)
                                       (nth 1 learn-data)
                                       (nth 2 learn-data)
                                       quality
                                       nil))
    ;; cal due date
    (setq due-date (format-time-string "%Y-%m-%d %H:%M:%S" (time-add (current-time) (days-to-time (nth 0 learn-data)))))

    ;; insert review log
    (anki-core-sql `[:insert :into revlog :values([,id ,learn-data ,due-date])])

    ;; (let ((learn-entry (assoc id anki-core-database-review-logs)))
    ;;   (if learn-entry
    ;;       (setf (cdr learn-entry) learn-data) ; if entry exists, only set learn data
    ;;     (push (cons id learn-data) anki-core-database-review-logs))) ; if entry miss, push entry + learn data
    ;; (anki-core-backup-learn-data)                                    ; backup learn data

    (message due-date)))

(defun anki-learn-mock-smart-reschedule (&optional id)
  "TODO: Get mock due date for all quality based on ID."
  (let* ((id (or id (anki-find-card-id-at-point)))
         (learn-data (anki-learn-get-learn-data id))
         next-learn-data)
    ;; next interval - learn data
    (cl-loop for quality in '(0 1 2 3 4 5)
             if (setq next-learn-data
                      (determine-next-interval-sm2 (nth 0 learn-data)
                                                   (nth 1 learn-data)
                                                   (nth 2 learn-data)
                                                   quality
                                                   nil) )
             collect (format "%s" (let ((days (nth 0 next-learn-data)))
                                         (cond ((< days 0) "<1 min")
                                               (t (format "%d d" days))))))))

;;; SM2 Algorithm =============================================================

(defcustom anki-learn-add-random-noise-to-intervals-p
  nil
  "If true, the number of days until an item's next repetition
will vary slightly from the interval calculated by the SM2
algorithm. The variation is very small when the interval is
small, but scales up with the interval."
  :group 'anki
  :type 'boolean)

(defcustom anki-learn-failure-quality
  2
  "Lower bound for an recall to be marked as failure.

If the quality of recall for an item is this number or lower,
it is regarded as an unambiguous failure, and the repetition
interval for the card is reset to 0 days.  If the quality is higher
than this number, it is regarded as successfully recalled, but the
time interval to the next repetition will be lowered if the quality
was near to a fail.

By default this is 2, for SuperMemo-like behaviour.  For
Mnemosyne-like behaviour, set it to 1.  Other values are not
really sensible."
  :group 'anki
  :type '(choice (const 2) (const 1)))



(defun anki-learn-random-dispersal-factor ()
  "Returns a random number between 0.5 and 1.5.

This returns a strange random number distribution. See
http://www.supermemo.com/english/ol/sm5.htm for details."
  (let ((a 0.047)
        (b 0.092)
        (p (- (cl-random 1.0) 0.5)))
    (cl-flet ((sign (n)
                    (cond ((zerop n) 0)
                          ((cl-plusp n) 1)
                          (t -1))))
      (/ (+ 100 (* (* (/ -1 b) (log (- 1 (* (/ b a ) (abs p)))))
                   (sign p)))
         100.0))))

(defun determine-next-interval-sm2 (last-interval n ef quality of-matrix)
  "TODO: Arguments:
- LAST-INTERVAL -- the number of days since the item was last reviewed.
- REPEATS -- the number of times the item has been successfully reviewed
- EF -- the 'easiness factor'
- QUALITY -- 0 to 5
4. After each repetition assess the quality of repetition response in 0-5 grade scale:
5 - perfect response
4 - correct response after a hesitation
3 - correct response recalled with serious difficulty
2 - incorrect response; where the correct one seemed easy to recall
1 - incorrect response; the correct one remembered
0 - complete blackout.

Returns a list: (INTERVAL REPEATS EF FAILURES MEAN TOTAL-REPEATS OFMATRIX), where:
- INTERVAL is the number of days until the item should next be reviewed
- REPEATS is incremented by 1.
- EF is modified based on the recall quality for the item.
- OF-MATRIX is not modified."
  (if (zerop n) (setq n 1))
  ;; 2. With all items associate an E-Factor equal to 2.5.
  (if (not ef) (setq ef 2.5))
  (cl-assert (> n 0))
  (cl-assert (and (>= quality 0) (<= quality 5)))
  (if (<= quality anki-learn-failure-quality)
      ;; 6. If the quality response was lower than 3 then start
      ;; repetitions for the item from the beginning without changing the
      ;; E-Factor (i.e. use intervals I(1), I(2) etc. as if the item was
      ;; memorized anew).
      (list -1 1 ef of-matrix)
    (let* ((next-ef (modify-e-factor ef quality))
           ;;3. Repeat items using the following intervals:
           ;; I(1):=1
           ;; I(2):=6
           ;; for n>2: I(n):=I(n-1)*EF
           ;; where:
           ;; I(n) - inter-repetition interval after the n-th repetition (in days),
           ;; EF - E-Factor of a given item
           (interval
            (cond
             ((<= n 1) 1)
             ((= n 2) 6)
             (t (* last-interval next-ef)))))
      (list interval
            (1+ n)
            next-ef
            of-matrix))))

(defun anki-find-card-id-at-point ()
  "TODO: "
  (let ((card (anki-find-card-at-point)))
    (if (hash-table-p card)
        (gethash 'id card))))

(defun anki-learn-get-card (id)
  "TODO: Get card based on card id."
  (rassoc id anki-core-database-index))

(defun anki-learn-get-learn-data (id)
  "TODO: Get learn data base on max due date of the ID."
  (let ((due-data (nth 2 (car (anki-core-sql `[:select [ROWID *] :from revlog :where id :like ,(concat "%%" id "%%") :order-by ROWID :desc :limit 1])))))
    (if due-data
        due-data
      (copy-list initial-repetition-state))))

(defun anki-learn-get-due-date (id)
  "TODO: Get due date based on card ID."
  (nth 3 (car (anki-core-sql `[:select [ROWID *] :from revlog :where id :like ,(concat "%%" id "%%") :order-by ROWID :desc :limit 1]))))

(provide 'anki-learn)

;;; anki-learn.el ends here
