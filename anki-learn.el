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
  (require 'cl))

(defcustom anki-learn-always-reschedule nil
  "If non-nil, always reschedule items, even if retention was \"perfect\"."
  :type 'boolean
  :group 'anki)

(defcustom anki-learn-fraction 0.5
  "Controls the rate at which EF is increased or decreased.
Must be a number between 0 and 1 (the greater it is the faster
the changes of the OF matrix)."
  :type 'float
  :group 'anki)

(defun initial-optimal-factor (n ef)
  (if (= 1 n)
      4
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
  (if (< ef 1.3)
      1.3
    (+ ef (- 0.1 (* (- 5 quality) (+ 0.08 (* (- 5 quality) 0.02)))))))

(defun modify-of (of q fraction)
  (let ((temp (* of (+ 0.72 (* q 0.07)))))
    (+ (* (- 1 fraction) of) (* fraction temp))))

(defun calculate-new-optimal-factor (interval-used quality used-of
                                                   old-of fraction)
  "This implements the SM-5 learning algorithm in Lisp.
INTERVAL-USED is the last interval used for the item in question.
QUALITY is the quality of the repetition response.
USED-OF is the optimal factor used in calculation of the last
interval used for the item in question.
OLD-OF is the previous value of the OF entry corresponding to the
relevant repetition number and the E-Factor of the item.
FRACTION is a number belonging to the range (0,1) determining the
rate of modifications (the greater it is the faster the changes
of the OF matrix).

Returns the newly calculated value of the considered entry of the
OF matrix."
  (let (;; the value proposed for the modifier in case of q=5
        (mod5 (/ (1+ interval-used) interval-used))
        ;; the value proposed for the modifier in case of q=2
        (mod2 (/ (1- interval-used) interval-used))
        ;; the number determining how many times the OF value will
        ;; increase or decrease
        modifier)
    (if (< mod5 1.05)
        (setq mod5 1.05))
    (if (< mod2 0.75)
        (setq mod5 0.75))
    (if (> quality 4)
        (setq modifier (1+ (* (- mod5 1) (- quality 4))))
      (setq modifier (- 1 (* (/ (- 1 mod2) 2) (- 4 quality)))))
    (if (< modifier 0.05)
        (setq modifier 0.05))
    (setq new-of (* used-of modifier))
    (if (> quality 4)
        (if (< new-of old-of)
            (setq new-of old-of)))
    (if (< quality 4)
        (if (> new-of old-of)
            (setq new-of old-of)))
    (setq new-of (+ (* new-of fraction) (* old-of (- 1 fraction))))
    (if (< new-of 1.2)
        (setq new-of 1.2)
      new-of)))

(defvar initial-repetition-state '(-1 1 2.5 nil))

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
  (let* ((id (or (anki-find-card-id-at-point) "1306463892572"))
         (learn-hash (anki-learn-get-card id))
         (learn-str (anki-learn-get-due-data id))
         (learn-data (or learn-str
                         (copy-list initial-repetition-state)))
         closed-dates)
    (setq learn-data
          (determine-next-interval (nth 1 learn-data)
                                   (nth 2 learn-data)
                                   quality
                                   (nth 3 learn-data)))
    (puthash 'learn-data learn-data learn-hash)
    (message (format-time-string "%Y-%m-%d %a %H:%M:%S" (time-add (current-time)
                                                                  (days-to-time (nth 0 learn-data)))))))

(defun anki-find-card-id-at-point ()
  "TODO: "
  (let ((card (anki-find-card-at-point)) )
    (if (hash-table-p card)
        (gethash 'id card))))

(defun anki-learn-get-card (id)
  "TODO: "
  (gethash id anki-core-database))


(defun anki-learn-get-due-data (id)
  "TODO: "
  (gethash 'learn-data (gethash id anki-core-database)))

(defun anki-learn-get-due-date (id)
  "TODO: "
  (let* ((learn-str (anki-learn-get-due-data id))
         (learn-data (or learn-str
                         (copy-list initial-repetition-state))))
    (message (format-time-string "%Y-%m-%d %a %H:%M:%S" (time-add (current-time)
                                                                  (days-to-time (nth 0 learn-data)))))))

(provide 'anki-learn)

;;; anki-learn.el ends here
