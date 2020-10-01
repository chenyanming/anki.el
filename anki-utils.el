;;; anki-utils.el -*- lexical-binding: t; -*-

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

(defun anki-decode-milliseconds (input)
  ;; TODO: Decrease the decoding time.
  (if input
      (format-time-string "%Y-%m-%d %a %H:%M:%S"
                          (seconds-to-time
                           (/ (string-to-number input) 1000))) ))

(defun anki-decode-seconds (input)
  ;; TODO: Decrease the decoding time.
  (if input
      (format-time-string "%Y-%m-%d %a %H:%M:%S"
                          (seconds-to-time (string-to-number input))) ))


(defun anki-decode-days (input)
  ;; TODO
  )



(provide 'anki-utils)
