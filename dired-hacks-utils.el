;;; dired-hacks-utils.el --- Utilities and helpers for dired-hacks collection

;; Copyright (C) 2014 Matus Goljer

;; Author: Matus Goljer <matus.goljer@gmail.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Keywords: files
;; Version: 0.0.1
;; Created: 14th February 2014
;; Package-requires: ((dash "2.5.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Utilities and helpers for `dired-hacks' collection of dired
;; improvements.

;; See https://github.com/Fuco1/dired-hacks for the entire collection

;;; Code:

(require 'dash)
(require 'dired)

(defgroup dired-hacks ()
  "Collection of useful dired additions."
  :group 'dired
  :prefix "dired-hacks-")

(defun dired-utils-goto-line (filename)
  "Go to line describing FILENAME in listing.

Should be absolute file name matched against
`dired-get-filename'."
  (goto-char (point-min))
  (let (stop)
    (while (and (not stop)
                (= (forward-line) 0))
      (when (equal filename (ignore-errors (dired-get-filename)))
        (setq stop t)
        (dired-move-to-filename)))))

(defun dired-utils-match-filename-regexp (filename alist)
  "Match FILENAME against each car in ALIST and return first matched cons.

Each car in ALIST is a regular expression.

The matching is done using `string-match-p'."
  (let (match)
    (--each-while alist (not match)
      (when (string-match-p (car it) filename)
        (setq match it)))
    match))

(defun dired-utils-match-filename-extension (filename alist)
  "Match FILENAME against each car in ALIST and return first matched cons.

Each car in ALIST is a string representing file extension
*without* the delimiting dot."
  (let (done)
    (--each-while alist (not done)
      (when (string-match-p (concat "\\." (regexp-quote (car it)) "\\'") file)
        (setq done it)))
    done))


;;; Predicates
(defun dired-utils-is-file-p ()
  "Return non-nil if the line at point is a file or a directory."
  (ignore-errors (dired-get-filename 'no-dir)))

(defun dired-utils-is-dir-p ()
  "Return non-nil if the line at point is a directory."
  (ignore-errors
    (--when-let (dired-get-filename 'no-dir)
      (file-directory-p it))))


;;; Interactive
;; TODO: add wrap-around option
(defun dired-hacks-next-file (&optional arg)
  "Move point to the next file.

Optional prefix ARG says how many lines to move; default is one
line."
  (interactive "p")
  (unless arg (setq arg 1))
  (if (< arg 0)
      (dired-hacks-previous-file (- arg))
    (--dotimes arg
      (forward-line)
      (while (and (not (dired-utils-is-file-p))
                  (= (forward-line) 0)))
      (when (= (point) (point-max))
        (forward-line -1)))
    (dired-move-to-filename)))

(defun dired-hacks-previous-file (&optional arg)
  "Move point to the previous file.

Optional prefix ARG says how many lines to move; default is one
line."
  (interactive "p")
  (unless arg (setq arg 1))
  (if (< arg 0)
      (dired-hacks-next-file (- arg))
    (--dotimes arg
      (forward-line -1)
      (while (and (not (dired-utils-is-file-p))
                  (= (forward-line -1) 0)))
      (when (= (point) (point-min))
        (dired-hacks-next-file)))
    (dired-move-to-filename)))

(provide 'dired-hacks-utils)

;;; dired-hacks-utils.el ends here
