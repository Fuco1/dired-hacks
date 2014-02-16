;;; dired-hacks-utils.el --- Utilities and helpers for dired-hacks collection.

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


(provide 'dired-hacks-utils)

;;; dired-hacks-utils.el ends here
