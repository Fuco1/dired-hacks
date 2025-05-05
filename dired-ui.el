;;; dired-ui.el --- Additional or extended dired commands -*- lexical-binding: t -*-

;; Copyright (C) 2025 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Version: 0.0.1
;; Created: 5th May 2025
;; Package-requires: ((f "0.19.0"))
;; Keywords: files

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'dired)
(require 'f)

;;;###autoload
(defun dired-ui-dired-copy-filename-as-kill (&optional arg)
  "Copy name of file at point into the kill ring.

This function works the same as `dired-copy-filename-as-kill' but
adds the prefix option \\[universal-argument] to copy file name
up to the project root as determined by `project-root'.

The behaviour of \\[universal-argument] is changed from the
original function, where it used to copy up to the default
directory of the current buffer, which in case of inserted
subdirectories was the \"main\" directory."
  (interactive "P")
  (if (consp arg)
      (let* ((result (dired-copy-filename-as-kill 0))
             (root (project-root (project-current)))
             (relative-path (concat "./" (f-relative result root))))
        (if (eq last-command 'kill-region)
            (kill-append relative-path nil)
          (kill-new relative-path)
          (message "%s" relative-path)))
    (dired-copy-filename-as-kill arg)))


(provide 'dired-ui)
;;; dired-ui.el ends here
