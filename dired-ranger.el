;;; dired-ranger.el --- Implementation of useful ranger features for dired

;; Copyright (C) 2014 Matúš Goljer <matus.goljer@gmail.com>

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Version: 0.0.1
;; Created: 17th June 2014
;; Package-requires: ((dash "2.7.0") (dired-hacks-utils "0.0.1"))
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

;; TODO: add commentary

;;; Code:

(require 'dired-hacks-utils)
(require 'dash)
(require 'ring)

(defgroup dired-ranger ()
  "Implementation of useful ranger features for dired."
  :group 'dired-hacks
  :prefix "dired-ranger-")


;; multi-stage copy/paste operations
(defcustom dired-ranger-copy-ring-size 10
  "Specifies how many filesets for copy/paste operations should be stored."
  :type 'integer
  :group 'dired-ranger)

(defvar dired-ranger-copy-ring (make-ring dired-ranger-copy-ring-size))

(defun dired-ranger-copy (arg)
  "Place the marked items in the copy ring.

With non-nil prefix argument, add the marked items to the current
selection.  This allows you to gather files from multiple dired
buffers for a single paste."
  (interactive "P")
  (let ((marked (dired-get-marked-files)))
    (if (or (not arg)
            (ring-empty-p dired-ranger-copy-ring))
        (progn
          (ring-insert
           dired-ranger-copy-ring
           (cons (list (current-buffer)) marked))
          ;; TODO: abstract the message/plural detection somewhere
          ;; (e.g. give it a verb and number to produce the correct
          ;; string.)
          (message (format "Copied %d item%s into copy ring."
                           (length marked)
                           (if (> (length marked) 1) "s" ""))))
      (let ((current (ring-remove dired-ranger-copy-ring 0)))
        (ring-insert
         dired-ranger-copy-ring
         (cons (cons (current-buffer) (car current))
               (-concat (dired-get-marked-files) (cdr current))))
        (message (format "Added %d item%s into copy ring."
                         (length marked)
                         (if (> (length marked) 1) "s" "")))))))

(defun dired-ranger-revert-target (char files)
  "Revert the target buffer and mark the new files."
  (let ((current-file (dired-utils-get-filename)))
    (revert-buffer)
    (let ((dired-marker-char char))
      (--each (-map 'file-name-nondirectory files)
        (dired-utils-goto-line (concat target-directory it))
        (dired-mark 1)))
    (dired-utils-goto-line current-file)))

(defun dired-ranger-paste (arg)
  "Copy the items from copy ring to current directory.

With raw prefix argument \\[universal-argument], do not remove
the selection from the stack so it can be copied again.

With numeric prefix argument, copy the n-th selection from the
copy ring."
  (interactive "P")
  (let* ((index (if (numberp arg) arg 0))
         (data (ring-ref dired-ranger-copy-ring index))
         (files (cdr data))
         (target-directory (dired-current-directory))
         (copied-files 0))
    (--each files (when (file-exists-p it)
                    (copy-file it target-directory 0)
                    (cl-incf copied-files)))
    ;; TODO: abstract the revert/mark code, it is used for copy and
    ;; paste, and I can see bunch of other uses
    (dired-ranger-revert-target ?P files)
    (unless arg (ring-remove dired-ranger-copy-ring 0))
    (message (format "Pasted %d/%d item%s from copy ring."
                     copied-files
                     (length files)
                     (if (> (length files) 1) "s" "")))))

(defun dired-ranger-move (arg)
  "Move the items from copy ring to current directory.

This behaves like `dired-ranger-paste' but moves the files
instead of copying them."
  (interactive "P")
  (let* ((index (if (numberp arg) arg 0))
         (data (ring-ref dired-ranger-copy-ring index))
         (buffers (car data))
         (files (cdr data))
         (target-directory (dired-current-directory))
         (copied-files 0))
    (--each files (when (file-exists-p it)
                    (rename-file it target-directory 0)
                    (cl-incf copied-files)))
    (dired-ranger-revert-target ?M files)
    (--each buffers
      (when (buffer-live-p it)
        (with-current-buffer it (revert-buffer))))
    (unless arg (ring-remove dired-ranger-copy-ring 0))
    (message (format "Moved %d/%d item%s from copy ring."
                     copied-files
                     (length files)
                     (if (> (length files) 1) "s" "")))))

(provide 'dired-ranger)
;;; dired-ranger.el ends here
