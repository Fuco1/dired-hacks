;;; dired-avfs.el --- AVFS support for dired.

;; Copyright (C) 2014 Matus Goljer

;; Author: Matus Goljer <matus.goljer@gmail.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Keywords: lisp
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

;; Adds support for seamless browsing

;; See https://github.com/Fuco1/dired-hacks for the entire collection

;;; Code:

(require 'dash)

(defgroup dired-avfs ()
  "Dired-avfs"
  :group 'dired-hacks
  :prefix "dired-avfs-")

(defcustom dired-avfs-root "~/.avfs"
  "Root where the avfs virtual filesystem is mounted."
  :type 'directory
  :group 'dired-avfs)

(defcustom dired-avfs-archives '("zip" "rar" "tar")
  "Archives that are automagically opened via avfs."
  :type '(repeat string)
  :group 'dired-avfs)

(defcustom dired-avfs-hide-root t
  "If non-nil, hide the avfs root in dired listing."
  :type 'boolean
  :group 'dired-avfs)

(defcustom dired-avfs-ignore-commands nil
  "Do not open a file via avfs if it was opened using this command.

For example, this allows the user to open files via avfs from
dired, but not from `find-file'."
  :type '(repeat symbol)
  :group 'dired-avfs)

(defun dired-avfs--archive-filename (filename)
  (concat dired-avfs-root (file-truename filename) "#"))

(defun dired-avfs--archive-p (filename)
  (let ((extensions (concat "\\." (regexp-opt dired-avfs-archives) "\\'")))
    (string-match-p extensions filename)))

(defun dired-avfs--open (filename)
  (find-file (dired-avfs--archive-filename filename)))

(defun dired-avfs--hide-root ()
  (save-excursion
    (when dired-avfs-hide-root
      (goto-char (point-min))
      (when (search-forward (file-truename dired-avfs-root) nil t)
        (let ((inhibit-read-only t))
          (put-text-property (match-beginning 0) (match-end 0) 'invisible t))))))

(add-hook 'dired-after-readin-hook 'dired-avfs--hide-root)

(defun dired-avfs-open ()
  (interactive)
  (dired-avfs--open (dired-file-name-at-point)))

;; make this somehow work with other custom redefinitions of
;; `dired-find-file'
(defun dired-avfs-find-file ()
  "In Dired, visit the file or directory named on this line.

If point is on a file, behaves like `dired-file-file' but handles
archives via avfs.

If point is on a directory header, open a new dired for the
directory under point."
  (interactive)
  (let ((file (condition-case nil
                  (dired-get-file-for-visit)
                (error "Unable to visit this file")))
        (find-file-run-dired t))
    (cond
     ((dired-avfs--archive-p file)
      (dired-avfs--open file))
     ((dired-get-subdir) ;; this should not be here!!!
      (-when-let (end (save-excursion (re-search-forward "[/:]" (line-end-position) t)))
        (let ((path (buffer-substring-no-properties
                     (+ 2 (line-beginning-position))
                     (1- end))))
          (find-file path))))
     (t
      (find-file file)))))

(define-key dired-mode-map [remap dired-find-file] 'dired-avfs-find-file)

(defadvice find-file-noselect (before fix-avfs-arguments activate)
  "If the target is archive that can be handled via avfs,
automagically change the filename to the location of virtual
directory representing this archive."
  (when (and (not (memq this-command dired-avfs-ignore-commands))
             (dired-avfs--archive-p (ad-get-arg 0)))
    (ad-set-arg 0 (dired-avfs--archive-filename (ad-get-arg 0)))))

(provide 'dired-avfs)

;;; dired-avfs.el ends here
