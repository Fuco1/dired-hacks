;;; dired-hacks-utils.el --- Utilities and helpers for dired-hacks collection

;; Copyright (C) 2014-2015 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
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

;; This package also provides these interactive functions:
;; * `dired-hacks-next-file' - go to next file, skipping empty and non-file lines
;; * `dired-hacks-previous-file' - go to previous file, skipping empty
;;   and non-file lines

;; See https://github.com/Fuco1/dired-hacks for the entire collection

;;; Code:

(require 'dash)
(require 'dired)

(defgroup dired-hacks ()
  "Collection of useful dired additions."
  :group 'dired
  :prefix "dired-hacks-")

(defun dired-utils-get-filename (&optional localp)
  "Like `dired-get-filename' but never signal an error.

Optional arg LOCALP with value `no-dir' means don't include
directory name in result."
  (dired-get-filename localp t))

(defun dired-utils-get-all-files (&optional localp)
  "Return all files in this dired buffer as a list.

LOCALP has same semantics as in `dired-get-filename'."
  (save-excursion
    (goto-char (point-min))
    (let (r)
      (while (= 0 (forward-line))
        (--when-let (dired-utils-get-filename localp)
          (push it r)))
      (nreverse r))))

(defconst dired-utils-file-attributes-keywords
  '(:isdir :nlinks :uid :gid :atime :mtime :ctime :size :modes :gidchg :inode :devnum)
  "List of keywords to map with `file-attributes'.")

(defconst dired-utils-info-keywords
  `(:name :issym :target ,@dired-utils-file-attributes-keywords)
  "List of keywords available for `dired-utils-get-info'.")

(defun dired-utils--get-keyword-info (keyword)
  "Get file information about KEYWORD."
  (let ((filename (dired-utils-get-filename)))
    (cl-case keyword
      (:name filename)
      (:isdir (file-directory-p filename))
      (:issym (and (file-symlink-p filename) t))
      (:target (file-symlink-p filename))
      (t
       (nth (-elem-index keyword dired-utils-file-attributes-keywords)
            (file-attributes filename))))))

(defun dired-utils-get-info (&rest keywords)
  "Query for info about the file at point.

KEYWORDS is a list of attributes to query.

When querying for one attribute, its value is returned.  When
querying for more than one, a list of results is returned.

The available keywords are listed in
`dired-utils-info-keywords'."
  (let ((attributes (mapcar 'dired-utils--get-keyword-info keywords)))
    (if (> (length attributes) 1)
        attributes
      (car attributes))))

(defun dired-utils-goto-line (filename)
  "Go to line describing FILENAME in listing.

Should be absolute file name matched against
`dired-get-filename'."
  (goto-char (point-min))
  (let (stop)
    (while (and (not stop)
                (= (forward-line) 0))
      (when (equal filename (dired-utils-get-filename))
        (setq stop t)
        (dired-move-to-filename)))
    stop))

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
      (when (string-match-p (concat "\\." (regexp-quote (car it)) "\\'") filename)
        (setq done it)))
    done))


;;; Predicates
(defun dired-utils-is-file-p ()
  "Return non-nil if the line at point is a file or a directory."
  (dired-utils-get-filename 'no-dir))

(defun dired-utils-is-dir-p ()
  "Return non-nil if the line at point is a directory."
  (--when-let (dired-utils-get-filename)
    (file-directory-p it)))


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
      (while (and (or (not (dired-utils-is-file-p))
                      (get-text-property (point) 'invisible))
                  (= (forward-line) 0))))
    (if (not (= (point) (point-max)))
        (dired-move-to-filename)
      (forward-line -1)
      (dired-move-to-filename)
      nil)))

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
      (while (and (or (not (dired-utils-is-file-p))
                      (get-text-property (point) 'invisible))
                  (= (forward-line -1) 0))))
    (if (not (= (point) (point-min)))
        (dired-move-to-filename)
      (dired-hacks-next-file)
      nil)))

(defun dired-hacks-compare-files (file-a file-b)
  "Test if two files FILE-A and FILE-B are the (probably) the same."
  (interactive (let ((other-dir (dired-dwim-target-directory)))
                 (list (read-file-name "File A: " default-directory (car (dired-get-marked-files)) t)
                       (read-file-name "File B: " other-dir (with-current-buffer (cdr (assoc other-dir dired-buffers))
                                                              (car (dired-get-marked-files))) t))))
  (let ((md5-a (with-temp-buffer
                 (shell-command (format "md5sum %s" file-a) (current-buffer))
                 (buffer-string)))
        (md5-b (with-temp-buffer
                 (shell-command (format "md5sum %s" file-b) (current-buffer))
                 (buffer-string))))
    (message "%s%sFiles are %s." md5-a md5-b
             (if (equal (car (split-string md5-a))
                        (car (split-string md5-b)))
                 "probably the same" "different"))))

(provide 'dired-hacks-utils)

;;; dired-hacks-utils.el ends here
