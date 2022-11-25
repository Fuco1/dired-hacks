;;; dired-avfs.el --- AVFS support for dired

;; Copyright (C) 2014 Matus Goljer

;; Author: Matus Goljer <matus.goljer@gmail.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Keywords: files
;; Version: 0.0.1
;; Created: 14th February 2014
;; Package-Requires: ((dash "2.5.0") (dired-hacks-utils "0.0.1"))

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

;; Adds AVFS (http://avf.sourceforge.net/) support for seamless archive
;; browsing.  This extension therefore depends on the presence of `avfsd'
;; on your system.  In debian-derived distributions you can usually do
;;
;;     apt-get install avfs
;;
;; `avfs' is probably also available for Mac OS.  You're out of luck on
;; Windows, sorry.

;; Once the daemon is installed, run it with `mountavfs' and everything
;; "Should Just Work^TM".

;; See https://github.com/Fuco1/dired-hacks for the entire collection.

;;; Code:

(require 'dired-hacks-utils)
(require 'dash)

(defgroup dired-avfs ()
  "AVFS support for dired."
  :group 'dired-hacks
  :prefix "dired-avfs-")

(defcustom dired-avfs-root "~/.avfs"
  "Root where the avfs virtual filesystem is mounted."
  :type 'directory
  :group 'dired-avfs)

(defcustom dired-avfs-archives
  '("zip" "rar" "tar" "tar.gz" "tgz" "tar.bz2" "tb2" "tbz2" "tar.xz" "txz")
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

(defcustom dired-avfs-file-size-threshold 100
  "Ask before opening files if their size exceeds this setting.

The value is in megabytes."
  :type 'integer
  :group 'dired-avfs)

(defun dired-avfs--archive-filename (filename)
  "Transform FILENAME into corresponding avfs filename."
  (file-truename (concat dired-avfs-root (file-truename filename) "#")))

(defun dired-avfs--archive-p (filename)
  "Non-nil if FILENAME should be opened in avfs."
  (let ((extensions (concat "\\." (regexp-opt dired-avfs-archives) "\\'")))
    (string-match-p extensions filename)))

(defun dired-avfs--open (filename)
  "Open FILENAME as avfs filename."
  (find-file (dired-avfs--archive-filename filename)))

(defun dired-avfs--hide-root ()
  "Remove the avfs root prefix from the dired header."
  (save-excursion
    (when dired-avfs-hide-root
      (goto-char (point-min))
      (when (search-forward (file-truename dired-avfs-root) nil t)
        (let ((inhibit-read-only t))
          (put-text-property (match-beginning 0) (match-end 0) 'invisible t))))))

(add-hook 'dired-after-readin-hook 'dired-avfs--hide-root)

(defun dired-avfs-open ()
  "Open file at point using avfs."
  (interactive)
  (dired-avfs--open (dired-file-name-at-point)))

(defadvice find-file-noselect (before fix-avfs-arguments activate)
  "Change target filename to avfs-compatible filename.

If the target is archive that can be handled via avfs,
automagically change the filename to the location of virtual
directory representing this archive."
  (when (and (not (memq this-command dired-avfs-ignore-commands))
             (or (not (featurep 'tramp))
                 (not (tramp-tramp-file-p (ad-get-arg 0))))
             (dired-avfs--archive-p (ad-get-arg 0))
             (if (> (nth 7 (file-attributes (ad-get-arg 0))) (* dired-avfs-file-size-threshold 1048576))
                 (y-or-n-p (format "Size of this file exceeds `dired-avfs-file-size-threshold' (%d MB), extracting the information might take very long time.  Do you want to continue?"
                                   dired-avfs-file-size-threshold))
               t))
    (ad-set-arg 0 (dired-avfs--archive-filename (ad-get-arg 0)))))

(provide 'dired-avfs)

;;; dired-avfs.el ends here
