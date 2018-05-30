;;; dired-collapse.el --- Collapse unique nested paths in dired listing -*- lexical-binding: t -*-

;; Copyright (C) 2017 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Version: 0.0.1
;; Created: 15th July 2017
;; Package-requires: ((dash "2.10.0") (f "0.19.0"))
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

;; Often times we find ourselves in a situation where a single file
;; or directory is nested in a chain of nested directories with no
;; other content.  This is sometimes due to various mandatory
;; layouts demanded by packaging tools or tools generating these
;; deeply-nested "unique" paths to disambiguate architectures or
;; versions (but we often use only one anyway).  If the user wants
;; to access these directories they have to quite needlessly
;; drill-down through varying number of "uninteresting" directories
;; to get to the content.

;; This minor mode is in main inspired by how GitHub renders these
;; paths: if there is a chain of directories where each one only has
;; one child, they are concatenated together and shown on the first
;; level in this collapsed form.  When the user clicks this
;; collapsed directory they are immediately brought to the deepest
;; directory with some actual content.

;; To enable or disable this functionality use `dired-collapse-mode'
;; to toggle it for the current dired buffer.

;; If the deepest directory contains only a single file this file is
;; displayed instead of the last directory.  This way we can get
;; directly to the file itself.  This is often helpful with config
;; files which are stored in their own directories, for example in
;; `~/.config/foo/config' and similar situations.

;; The files or directories re-inserted in this manner will also
;; have updated permissions, file sizes and modification dates so
;; they truly correspond to the properties of the file being shown.

;; The path to the deepest file is dimmed with the `shadow' face so
;; that it does not distract but at the same time is still available
;; for inspection.

;; The mode is integrated with `dired-rainbow' so the nested files
;; are properly colored according to user's rules.

;;; Code:

(require 'dash)
(require 'dired)
(require 'f)

(defgroup dired-collapse ()
  "Collapse unique nested paths in dired listing."
  :group 'dired-hacks
  :prefix "dired-collapse-")

;;;###autoload
(define-minor-mode dired-collapse-mode
  "Toggle collapsing of unique nested paths in Dired."
  :group 'dired-collapse
  :lighter ""
  (if dired-collapse-mode
      (progn
        (add-hook 'dired-after-readin-hook 'dired-collapse 'append 'local)
        (add-hook 'dired-subtree-after-insert-hook 'dired-collapse 'append 'local)
        ;; collapse the buffer only if it is not empty (= we haven't
        ;; yet read in the current directory)
        (unless (= (buffer-size) 0)
          (dired-collapse)))
    (remove-hook 'dired-after-readin-hook 'dired-collapse 'local)
    (remove-hook 'dired-subtree-after-insert-hook 'dired-collapse 'local)
    (revert-buffer)))

(defun dired-collapse--replace-file (file)
  "Replace file on the current line with FILE."
  (delete-region (line-beginning-position) (1+ (line-end-position)))
  (insert "  ")
  (insert-directory file dired-listing-switches nil nil)
  (forward-line -1)
  (dired-align-file (line-beginning-position) (1+ (line-end-position)))
  (when (file-remote-p (dired-get-filename nil t))
    (while (search-forward (dired-current-directory) (line-end-position) t)
      (replace-match ""))))

(defun dired-collapse ()
  "Collapse unique nested paths in dired listing."
  (-let* (;; dired-hide-details-mode hides details by assigning a special invisibility text property
          ;; to them, while dired-collapse requires all the details. So we disable invisibility here
          ;; temporarily.
          (buffer-invisibility-spec nil)
          (inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (and (looking-at-p dired-re-dir)
                   (not (member (dired-get-filename 'no-dir t) (list "." "..")))
                   (not (eolp)))
          (let ((path (dired-get-filename nil t))
                files)
            (while (and (file-directory-p path)
                        (file-readable-p path)
                        (setq files (f-entries path))
                        (= 1 (length files)))
              (setq path (car files)))
            (setq path (s-chop-prefix (dired-current-directory) path))
            (when (string-match-p "/" path)
              (let ((default-directory (dired-current-directory)))
                (dired-collapse--replace-file path))
              (dired-insert-set-properties (line-beginning-position) (line-end-position))
              (dired-move-to-filename)
              (let* ((beg (point))
                     (end (save-excursion
                            (dired-move-to-end-of-filename)
                            (1+ (search-backward "/"))))
                     (ov (make-overlay beg end)))
                (overlay-put ov 'face 'shadow)))))
        (forward-line 1)))))

(provide 'dired-collapse)
;;; dired-collapse.el ends here
