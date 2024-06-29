;;; dired-ranger.el --- Implementation of useful ranger features for dired

;; Copyright (C) 2014-2015 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Version: 0.0.1
;; Created: 17th June 2014
;; Package-Requires: ((dash "2.7.0") (dired-hacks-utils "0.0.1") (emacs "24.3"))
;; Keywords: files
;; URL: https://github.com/Fuco1/dired-hacks

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

;; This package implements useful features present in the
;; [ranger](http://ranger.github.io/) file manager which are missing
;; in dired.

;; Multi-stage copy/pasting of files
;; ---------------------------------

;; A feature present in most orthodox file managers is a "two-stage"
;; copy/paste process.  Roughly, the user first selects some files,
;; "copies" them into a clipboard and then pastes them to the target
;; location.  This workflow is missing in dired.

;; In dired, user first marks the files, then issues the
;; `dired-do-copy' command which prompts for the destination.  The
;; files are then copied there.  The `dired-dwim-target' option makes
;; this a bit friendlier---if two dired windows are opened, the other
;; one is automatically the default target.

;; With the multi-stage operations, you can gather files from
;; *multiple* dired buffers into a single "clipboard", then copy or
;; move all of them to the target location.  Another huge advantage is
;; that if the target dired buffer is already opened, switching to it
;; via ido or ibuffer is often faster than selecting the path.

;; Call `dired-ranger-copy' to add marked files (or the file under
;; point if no files are marked) to the "clipboard".  With non-nil
;; prefix argument, add the marked files to the current clipboard.

;; Past clipboards are stored in `dired-ranger-copy-ring' so you can
;; repeat the past pastes.

;; Call `dired-ranger-paste' or `dired-ranger-move' to copy or move
;; the files in the current clipboard to the current dired buffer.
;; With raw prefix argument (usually C-u), the clipboard is not
;; cleared, so you can repeat the copy operation in another dired
;; buffer.

;; The copy or move operation is asynchronous if `dired-async-mode'
;; is activated.

;; Bookmarks
;; ---------

;; Use `dired-ranger-bookmark' to bookmark current dired buffer.  You
;; can later quickly revisit it by calling
;; `dired-ranger-bookmark-visit'.

;; A bookmark name is any single character, letter, digit or a symbol.

;; A special bookmark with name `dired-ranger-bookmark-LRU' represents
;; the least recently used dired buffer.  Its default value is `.  If
;; you bind `dired-ranger-bookmark-visit' to the same keybinding,
;; hitting `` will instantly bring you to the previously used dired
;; buffer.  This can be used to toggle between two dired buffers in a
;; very fast way.

;; These bookmarks are not persistent.  If you want persistent
;; bookmarks use the bookmarks provided by Emacs, see (info "(emacs)
;; Bookmarks").

;;; Code:

(require 'dash)
(require 'ring)
(require 'dired-aux)

(defgroup dired-ranger ()
  "Implementation of useful ranger features for dired."
  :group 'dired-hacks
  :prefix "dired-ranger-")


;; multi-stage copy/paste operations
(defcustom dired-ranger-copy-ring-size 10
  "Specifies how many filesets for copy/paste operations should be stored."
  :type 'natnum
  :group 'dired-ranger)

(defvar dired-ranger-copy-ring (make-ring dired-ranger-copy-ring-size))

;;;###autoload
(defun dired-ranger-copy (arg)
  "Place the marked items in the copy ring.

With non-nil prefix argument, add the marked items to the current
selection.  This allows you to gather files from multiple dired
buffers for a single paste."
  (interactive "P")
  ;; TODO: add dired+ `dired-get-marked-files' support?
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
         (cons (-distinct (cons (current-buffer) (car current)))
               (-distinct (-concat (dired-get-marked-files) (cdr current)))))
        (message (format "Added %d item%s into copy ring."
                         (length marked)
                         (if (> (length marked) 1) "s" "")))))))

(defun dired-ranger--name-constructor (oldname)
  "Return the new file name corresponding to OLDNAME."
  (concat (dired-current-directory) (file-name-nondirectory oldname)))

;;;###autoload
(defun dired-ranger-paste (arg)
  "Copy the items from copy ring to current directory.

With raw prefix argument \\[universal-argument], do not remove
the selection from the stack so it can be copied again.

With numeric prefix argument, copy the n-th selection from the
copy ring."
  (interactive "P")
  (let* ((index (if (numberp arg) arg 0))
         (data (ring-ref dired-ranger-copy-ring index))
         (files (cdr data)))
    (dired-create-files #'dired-copy-file "Copy" files
                        #'dired-ranger--name-constructor ?C)
    (unless arg (ring-remove dired-ranger-copy-ring 0))))

;;;###autoload
(defun dired-ranger-move (arg)
  "Move the items from copy ring to current directory.

This behaves like `dired-ranger-paste' but moves the files
instead of copying them."
  (interactive "P")
  (let* ((index (if (numberp arg) arg 0))
         (data (ring-ref dired-ranger-copy-ring index))
         (files (cdr data)))
    (dired-create-files #'rename-file "Rename" files
                        #'dired-ranger--name-constructor ?M)
    (unless arg (ring-remove dired-ranger-copy-ring 0))))


;; bookmarks
(defcustom dired-ranger-bookmark-reopen 'ask
  "Should we reopen closed dired buffer when visiting a bookmark?

This does only correctly reopen regular dired buffers listing one
directory.  Special dired buffers like the output of `find-dired'
or `ag-dired', virtual dired buffers and subdirectories can not
be recreated.

The value 'never means never reopen the directory.

The value 'always means always reopen the directory.

The value 'ask will ask if we should reopen or not.  Reopening a
dired buffer for a directory that is already opened in dired will
bring that up, which might be unexpected as that directory might
come from a non-standard source (i.e. not be file-system
backed)."
  :type '(radio
          (const :tag "Never reopen automatically." never)
          (const :tag "Always reopen automatically." always)
          (const :tag "Reopen automatically only in standard dired buffers, ask otherwise." ask))
  :group 'dired-ranger)

(defcustom dired-ranger-bookmark-LRU ?`
  "Bookmark representing the least recently used/visited dired buffer.

If a dired buffer is currently active, select the one visited
before.  If a non-dired buffer is active, visit the least
recently visited dired buffer."
  :type 'char
  :group 'dired-ranger)

(defvar dired-ranger-bookmarks nil
  "An alist mapping bookmarks to dired buffers and locations.")

;;;###autoload
(defun dired-ranger-bookmark (char)
  "Bookmark current dired buffer.

CHAR is a single character (a-zA-Z0-9) representing the bookmark.
Reusing a bookmark replaces the content.  These bookmarks are not
persistent, they are used for quick jumping back and forth
between currently used directories."
  (interactive "cBookmark name: ")
  (let ((dir (file-truename default-directory)))
    (-if-let (value (cdr (assoc char dired-ranger-bookmarks)))
        (setf (cdr (assoc char dired-ranger-bookmarks)) (cons dir (current-buffer)))
      (push (-cons* char dir (current-buffer)) dired-ranger-bookmarks))
    (message "Bookmarked directory %s as `%c'" dir char)))

;;;###autoload
(defun dired-ranger-bookmark-visit (char)
  "Visit bookmark CHAR.

If the associated dired buffer was killed, we try to reopen it
according to the setting `dired-ranger-bookmark-reopen'.

The special bookmark `dired-ranger-bookmark-LRU' always jumps to
the least recently visited dired buffer.

See also `dired-ranger-bookmark'."
  (interactive "cBookmark name: ")
  (if (eq char dired-ranger-bookmark-LRU)
      (progn
        (let ((buffers (buffer-list)))
          (when (eq (with-current-buffer (car buffers) major-mode) 'dired-mode)
            (pop buffers))
          (switch-to-buffer (--first (eq (with-current-buffer it major-mode) 'dired-mode) buffers))))
    (-if-let* ((value (cdr (assoc char dired-ranger-bookmarks)))
               (dir (car value))
               (buffer (cdr value)))
        (if (buffer-live-p buffer)
            (switch-to-buffer buffer)
          (when
              ;; TODO: abstract this never/always/ask pattern. It is
              ;; also used in filter.
              (cond
               ((eq dired-ranger-bookmark-reopen 'never) nil)
               ((eq dired-ranger-bookmark-reopen 'always) t)
               ((eq dired-ranger-bookmark-reopen 'ask)
                (y-or-n-p (format "The dired buffer referenced by this bookmark does not exist.  Should we try to reopen `%s'?" dir))))
            (find-file dir)
            (setf (cdr (assoc char dired-ranger-bookmarks)) (cons dir (current-buffer)))))
      (message "Bookmark `%c' does not exist." char))))

(provide 'dired-ranger)
;;; dired-ranger.el ends here
