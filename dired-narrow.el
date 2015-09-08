;;; dired-narrow.el --- Live-narrowing of search results for dired

;; Copyright (C) 2014-2015 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Version: 0.0.1
;; Created: 14th February 2014
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

;; This package provides live filtering of files in dired buffers.  In
;; general, after calling the respective narrowing function you type a
;; filter string into the minibuffer.  After each change the changes
;; are automatically reflect in the buffer.  Typing C-g will cancel
;; the narrowing and restore the original view, typing RET will exit
;; the query mode and leave the filter in the narrowed state.  To
;; bring it back to the original view, you can call `revert-buffer'
;; (usually bound to `g').

;; These narrowing functions are provided:

;; * `dired-narrow'
;; * `dired-narrow-regexp'
;; * `dired-narrow-fuzzy'

;; You can also create your own narrowing functions quite easily.  To
;; define new narrowing function, use `dired-narrow--internal' and
;; pass it an apropriate filter.  The filter should take one argument
;; which is the filter string from the minibuffer.  It is then called
;; at each line that describes a file with point at the beginning of
;; the file name.  If the filter returns nil, the file is removed from
;; the view.  As an inspiration, look at the built-in functions
;; mentioned above.

;; See https://github.com/Fuco1/dired-hacks for the entire collection.

;;; Code:

(require 'dash)
(require 'dired-hacks-utils)

(defgroup dired-narrow ()
  "Live-narrowing of search results for dired."
  :group 'dired-hacks
  :prefix "dired-narrow-")


;; Utils


;; helper function for `dired-narrow--operate-over-property-range'
(defun dired-narrow--next-property-with-value (start prop val)
  "Return the starting position of text with property, PROP, and value, VAL.
VAL can be a list, in which case PROP's value is an element of
the list. If VAL is t, return the starting position of the text
with a non-nil PROP."
  (while (and start
              (not (or
                    (and (eq val t) (get-text-property start prop))
                    (and (listp val)
                         (memq (get-text-property start prop) val))
                    (eq (get-text-property start prop) val))))
    (setq start (next-single-property-change start prop)))
  start)

(defun dired-narrow--operate-over-property-range (function prop val)
  "Call FUNCTION for every range of PROP with value, VAL.
VAL can be a list, in which case PROP's value is an element of
the list. If VAL is t, then call FUNCTION on all non-nil ranges
of PROP. FUNCTION takes two arguments, the start and end of the
range, respectively."
  (let ((start (dired-narrow--next-property-with-value (point-min) prop val))
         end)
    (while start
      (setq end
            (or (and (eq val t) (text-property-any start (point-max) prop nil))
                (text-property-not-all
                 start (point-max) prop (get-text-property start prop))))

      (funcall function start (if end end (point-max)))

      (setq start (dired-narrow--next-property-with-value
                   (when end
                     (next-single-property-change start prop)) prop val)))))

(defun dired-narrow--remove-text-with-property (prop val)
  "Delete all text in the current buffer with text property PROP
and value VAL."
  (dired-narrow--operate-over-property-range
   #'delete-region prop val))

(defvar dired-narrow-filter-function 'identity
  "Filter function used to filter the dired view.")

(defun dired-narrow--update (filter)
  "Make the files not matching the filter invisible."
  (goto-char (point-min))
  (let ((inhibit-read-only t))
    (while (dired-hacks-next-file)
      (let ((matched (funcall dired-narrow-filter-function filter)))
        (put-text-property (line-beginning-position) (1+ (line-end-position))
                           'invisible (unless matched :dired-narrow))
        (when matched
          ;; For Emacs 24.4+ restore invisible text properties to line
          (when (fboundp 'dired-insert-set-properties)
            (dired-insert-set-properties
             (line-beginning-position) (1+ (line-end-position)))))))))


(defun dired-narrow--restore ()
  "Restore the invisible files of the current buffer."
  (remove-from-invisibility-spec :dired-narrow)
  (when (fboundp 'dired-insert-set-properties)
    (dired-insert-set-properties (point-min) (point-max))))


;; Live filtering

(defvar dired-narrow-buffer nil
  "Dired buffer we are currently filtering.")

(defun dired-narrow--minibuffer-setup ()
  "Set up the minibuffer for live filtering."
  (when dired-narrow-buffer
    (add-hook 'post-command-hook 'dired-narrow--live-update nil :local)))

(add-hook 'minibuffer-setup-hook 'dired-narrow--minibuffer-setup)

(defun dired-narrow--live-update ()
  "Update the dired buffer based on the contents of the minibuffer."
  (when dired-narrow-buffer
    (let ((current-filter (minibuffer-contents-no-properties)))
      (with-current-buffer dired-narrow-buffer
        (dired-narrow--update current-filter)))))

(defun dired-narrow--internal (filter-function)
  "Narrow a dired buffer to the files matching a filter.

The function FILTER-FUNCTION is called on each line: if it
returns non-nil, the line is kept, otherwise it is removed.  The
function takes one argument, which is the current filter string
read from minibuffer."
  (let ((dired-narrow-buffer (current-buffer))
        (dired-narrow-filter-function filter-function)
        (current-file (dired-utils-get-filename)))
    (with-current-buffer dired-narrow-buffer
      (add-to-invisibility-spec :dired-narrow)
      (condition-case nil
          (progn
            (read-from-minibuffer "Filter: ")
            (let ((inhibit-read-only t))
              (dired-narrow--remove-text-with-property
               'invisible :dired-narrow))
            (when (featurep 'dired-details)
              (dired-details-delete-overlays)
              (dired-details-activate))
            (dired-next-subdir 0)
            (dired-hacks-next-file))
        (quit
         (let ((inhibit-read-only t))
           (dired-narrow--restore))
         (dired-utils-goto-line current-file))))))


;; Interactive

(defun dired-narrow--regexp-filter (filter)
  (re-search-forward filter (line-end-position) t))

;;;###autoload
(defun dired-narrow-regexp ()
  "Narrow a dired buffer to the files matching a regular expression."
  (interactive)
  (dired-narrow--internal 'dired-narrow--regexp-filter))

(defun dired-narrow--string-filter (filter)
  (let ((words (split-string filter " ")))
    (--all? (save-excursion (search-forward it (line-end-position) t)) words)))

;;;###autoload
(defun dired-narrow ()
  "Narrow a dired buffer to the files matching a string.

If the string contains spaces, then each word is matched against
the file name separately.  To succeed, all of them have to match
but the order does not matter.

For example \"foo bar\" matches filename \"bar-and-foo.el\"."
  (interactive)
  (dired-narrow--internal 'dired-narrow--string-filter))

(defun dired-narrow--fuzzy-filter (filter)
  (re-search-forward
   (mapconcat 'regexp-quote
              (mapcar 'char-to-string (string-to-list filter))
              ".*")
   (line-end-position) t))

;;;###autoload
(defun dired-narrow-fuzzy ()
  "Narrow a dired buffer to the files matching a fuzzy string.

A fuzzy string is constructed from the filter string by inserting
\".*\" between each letter.  This is then matched as regular
expression against the file name."
  (interactive)
  (dired-narrow--internal 'dired-narrow--fuzzy-filter))

(provide 'dired-narrow)
;;; dired-narrow.el ends here
