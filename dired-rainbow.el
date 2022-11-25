;;; dired-rainbow.el --- Extended file highlighting according to its type

;; Copyright (C) 2014-2017 Matus Goljer

;; Author: Matus Goljer <matus.goljer@gmail.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Keywords: files
;; Package-Version: 20170922.817
;; Version: 0.0.3
;; Created: 16th February 2014
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

;; This package adds more customizable highlighting for files in dired
;; listings.  The group `dired-faces' provides only nine faces and
;; isn't very fine-grained.
;;
;; The definitions are added by several macros, currently available
;; are:
;;
;; * `dired-rainbow-define` - add face by file extension
;; * `dired-rainbow-define-chmod` - add face by file permissions
;;
;; You can display their documentation by calling (substituting the
;; desired macro name):
;;
;; M-x describe-function RET dired-rainbow-define RET
;;
;; Here are some example uses:
;;
;; (defconst my-dired-media-files-extensions
;;   '("mp3" "mp4" "MP3" "MP4" "avi" "mpg" "flv" "ogg")
;;   "Media files.")
;;
;; (dired-rainbow-define html "#4e9a06" ("htm" "html" "xhtml"))
;; (dired-rainbow-define media "#ce5c00" my-dired-media-files-extensions)
;;
;; ; boring regexp due to lack of imagination
;; (dired-rainbow-define log (:inherit default
;;                            :italic t) ".*\\.log")
;;
;; ; highlight executable files, but not directories
;; (dired-rainbow-define-chmod executable-unix "Green" "-.*x.*")
;;
;; See https://github.com/Fuco1/dired-hacks for the entire collection.

;;; Code:

(require 'dired-hacks-utils)
(require 'dash)

(defgroup dired-rainbow ()
  "Extended file highlighting according to its type."
  :group 'dired-hacks
  :prefix "dired-rainbow-")

(defvar dired-rainbow-ext-to-face nil
  "An alist mapping extension groups to face and compiled regexp.

This alist is constructed in `dired-rainbow-define' for the case
when the user wants to reuse the associations outside of dired.")

(defun dired-rainbow--get-face (face-props)
  "Return face specification according to FACE-PROPS.

See `dired-rainbow-define'."
  (cond
   ((stringp face-props)
    `(:foreground ,face-props))
   ((symbolp face-props)
    `(:inherit ,face-props))
   (t face-props)))

(defmacro dired-rainbow-define (symbol face-props extensions &optional how)
  "Define a custom dired face highlighting files by extension.

SYMBOL is the identifier of the face.  The macro will define a face named

  dired-rainbow-SYMBOL-face.

FACE-PROPS is a string, a list or a symbol.  If a string, it is
assumed to be either a color name or a hexadecimal code (#......)
describing a color.  If a list, it is assumed to be a property
list describing the face.  See `defface' for list of possible
attributes.  If a symbol it is taken as the name of an existing
face which is used.

EXTENSIONS is either a list or a symbol evaluating to a list of
extensions that should be highlighted with this face.  Note that
if you specify a symbol, its value *must* be known during
compilation and must be defined before this macro is processed.

Additionally, EXTENSIONS can be a single string or a symbol
evaluating to a string that is interpreted as a regexp matching
the entire file name.

HOW is a parameter that is passed directly to `font-lock-add-keywords'
to control the order."
  (declare (debug (symbolp [&or stringp listp symbolp] [&or symbolp listp stringp])))
  (let* ((matcher (if (or (listp extensions)
                          (stringp extensions))
                      extensions
                    (symbol-value extensions)))
         (regexp (concat
                  "^[^!].[^d].*[ ]"
                  dired-hacks-datetime-regexp
                  "[ ]\\("
                  (if (listp matcher)
                      (concat ".*\\." (regexp-opt matcher))
                    matcher)
                  "\\)$"))
         (face-name (intern (concat "dired-rainbow-" (symbol-name symbol) "-face"))))
    `(progn
       (defface ,face-name
         '((t ,(dired-rainbow--get-face face-props)))
         ,(concat "dired-rainbow face matching " (symbol-name symbol) " files.")
         :group 'dired-rainbow)
       (font-lock-add-keywords 'dired-mode '((,regexp 1 ',face-name prepend)) ,how)
       (font-lock-add-keywords 'wdired-mode '((,regexp 1 ',face-name prepend)) ,how)
       ,(if (listp matcher) `(push
                              '(,matcher ,face-name ,(concat "\\." (regexp-opt matcher)))
                              dired-rainbow-ext-to-face)))))

(defmacro dired-rainbow-define-chmod (symbol face-props chmod &optional how)
  "Define a custom dired face highlighting files by chmod permissions.

SYMBOL is the identifier of the face.  The macro will define a face named

  dired-rainbow-SYMBOL-face.

FACE-PROPS is a string, a list or a symbol.  If a string, it is
assumed to be either a color name or a hexadecimal code (#......)
describing a color.  If a list, it is assumed to be a property
list describing the face.  See `defface' for list of possible
attributes.  If a symbol it is taken as the name of an existing
face which is used.

CHMOD is a regexp matching \"ls -l\" style permissions string.
For example, the pattern

  \"-.*x.*\"

matches any file with executable flag set for user, group or everyone.

HOW is a parameter that is passed directly to `font-lock-add-keywords'
to control the order."
  (declare (debug (symbolp [&or stringp listp symbolp] stringp)))
  (let* ((regexp (concat
                  "^[^!]."
                  chmod
                  ".*[ ]"
                  dired-hacks-datetime-regexp
                  "[ ]\\(.*?\\)$"))
         (face-name (intern (concat "dired-rainbow-" (symbol-name symbol) "-face"))))
    `(progn
       (defface ,face-name
         '((t ,(dired-rainbow--get-face face-props)))
         ,(concat "dired-rainbow face matching " (symbol-name symbol) " files.")
         :group 'dired-rainbow)
       (font-lock-add-keywords 'dired-mode '((,regexp 1 ',face-name prepend)) ,how)
       (font-lock-add-keywords 'wdired-mode '((,regexp 1 ',face-name prepend)) ,how))))

(provide 'dired-rainbow)

;;; dired-rainbow.el ends here
