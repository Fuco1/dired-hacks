;;; dired-filter.el --- Ibuffer-like filtering for dired.

;; Copyright (C) 2014 Matus Goljer

;; Author: Matus Goljer <matus.goljer@gmail.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Keywords: lisp
;; Version: 0.0.1
;; Created: 14th February 2014
;; Package-requires: ((dash "2.5.0") (dired-hacks-utils "0.0.1"))

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

;; Adds much-lacking filtering facilities to dired.

;; See https://github.com/Fuco1/dired-hacks for the entire collection

;;; Code:

(require 'dired-x)
(require 'dired-aux)
(require 'dash)
(require 'dired-hacks-utils)

(defvar dired-filter-marker-char ?\x3FF
  "Temporary marker used by Dired-Filter.")

(defvar dired-filter-stack nil
  "Filter stack.")
(make-variable-buffer-local 'dired-filter-stack)

(defvar dired-filter-alist nil
  "Definitions of filters.

Entries are of type (name desc body) ")

(defgroup dired-filter ()
  "Dired-filter"
  :group 'dired-hacks
  :prefix "dired-filter-")

(defcustom dired-filter-verbose t
  "If non-nil, print debug messages."
  :type 'boolean
  :group 'dired-filter)

(defcustom dired-filter-prefix "/"
  "Prefix key for `dired-filter-mode-map'."
  :type 'string
  :group 'dired-filter
  :set 'dired-filter--set-prefix-key)

(defun dired-filter--set-prefix-key (varname value)
  (when varname
    (set-default varname value))
  (define-key dired-mode-map (kbd value) dired-filter-map))


;; internals
(defun dired-filter--push (filter)
  "Push FILTER onto the active filter stack."
  (push filter dired-filter-stack))

(defun dired-filter--make-filter-1 (stack)
  (cond
   ((eq (car stack) 'or)
    `(or ,@(mapcar 'dired-filter--make-filter-1 (cdr stack))))
   ((eq (car stack) 'not)
    `(not ,@(mapcar 'dired-filter--make-filter-1 (cdr stack))))
   (t (let* ((def (assoc (car stack) dired-filter-alist))
             (remove (caddr def))
             (qualifier (cdr stack)))
        (if qualifier
            `(let ((qualifier ,qualifier))
               ,(if remove
                    `(not ,(cadddr def))
                  (cadddr def)))
          (if remove
              `(not ,(cadddr def))
            (cadddr def)))))))

(defun dired-filter--make-filter ()
  "Build the expression that filters the files.

When this expression evals to non-nil, file is kept in the
listing."
  `(and ,@(mapcar 'dired-filter--make-filter-1 dired-filter-stack)))

(defun dired-filter--update ()
  "Re-run the filters."
  (let ((file-name (ignore-errors (dired-get-filename))))
    (dired-revert)
    (when file-name
      (dired-utils-goto-line file-name))))

(defun dired-filter--expunge ()
  "Remove the files specified by current `dired-filter-stack'
from the listing."
  (interactive)
  (when (and dired-filter-mode
             dired-filter-stack)
    (let ((filter (dired-filter--make-filter))
          (dired-marker-char dired-filter-marker-char)
          (old-modified-p (buffer-modified-p))
          count)
      (when dired-filter-verbose (message "Filtering..."))
      (if (eval (dired-filter--mark-unmarked filter))
          (setq count (dired-do-kill-lines nil (if dired-filter-verbose "Filtered %d line%s." "")))
        (when dired-filter-verbose (message "Nothing to filter")))
      (set-buffer-modified-p (and old-modified-p
                                  (save-excursion
                                    (goto-char (point-min))
                                    (re-search-forward dired-re-mark nil t))))
      count)))

(defun dired-filter--mark-unmarked (filter)
  `(dired-mark-if
    (let ((file-name (ignore-errors (dired-get-filename 'no-dir t))))
      (and
       file-name
       (looking-at " ")
       (not ,filter)))
    nil))


;; ui
;;;###autoload
(cl-defmacro dired-filter-define (name documentation
                                       (&key
                                        description
                                        reader
                                        remove)
                                       &rest body)
  "Create a filter NAME.

Files matched by the predicate are kept in the listing.

For filters where the reverse behaviour makes more sense as
default, you can set the `:remove' argument to `t' to flip the
truth value by default.  Do not flip the value in the predicate
itself!

DOCUMENTATION is the documentation of the created filter.

BODY should contain forms which will be evaluated to test whether or
not a particular file should be displayed or not.  The forms in BODY
will be evaluated with FILE-NAME bound to the file name, and QUALIFIER
bound to the current argument of the filter.

:description is a short description of this filter (usually one
or two words).

:reader is a form that is used by `interactive' to read optional
argument.  If not specified or nil, the filter does not accept
argument from user.

:remove reverses the default matching strategy of the filter."
  (declare (indent 2) (doc-string 2))
  (let ((fn-name (intern (concat "dired-filter-by-" (symbol-name name)))))
    `(progn
       (defun ,fn-name (qualifier)
         ,(or (and documentation
                   (if remove
                       (concat documentation "\n\nBy default, files matched by this filter are /removed/.")
                     documentation))
              "This filter is not documented.")
         (interactive (list ,reader))
         (dired-filter--push (cons ',name qualifier))
         (message "%s" (format ,(concat (format "Filter by %s added: " description) " %s") qualifier))
         (dired-filter--update))
       (push (list ',name ,description ,remove ',(if (= (length body) 1)
                                                     `,(car body)
                                                   `(progn ,@body)))
             dired-filter-alist))))

(dired-filter-define dot-files
    "Toggle current view to dot-files."
  (:description "dot-files"
   :reader nil
   :remove t)
  (string-match "^\\." file-name))

(dired-filter-define name
    "Toggle current view to files matching QUALIFIER."
  (:description "name"
   :reader (regexp-quote (read-from-minibuffer "Pattern: " )))
  (string-match qualifier file-name))

(dired-filter-define regexp
    "Toggle current view to files matching QUALIFIER as a regular expression."
  (:description "name"
   :reader (read-from-minibuffer "Regexp: " ))
  (string-match qualifier file-name))

(dired-filter-define extension
    "Toggle current view to files with extension matching QUALIFIER."
  (:description "name"
   :reader (concat "\\." (regexp-quote (read-from-minibuffer "Extension: " )) "\\'"))
  (string-match qualifier file-name))

(dired-filter-define omit
    "Toggle current view to files matched by `dired-omit-regexp'."
  (:description "omit"
   :reader (dired-omit-regexp)
   :remove t)
  (string-match qualifier file-name))

(dired-filter-define predicate
    "Toggle current view to files for which QUALIFIER returns non-nil."
  (:description "predicate"
   :reader (read-minibuffer "Filter by predicate (form): "))
  (eval qualifier))

(defun dired-filter-transpose ()
  "Transpose the two top filters."
  (interactive)
  (let ((top (car dired-filter-stack))
        (top2 (cadr dired-filter-stack)))
    (when (not top2)
      (error "To transpose you need at least two filters on stack"))
    (pop dired-filter-stack)
    (pop dired-filter-stack)
    (dired-filter--push top)
    (dired-filter--push top2)))

(defun dired-filter-or ()
  "Or the top two filters."
  (interactive)
  (let ((top (car dired-filter-stack))
        (top2 (cadr dired-filter-stack)))
    (when (not top2)
      (error "To \"or\" you need at least two filters on stack"))
    (pop dired-filter-stack)
    (pop dired-filter-stack)
    (cond
     ((and (eq (car top) 'or)
           (eq (car top2) 'or))
      (dired-filter--push (append '(or) (cdr top) (cdr top2))))
     ((eq (car top) 'or)
      (dired-filter--push (append '(or) (cdr top) `(,top2))))
     ((eq (car top2) 'or)
      (dired-filter--push (append `(or ,top) (cdr top2))))
     (t (dired-filter--push `(or ,top ,top2))))
    (dired-filter--update)))

(defun dired-filter-negate ()
  "Logically negate the top filter."
  (interactive)
  (let ((top (car dired-filter-stack)))
    (when (not top)
      (error "You need at least one filter on the stack"))
    (pop dired-filter-stack)
    (dired-filter--push `(not ,top))
    (dired-filter--update)))

(defun dired-filter-decompose ()
  "Decompose the composite filter on top of the stack.

This means, if the filter is an `or' or `not' filter, pop it and
push all its constituents back on the stack."
  (interactive)
  (let ((top (car dired-filter-stack)))
    (if (not (or (eq (car top) 'or)
                 (eq (car top) 'not)))
        (error "You can only decompose `or' or `not' filters.")
      (pop dired-filter-stack)
      (--each (nreverse (cdr top))
        (dired-filter--push it))
      (dired-filter--update))))

(defun dired-filter-pop ()
  "Remove the top filter in this buffer."
  (interactive)
  (let ((top (pop dired-filter-stack)))
    (when dired-filter-verbose
      (if top
          (message "Popped filter %s: %s" (car top) (cdr top))
        (message "Filter stack was empty."))))
  (dired-filter--update))

(defun dired-filter-pop-all ()
  "Remove all the filters in this buffer."
  (interactive)
  (setq dired-filter-stack nil)
  (dired-filter--update))


;; mode stuff
(defvar dired-filter-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'dired-filter-by-name)
    (define-key map "r" 'dired-filter-by-regexp)
    (define-key map "x" 'dired-filter-by-extension)
    (define-key map "." 'dired-filter-by-dot-files)
    (define-key map "i" 'dired-filter-by-omit)
    (define-key map "e" 'dired-filter-by-predicate)

    (define-key map "o" 'dired-filter-or)
    (define-key map "!" 'dired-filter-negate)
    (define-key map "d" 'dired-filter-decompose)
    (define-key map (kbd "TAB") 'dired-filter-transpose)
    (define-key map "p" 'dired-filter-pop)
    (define-key map "/" 'dired-filter-pop-all)
    map)
  "Keymap used for `dired-filter-mode'.")

(define-minor-mode dired-filter-mode
  "Toggle filtering of files in Dired."
  :group 'dired-filter
  :lighter " Filter"
  (if dired-filter-mode
      (dired-filter--expunge)
    (revert-buffer)))

(add-hook 'dired-after-readin-hook 'dired-filter--expunge)

(provide 'dired-filter)

;;; dired-filter.el ends here
