;;; dired-filter.el --- Ibuffer-like filtering for dired

;; Copyright (C) 2014 Matus Goljer

;; Author: Matus Goljer <matus.goljer@gmail.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Keywords: files
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

;;  Introduction
;;  ------------

;; The filtering system is designed after ibuffer: every dired
;; buffer has associated "filter stack" where user can push
;; filters (predicates).  These filters are by default
;; logically "anded", meaning, only the files satsifying all the
;; predicates are shown.

;; Some filters take additional input from the user such as part of
;; name, regexp or extension, other filters only use a predefined
;; predicate such as "show only directories" or "omit dot files".

;; In addition, there are two "metafilters", the `or' filter and the
;; `not' filter.  These take other filters as arguments and change
;; their logical interpretation.  The `or' filter takes the two
;; filters on top of the stack, pops them and pushes a filter that
;; matches files satisfying one or the other (or both) filters.  The
;; `not' filter pops the top filter and pushes its logical negation.

;; To enable or disable the filters, toggle minor mode
;; `dired-filter-mode'.  Toggling this mode preserves the filter
;; stack, so you can use it to quickly hide/unhide files filtered by
;; the current filter setup.

;; All the provided interactive functions are available from
;; `dired-filter-map'.  You can customize `dired-filter-prefix' to set
;; a prefix for this map or bind it manually to a prefix of your
;; choice using:
;;
;;     (define-key dired-mode-map (kbd "some-key") dired-filter-map)

;;  Stack operations
;;  ----------------

;; To remove the filter from the stack, use `dired-filter-pop' or
;; `dired-filter-pop-all'

;; To break a metafilter apart, you can use `dired-filter-decompose'
;; to decompose the parts of the metafilter and push them back to
;; the stack.

;; You can transpose the filters on the top of the stack using
;; `dired-filter-transpose'

;;  Built-in filters
;;  ----------------

;; Here's a list of built-in filters:

;; * dired-filter-by-name
;; * dired-filter-by-regexp
;; * dired-filter-by-extension
;; * dired-filter-by-dot-files
;; * dired-filter-by-omit
;; * dired-filter-by-predicate
;; * dired-filter-by-file
;; * dired-filter-by-directory
;; * dired-filter-by-mode

;; You can see their documentation by calling M-x `describe-function'.

;; Specifically, `dired-filter-by-omit' removes the files that would
;; be removed by `dired-omit-mode', so you should not need to use
;; both---in fact it is discouraged, as it would make the read-in
;; slower.

;; To define your own filters, you can use the macro
;; `dired-filter-define'.  If you define some interesting filter,
;; please consider contributing it to the upstream.

;; See https://github.com/Fuco1/dired-hacks for the entire collection.

;;; Code:

(require 'dired-x)
(require 'dired-aux)
(require 'dired-hacks-utils)
(require 'dash)

(defvar dired-filter-marker-char ?\x3FF
  "Temporary marker used by Dired-Filter.")

(defvar dired-filter-alist nil
  "Definitions of filters.

Entries are of type (name desc body) ")

(defgroup dired-filter ()
  "Ibuffer-like filtering for dired."
  :group 'dired-hacks
  :prefix "dired-filter-")

(defcustom dired-filter-stack '((omit))
  "Filter stack.

You can customize this variable to change what filters are active
when new dired buffer is created.

The stack is a list of conses where car is the symbol
representing the fitler (it's the part of `dired-filter-by-...')
and cdr is current value of its argument, or nil if filter
doesn't take argument.

By default, `dired-filter-by-omit' is active."
  :type '(repeat (cons
                  (symbol :tag "Fitler")
                  (sexp :tag "Qualifier")))
  :group 'dired-filter)
(make-variable-buffer-local 'dired-filter-stack)

(defcustom dired-filter-verbose t
  "If non-nil, print debug messages."
  :type 'boolean
  :group 'dired-filter)

(defcustom dired-filter-show-filters t
  "If non-nil, if `dired-filter-stack' is non-nil, show a
description of active filters in header line.

This modifies `header-line-format' by appending
`dired-filter-header-line-format' to it."
  :type 'boolean
  :group 'dired-filter)

(defcustom dired-filter-header-line-format '((:eval (format "Active filters: %s" (dired-filter--describe-filters))))
  "A format expression for dired-filter's header line.

Has the same format as `mode-line-format'."
  :type 'sexp
  :group 'dired-filter)
(put 'dired-filter-header-line-format 'risky-local-variable t)

(defvar dired-filter-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'dired-filter-by-name)
    (define-key map "r" 'dired-filter-by-regexp)
    (define-key map "x" 'dired-filter-by-extension)
    (define-key map "." 'dired-filter-by-dot-files)
    (define-key map "O" 'dired-filter-by-omit)
    (define-key map "e" 'dired-filter-by-predicate)
    (define-key map "f" 'dired-filter-by-file)
    (define-key map "i" 'dired-filter-by-directory)
    (define-key map "m" 'dired-filter-by-mode)

    (define-key map "o" 'dired-filter-or)
    (define-key map "!" 'dired-filter-negate)
    (define-key map "d" 'dired-filter-decompose)
    (define-key map (kbd "TAB") 'dired-filter-transpose)
    (define-key map "p" 'dired-filter-pop)
    (define-key map "/" 'dired-filter-pop-all)
    map)
  "Keymap used for `dired-filter-mode'.")

(defun dired-filter--set-prefix-key (varname value)
  (when varname
    (set-default varname value))
  (define-key dired-mode-map (kbd value) dired-filter-map))

(defcustom dired-filter-prefix "/"
  "Prefix key for `dired-filter-mode-map'."
  :type 'string
  :group 'dired-filter
  :set 'dired-filter--set-prefix-key)


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
             (remove (cadddr def))
             (qualifier (cond
                         ;; special hack for omit filter, to
                         ;; recompute the filter regexp
                         ((eq (car stack) 'omit)
                          (dired-omit-regexp))
                         (t (cdr stack)))))
        (if qualifier
            `(let ((qualifier ,qualifier))
               ,(if remove
                    `(not ,(car (cddddr def)))
                  (car (cddddr def))))
          (if remove
              `(not ,(car (cddddr def)))
            (car (cddddr def))))))))

(defun dired-filter--make-filter ()
  "Build the expression that filters the files.

When this expression evals to non-nil, file is kept in the
listing."
  `(and ,@(mapcar 'dired-filter--make-filter-1 dired-filter-stack)))

(defun dired-filter--describe-filters-1 (stack)
  "Return a string describing `dired-filter-stack'."
  (cond
   ((eq (car stack) 'or)
    (concat "[OR " (mapconcat 'dired-filter--describe-filters-1 (cdr stack) " ") "]"))
   ((eq (car stack) 'not)
    (concat "[NOT " (mapconcat 'dired-filter--describe-filters-1 (cdr stack) " ") "]"))
   (t (let* ((def (assoc (car stack) dired-filter-alist))
             (desc (cadr def))
             (desc-qual (caddr def))
             (qualifier (cdr stack))
             (qual-formatted (eval desc-qual)))
        (if qual-formatted
            (format "[%s: %s]" desc qual-formatted)
          (format "[%s]" desc))))))

(defun dired-filter--describe-filters ()
  (mapconcat 'dired-filter--describe-filters-1 dired-filter-stack " "))

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
  ;; this should probably be moved elsewhere
  (if (and dired-filter-mode
           dired-filter-show-filters
           dired-filter-stack)
      (add-to-list 'header-line-format '("" dired-filter-header-line-format) t)
    (setq header-line-format (--remove (equal it '("" dired-filter-header-line-format)) header-line-format)))
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
                                        (qualifier-description '(identity qualifier))
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

BODY should contain forms which will be evaluated to test whether
or not a particular file should be displayed or not.  The forms
in BODY will be evaluated with FILE-NAME bound to the file name,
and QUALIFIER bound to the current argument of the filter.
During the evaluation point is at the beginning of line.

:description is a short description of this filter (usually one
or two words).

:reader is a form that is used by `interactive' to read optional
argument.  If not specified or nil, the filter does not accept
argument from user.

:qualifier-description is a form to format qualifier for display.

:remove reverses the default matching strategy of the filter."
  (declare (indent 2) (doc-string 2))
  (let ((fn-name (intern (concat "dired-filter-by-" (symbol-name name)))))
    `(progn
       (defun ,fn-name (&optional qualifier)
         ,(or (and documentation
                   (if remove
                       (concat documentation "\n\nBy default, files matched by this filter are /removed/.")
                     documentation))
              "This filter is not documented.")
         (interactive (list ,reader))
         (dired-filter--push (cons ',name qualifier))
         (message "%s" (format ,(concat (format "Filter by %s added: " description) " %s") qualifier))
         (dired-filter--update))
       (push (list ',name ,description ',qualifier-description
                   ,remove ',(if (= (length body) 1)
                                 `,(car body)
                               `(progn ,@body)))
             dired-filter-alist))))

;;;###autoload (autoload 'dired-filter-by-dot-files "dired-filter")
(dired-filter-define dot-files
    "Toggle current view to dot-files."
  (:description "dot-files"
   :reader nil
   :remove t)
  (string-match "^\\." file-name))

;;;###autoload (autoload 'dired-filter-by-name "dired-filter")
(dired-filter-define name
    "Toggle current view to files matching QUALIFIER."
  (:description "name"
   :reader (regexp-quote (read-from-minibuffer "Pattern: " )))
  (string-match qualifier file-name))

;;;###autoload (autoload 'dired-filter-by-regexp "dired-filter")
(dired-filter-define regexp
    "Toggle current view to files matching QUALIFIER as a regular expression."
  (:description "regexp"
   :reader (read-from-minibuffer "Regexp: " ))
  (string-match qualifier file-name))

;;;###autoload (autoload 'dired-filter-by-extension "dired-filter")
(dired-filter-define extension
    "Toggle current view to files with extension matching QUALIFIER."
  (:description "extension"
   :qualifier-description (substring qualifier 2 (- (length qualifier) 2))
   :reader (concat "\\." (regexp-quote (read-from-minibuffer "Extension: " )) "\\'"))
  (string-match qualifier file-name))

;;;###autoload (autoload 'dired-filter-by-omit "dired-filter")
(dired-filter-define omit
    "Toggle current view to files matched by `dired-omit-regexp'."
  (:description "omit"
   :qualifier-description nil
   :remove t)
  (string-match qualifier file-name))

;;;###autoload (autoload 'dired-filter-by-predicate "dired-filter")
(dired-filter-define predicate
    "Toggle current view to files for which QUALIFIER returns non-nil."
  (:description "predicate"
   :reader (read-minibuffer "Filter by predicate (form): "))
  (eval qualifier))

;;;###autoload (autoload 'dired-filter-by-directory "dired-filter")
(dired-filter-define directory
    "Toggle current view to show only directories."
  (:description "directory")
  (looking-at "^  d"))

;;;###autoload (autoload 'dired-filter-by-file "dired-filter")
(dired-filter-define file
    "Toggle current view to show only files."
  (:description "file")
  (looking-at "^  -"))

;;;###autoload (autoload 'dired-filter-by-mode "dired-filter")
(dired-filter-define mode
    "Toggle current view to files which open in mode specified by QUALIFIER.

This is \"guessed\" from `auto-mode-alist'.

This filter can potentially be very slow, depending on the size
of `auto-mode-alist'."
  (:description "mode"
   :qualifier-description (symbol-name (cadr qualifier))
   :reader (let ((mm (intern
                      (completing-read
                       "Major mode: "
                       (-map 'symbol-name (-uniq (-remove 'listp (-map 'cdr auto-mode-alist))))
                       nil nil nil nil
                       (-when-let* ((file (ignore-errors (dired-get-filename)))
                                    (mode (cdr (dired-utils-match-filename-regexp
                                                file auto-mode-alist))))
                         (symbol-name mode))))))
                          `',mm))
  (-when-let (mm (cdr (dired-utils-match-filename-regexp file-name auto-mode-alist)))
    (eq mm qualifier)))

;;;###autoload
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

;;;###autoload
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

;;;###autoload
(defun dired-filter-negate ()
  "Logically negate the top filter."
  (interactive)
  (let ((top (car dired-filter-stack)))
    (when (not top)
      (error "You need at least one filter on the stack"))
    (pop dired-filter-stack)
    (dired-filter--push `(not ,top))
    (dired-filter--update)))

;;;###autoload
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

;;;###autoload
(defun dired-filter-pop (&optional arg)
  "Remove the top filter in this buffer."
  (interactive "p")
  (while (and (> arg 0)
              dired-filter-stack)
    (let ((top (pop dired-filter-stack)))
      (when dired-filter-verbose
        (if top
            (--if-let (let ((qualifier (cdr top)))
                        (eval (caddr (assoc (car top) dired-filter-alist))))
                (message "Popped filter %s: %s" (car top) it)
              (message "Popped filter %s" (car top)))
          (message "Filter stack was empty."))))
    (setq arg (1- arg)))
  (dired-filter--update))

;;;###autoload
(defun dired-filter-pop-all ()
  "Remove all the filters in this buffer."
  (interactive)
  (setq dired-filter-stack nil)
  (dired-filter--update))


;; mode stuff
;;;###autoload
(define-minor-mode dired-filter-mode
  "Toggle filtering of files in Dired."
  :group 'dired-filter
  :lighter " Filter"
  (if dired-filter-mode
      (dired-filter--expunge)
    (revert-buffer)))

(add-hook 'dired-after-readin-hook 'dired-filter--expunge t)

(provide 'dired-filter)

;;; dired-filter.el ends here
