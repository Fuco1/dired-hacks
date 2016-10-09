;;; dired-filter.el --- Ibuffer-like filtering for dired

;; Copyright (C) 2014-2015 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Keywords: files
;; Version: 0.0.2
;; Created: 14th February 2014
;; Package-requires: ((dash "2.10.0") (dired-hacks-utils "0.0.1") (f "0.17.0") (cl-lib "0.3"))

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

;; To enable or disable the filters toggle minor mode
;; `dired-filter-mode'.  Toggling this mode preserves the filter
;; stack, so you can use it to quickly hide/unhide files filtered by
;; the current filter setup.

;; All the provided interactive functions are available from
;; `dired-filter-map'.  You can customize `dired-filter-prefix' to set
;; a prefix for this map or bind it manually to a prefix of your
;; choice using:
;;
;;     (define-key dired-mode-map (kbd "some-key") dired-filter-map)

;; The bindings follow a convention where the filters are mapped on
;; lower-case letters or punctuation, operators are mapped on symbols
;; (such as !, |, * etc.) and group commands are mapped on upper-case
;; letters.  The exception to this is `p' which is bound to
;; `dired-filter-pop', which is a very common operation and warrants a
;; quick binding.

;; In addition to filtering, you can also use the same predicates to
;; only mark files without removing the rest.  All the filtering
;; functions of the form `dired-filter-by-*' have their marking
;; counterpart `dired-filter-mark-by-*'.  These are available from
;; `dired-filter-mark-map'.  You can customize
;; `dired-filter-mark-prefix' a prefix for this map or bind it
;; manually to a prefix of your choice using:
;;
;;     (define-key dired-mode-map (kbd "some-key") dired-filter-mark-map)

;; The marking operations are not placed on stack, instead, the marks
;; are immediately updated by "OR"-ing them together.  To remove marks
;; that would otherwise be selected by a filter, use prefix argument
;; (usually bound to `C-u').  To logically negate the meaning of the
;; filter, you can call the function with a double prefix argument
;; (usually `C-u' `C-u')

;; You can use saved filters to mark files by calling
;; `dired-filter-mark-by-saved-filters'.

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

;; * `dired-filter-by-name'
;; * `dired-filter-by-regexp'
;; * `dired-filter-by-extension'
;; * `dired-filter-by-dot-files'
;; * `dired-filter-by-omit'
;; * `dired-filter-by-garbage'
;; * `dired-filter-by-predicate'
;; * `dired-filter-by-file'
;; * `dired-filter-by-directory'
;; * `dired-filter-by-mode'
;; * `dired-filter-by-symlink'
;; * `dired-filter-by-executable'

;; You can see their documentation by calling M-x `describe-function'.

;; Specifically, `dired-filter-by-omit' removes the files that would
;; be removed by `dired-omit-mode', so you should not need to use
;; both---in fact it is discouraged, as it would make the read-in
;; slower.

;; When called with negative prefix argument, some filters can read
;; multiple values.  The resulting predicate is often much faster than
;; having the filter repeated with single argument.  Read the
;; documentation to learn more about the calling conventions.
;; Currently, these filters support reading multiple arguments:

;; * `dired-filter-by-extension'

;; To define your own filters, you can use the macro
;; `dired-filter-define'.  If you define some interesting filter,
;; please consider contributing it to the upstream.

;;  Saved filters
;;  -------------

;; In addition to the built-in filters and your own custom filters,
;; this package provides an option to save complex compound filters
;; for later use.  When you set up a filter stack you would like to
;; save, call `dired-filter-save-filters'.  You will be prompted for a
;; name under which this stack will be saved.

;; The saved filter will be added to `dired-filter-saved-filters'
;; variable, which you can also customize via the customize interface
;; or manually add entries with `push' or `add-to-list'.  If you use
;; customize, calling `dired-filter-save-filters' will automatically
;; save the new value into your customize file.

;; You can delete saved filters with `dired-filter-delete-saved-filters'.

;; To use a saved filter, you can use either
;; `dired-filter-add-saved-filters' or
;; `dired-filter-load-saved-filters'.  The first pushes the saved
;; filter on top of the currently active stack, the second clears
;; current filter stack before loading the saved filter configuration.

;; An example use is to create filters for "logical groups" of files,
;; such as media files, image files or files used when programming in
;; certain environment (for example, show files with .h and .c
;; extensions).  Saved filters save you the time of setting up the
;; filters each time you want this specific view.

;; As a concrete example of above, author uses a saved filter "media"
;; with value:

;;     (extension "ogg" "flv" "mpg" "avi" "mp4" "mp3")
;;     ;; show all files matching any of these extensions

;;  Filter groups
;;  -------------

;; Furthermore, instead of only filtering the dired buffer by
;; removing lines you are not interested in, you can also group
;; lines together by filters.  That is, lines (files,
;; directories...) satisfying a filter will be moved together under
;; a common drawer.  This mechanism works in analogy with ibuffer
;; filter groups.

;; The variable `dired-filter-group-saved-groups' contains
;; definitions of filter groups.  You can create and save multiple
;; filter groups (views) and switch between them by setting the
;; `dired-filter-group' variable.

;; To enable or disable the filter groups toggle minor mode
;; `dired-filter-group-mode'.  Toggling this mode preserves the active
;; filter group so you can use it to quickly group and ungroup the
;; files.

;; Here is a screenshot with an active filter group.  Notice that regular
;; filtering works also with filter groups.

;; http://i.imgur.com/qtiDX1c.png

;; Placing the point on the drawer header and hitting `RET' folds it.
;; Hitting `RET' again expands it.

;; http://i.imgur.com/TDUsEKq.png

;; The `dired-filter-group-saved-groups' used in the above screenshot is the following:

;; (("default"
;;   ("PDF"
;;    (extension . "pdf"))
;;   ("LaTeX"
;;    (extension "tex" "bib"))
;;   ("Org"
;;    (extension . "org"))
;;   ("Archives"
;;    (extension "zip" "rar" "gz" "bz2" "tar"))))

;;  Other features
;;  --------------

;; You can clone the currently visible dired buffer by calling
;; `dired-filter-clone-filtered-buffer'.

;; See https://github.com/Fuco1/dired-hacks for the entire collection.

;;; Code:

(require 'dired-x)
(require 'dired-aux)
(require 'dired-hacks-utils)
(require 'dash)
(require 'thingatpt)
(require 'cl-lib)
(require 'f)

;; silence the compiler warning
(defvar dired-filter-mode nil)

(defvar dired-filter-marker-char ?\x3FF
  "Temporary marker used by Dired-Filter.")

(defvar dired-filter-alist nil
  "Definitions of filters.

Entries are of type (name desc body)")

(defgroup dired-filter ()
  "Ibuffer-like filtering for `dired'."
  :group 'dired-hacks
  :prefix "dired-filter-")

(defgroup dired-filter-group ()
  "Ibuffer-like filter groups for `dired-filter'."
  :group 'dired-filter
  :prefix "dired-filter-group")

(define-widget 'dired-filter 'lazy
  "A dired filter type."
  :tag "Filter"
  :type '(choice (cons :tag "Filter expression"
                   (symbol :tag "Filter")
                   (sexp :tag "Qualifier"))
                 (cons :tag "Logical OR of filters" (const or) (repeat dired-filter))
                 (list :tag "Logical negation of a filter" (const not) dired-filter)
                 (string :tag "Saved filter stack")))

(define-widget 'dired-filter-saved 'lazy
  "A named dired filter."
  :tag "Named filter"
  :type '(cons
           (string :tag "Filter name")
           (repeat :tag "Filter stack" dired-filter)))

(define-widget 'dired-filter-drawer 'lazy
  "A named dired filter drawer."
  :tag "Drawer"
  :type '(cons
          (string :tag "Drawer name")
          (repeat :tag "Filter stack" dired-filter)))

(defcustom dired-filter-stack '((omit))
  "Filter stack.

You can customize this variable to change what filters are active
when new dired buffer is created.

The stack is a list of conses where car is the symbol
representing the filter (it's the part of `dired-filter-by-...')
and cdr is current value of its argument, or nil if filter
doesn't take argument.

By default, `dired-filter-by-omit' is active."
  :type '(repeat dired-filter)
  :group 'dired-filter)
(make-variable-buffer-local 'dired-filter-stack)

(defcustom dired-filter-inherit-filter-stack nil
  "When non-nil, subdirectories inherit the filter of the parent directory."
  :type 'boolean
  :group 'dired-filter)

(defcustom dired-filter-save-with-custom t
  "When non-nil, use Custom to save interactively changed variables.

Currently, this only applies to `dired-filter-saved-filters'."
  :type 'boolean
  :group 'dired-filter)

(defcustom dired-filter-saved-filters nil
  "An alist of saved named filter."
  :type '(repeat :tag "Saved filters" dired-filter-saved)
  :group 'dired-filter)

(defcustom dired-filter-verbose t
  "If non-nil, print debug messages."
  :type 'boolean
  :group 'dired-filter)

(defcustom dired-filter-show-filters t
  "If non-nil, show a description of active filters in header line.

If `dired-filter-stack' is nil, no header is shown.

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

(defcustom dired-filter-keep-expanded-subtrees t
  "If non-nil, keep expanded subtree headers while filtering.

This setting has any effect only if `dired-subtree' is installed
as well."
  :type 'boolean
  :group 'dired-filter)

(defcustom dired-filter-revert 'ask
  "Should we revert the dired buffer when applying new filter?

The value 'never means never revert the buffer and add the filter
without refreshing the content.  This might provide incorrect
results if some previously filtered file should not be filtered
after the change.

The value 'always always reverts the buffer.

The value 'ask will ask if we should revert if the revert
function is non-standard, that is, not `dired-revert'.  This
means the dired buffer might come from `find-dired' or similar
operation and the reverting might be costly."
  :type '(radio
          (const :tag "Never revert automatically." never)
          (const :tag "Always revert automatically." always)
          (const :tag "Revert automatically only in standard dired buffers, ask otherwise." ask))
  :group 'dired-filter)

(defcustom dired-filter-group "default"
  "Active filter group.

Can be either a named filter group specified in
`dired-filter-group-saved-groups' or an anonymous filter stack."
  :type '(choice
          (string :tag "Filter group")
          (repeat :tag "Drawers" dired-filter-drawer))
  :group 'dired-filter-group)
(make-variable-buffer-local 'dired-filter-group)

(defcustom dired-filter-group-saved-groups '(("default"))
  "An alist of saved named filter groups.

A filter group is a list whose car is the name of the filter
group and whose cdr is a list of lists of the form
\(NAME . FILTER-STACK).

Each NAME defines the name of the drawer where files matching
FILTER-STACK are grouped together.

See `dired-filter-stack' for the format of FILTER-STACK."
  :type '(repeat
          :tag "Filter groups"
          (cons
           (string :tag "Filter group name")
           (repeat :tag "Drawers" dired-filter-drawer)))
  :group 'dired-filter-group)

(defface dired-filter-group-header
  '((t (:inherit header-line)))
  "The face used to highlight pair overlays."
  :group 'dired-filter-group)

;;;###autoload
(defvar dired-filter-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'dired-filter-by-name)
    (define-key map "r" 'dired-filter-by-regexp)
    (define-key map "." 'dired-filter-by-extension)
    (define-key map "h" 'dired-filter-by-dot-files) ;; hidden-files
    (define-key map "o" 'dired-filter-by-omit)
    (define-key map "g" 'dired-filter-by-garbage)
    (define-key map "e" 'dired-filter-by-predicate)
    (define-key map "f" 'dired-filter-by-file)
    (define-key map "d" 'dired-filter-by-directory)
    (define-key map "m" 'dired-filter-by-mode)
    (define-key map "s" 'dired-filter-by-symlink)
    (define-key map "x" 'dired-filter-by-executable)

    (define-key map "|" 'dired-filter-or)
    (define-key map "!" 'dired-filter-negate)
    (define-key map "*" 'dired-filter-decompose)
    (define-key map (kbd "TAB") 'dired-filter-transpose)
    (define-key map "p" 'dired-filter-pop)
    (define-key map "/" 'dired-filter-pop-all)

    (define-key map "S" 'dired-filter-save-filters)
    (define-key map "D" 'dired-filter-delete-saved-filters)
    (define-key map "A" 'dired-filter-add-saved-filters)
    (define-key map "L" 'dired-filter-load-saved-filters)
    map)
  "Keymap used for filtering files.")

;;;###autoload
(defvar dired-filter-mark-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'dired-filter-mark-by-name)
    (define-key map "r" 'dired-filter-mark-by-regexp)
    (define-key map "." 'dired-filter-mark-by-extension)
    (define-key map "h" 'dired-filter-mark-by-dot-files) ;; hidden-files
    (define-key map "o" 'dired-filter-mark-by-omit)
    (define-key map "g" 'dired-filter-mark-by-garbage)
    (define-key map "e" 'dired-filter-mark-by-predicate)
    (define-key map "f" 'dired-filter-mark-by-file)
    (define-key map "d" 'dired-filter-mark-by-directory)
    (define-key map "m" 'dired-filter-mark-by-mode)
    (define-key map "s" 'dired-filter-mark-by-symlink)
    (define-key map "x" 'dired-filter-mark-by-executable)
    (define-key map "L" 'dired-filter-mark-by-saved-filters)
    map)
  "Keymap used for marking files.")

(defun dired-filter--set-prefix-key (varname value)
  "Set VARNAME to VALUE.

Setter for `dired-filter-prefix' user variable."
  (when varname
    (set-default varname value))
  (when value
    (define-key dired-mode-map (read-kbd-macro value) dired-filter-map)))

(defun dired-filter--set-mark-prefix-key (varname value)
  "Set VARNAME to VALUE.

Setter for `dired-filter-mark-prefix' user variable."
  (when varname
    (set-default varname value))
  (when value
    (define-key dired-mode-map (read-kbd-macro value) dired-filter-mark-map)))

(defcustom dired-filter-prefix "/"
  "Prefix key for `dired-filter-map'."
  :type '(choice (string :tag "Prefix")
                 (const :tag "Don't set" nil))
  :group 'dired-filter
  :set 'dired-filter--set-prefix-key)

(defcustom dired-filter-mark-prefix nil
  "Prefix key for `dired-filter-mark-map'."
  :type '(choice (string :tag "Prefix")
                 (const :tag "Don't set" nil))
  :group 'dired-filter
  :set 'dired-filter--set-mark-prefix-key)

(defvar dired-filter-group-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<tab>") 'dired-filter-group-forward-drawer)
    (define-key map (kbd "<backtab>") 'dired-filter-group-backward-drawer)
    map)
  "Keymap used in `dired-filter-group-mode'.")


;; internals
(defun dired-filter--push (filter)
  "Push FILTER onto the active filter stack."
  (push filter dired-filter-stack))

;; TODO: save the filters in better structure to avoid undescriptive `cadddr'
(defun dired-filter--make-filter-1 (stack)
  "Translate STACK to a filter form."
  (cond
   ((stringp stack)
    `(and ,@(mapcar 'dired-filter--make-filter-1
                    (or (cdr (assoc stack dired-filter-saved-filters))
                        (error "saved filter %s does not exist" stack)))))
   ((stringp (car stack))
    `(and ,@(mapcar 'dired-filter--make-filter-1 (cdr stack))))
   ((eq (car stack) 'or)
    `(or ,@(mapcar 'dired-filter--make-filter-1 (cdr stack))))
   ((eq (car stack) 'not)
    `(not ,@(mapcar 'dired-filter--make-filter-1 (cdr stack))))
   (t (let* ((def (assoc (car stack) dired-filter-alist))
             (remove (cl-cadddr def))
             (qualifier (cond
                         ((eq (car stack) 'predicate)
                          (let* ((predicate (cdr stack))
                                 (predicate-keywords (mapcar (lambda (sym) (intern (concat ":" (symbol-name sym))))
                                                             (-filter 'symbolp (-flatten predicate))))
                                 (keywords (-intersection dired-utils-info-keywords predicate-keywords))
                                 (varlist (mapcar (lambda (keyword)
                                                    `(,(intern (substring (symbol-name keyword) 1))
                                                      (dired-utils-get-info ,keyword))) keywords)))
                            `'(let ,varlist
                                ,predicate)))
                         ((eq (car stack) 'omit)
                          ;; special hack for omit filter, to
                          ;; recompute the filter regexp
                          (dired-omit-regexp))
                         ((eq (car stack) 'extension)
                          (if (listp (cdr stack))
                              (concat "\\." (regexp-opt (-uniq (cdr stack))) "\\'")
                            (concat "\\." (regexp-quote (cdr stack)) "\\'")))
                         (t (cdr stack)))))
        (if qualifier
            `(let ((qualifier ,qualifier))
               ,(if remove
                    `(not ,(car (cl-cddddr def)))
                  (car (cl-cddddr def))))
          (if remove
              `(not ,(car (cl-cddddr def)))
            (car (cl-cddddr def))))))))

(defun dired-filter--make-filter (filter-stack)
  "Build the expression that filters the files according to FILTER-STACK.

When this expression evals to non-nil, file is kept in the
listing."
  `(and ,@(mapcar 'dired-filter--make-filter-1 filter-stack)))

(defun dired-filter--describe-filters-1 (stack)
  "Return a string describing STACK.

STACK is a filter stack with the format of `dired-filter-stack'."
  (cond
   ((stringp stack)
    (format "[Saved filter: %s]" stack))
   ((stringp (car stack))
    (format "[Saved filter: %s]" (car stack)))
   ((eq (car stack) 'or)
    (concat "[OR " (mapconcat 'dired-filter--describe-filters-1 (cdr stack) " ") "]"))
   ((eq (car stack) 'not)
    (concat "[NOT " (mapconcat 'dired-filter--describe-filters-1 (cdr stack) " ") "]"))
   (t (let* ((def (assoc (car stack) dired-filter-alist))
             (desc (cadr def))
             (desc-qual (cl-caddr def))
             (remove (if (cl-cadddr def) "!" ""))
             (qualifier (cdr stack))
             (qual-formatted (eval desc-qual)))
        (if qual-formatted
            (format "[%s%s: %s]" remove desc qual-formatted)
          (format "[%s%s]" remove desc))))))

(defun dired-filter--describe-filters ()
  "Return a string describing `dired-filter-stack'."
  (mapconcat 'dired-filter--describe-filters-1 dired-filter-stack " "))

(defun dired-filter--apply ()
  "Apply the filters and filter groups."
  (dired-filter--expunge)
  (dired-filter-group--apply
   (if (stringp dired-filter-group)
       (assoc dired-filter-group dired-filter-group-saved-groups)
     (cons "Anonymous" dired-filter-group))))

(defun dired-filter--update ()
  "Re-run the filters."
  (let ((file-name (dired-utils-get-filename)))
    (if (eq revert-buffer-function 'dired-revert)
        (revert-buffer)
      (if (cond
           ((eq dired-filter-revert 'never) nil)
           ((eq dired-filter-revert 'always) t)
           ((eq dired-filter-revert 'ask)
            (not (y-or-n-p "It appears the revert function for this dired buffer is non-standard.  Reverting might take a long time.
Do you want to apply the filters without reverting (this might provide incorrect results in some situations)?"))))
          (revert-buffer)
        (dired-filter--apply)))
    (if (and dired-filter-mode
             dired-filter-show-filters
             dired-filter-stack)
        (add-to-list 'header-line-format '("" dired-filter-header-line-format) t)
      (setq header-line-format (--remove (equal it '("" dired-filter-header-line-format)) header-line-format)))
    (when file-name
      (dired-utils-goto-line file-name))))

(defun dired-filter--narow-to-subdir ()
  "Narrow to subdir at point."
  (let ((beg (progn
               (dired-next-subdir 0)
               (line-beginning-position)))
        (end (progn
               (if (dired-next-subdir 1 t)
                   (line-beginning-position)
                 (point-max)))))
    (narrow-to-region beg end)))

(defun dired-filter--extract-lines (filter)
  "Extract all lines in the current directory matching FILTER.

The matched lines are returned as a string."
  (save-excursion
    (save-restriction
      (widen)
      (dired-filter--narow-to-subdir)
      (goto-char (point-min))
      (let* ((buffer-read-only nil)
             (filter (dired-filter--make-filter filter))
             (re nil))
        (while (not (eobp))
          (let ((file-name (dired-utils-get-filename 'no-dir)))
            (if (and file-name
                     (eval filter))
                (push (delete-and-extract-region
                       (line-beginning-position)
                       (progn (forward-line 1) (point)))
                      re)
              (forward-line 1))))
        (apply 'concat (nreverse re))))))

(defvar dired-filter-group-header-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'dired-filter-group-toggle-header)
    map)
  "Keymap used when over a group header.")

(defun dired-filter-group--make-header (name &optional collapsed)
  "Make group header named by NAME.

Optional argument COLLAPSED specifies if the header is collapsed
by default."
  (concat (propertize
           (concat
            "  "
            (propertize (format "[ %s%s ]" name (if collapsed " ..." ""))
                        'font-lock-face 'dired-filter-group-header))
           'keymap dired-filter-group-header-map
           'dired-filter-group-collapsed collapsed
           'dired-filter-group-header name)
          "\n"))

(defun dired-filter-group--apply (filter-group)
  "Apply FILTER-GROUP."
  (when (and dired-filter-group-mode
             dired-filter-group)
    (save-excursion
      (save-restriction
        (widen)
        (when (ignore-errors (dired-next-subdir 0))
          (goto-char (point-min))
          (let ((inhibit-read-only t))
            (while (--when-let (text-property-any (point-min) (point-max) 'font-lock-face 'dired-filter-group-header)
                     (goto-char it))
              (delete-region (line-beginning-position) (1+ (line-end-position))))
            (goto-char (point-min))
            (let ((next t))
              (while next
                (let ((name-group-alist nil))
                  (-when-let ((_ . filter-stacks) filter-group)
                    (dired-hacks-next-file)
                    (beginning-of-line)
                    (--each (reverse filter-stacks)
                      (-let* (((name . filter-stack) it)
                              ;; TODO: extract only from last line following the last
                              ;; filter group
                              (group (dired-filter--extract-lines filter-stack)))
                        (when (/= (length group) 0)
                          (push (cons name group) name-group-alist))))
                    (--each name-group-alist
                      (-let (((name . group) it))
                        (insert (dired-filter-group--make-header name) group)))
                    (when (and (text-property-any
                                (save-excursion (dired-next-subdir 0))
                                (point-max) 'font-lock-face 'dired-filter-group-header)
                               (save-excursion (backward-char 1) (dired-hacks-next-file)))
                      (insert (dired-filter-group--make-header "Default")))))
                (setq next (ignore-errors (dired-next-subdir 1))))))
          (when (featurep 'dired-details)
            (dired-details-delete-overlays)
            (dired-details-activate)))))))

(defun dired-filter-group-toggle-header ()
  "Collapse or expand a filter group."
  (interactive)
  (let ((inhibit-read-only t)
        (name (save-excursion
                (beginning-of-line)
                (get-text-property (point) 'dired-filter-group-header)))
        (collapsed (save-excursion
                     (beginning-of-line)
                     (get-text-property (point) 'dired-filter-group-collapsed)))
        (beg (save-excursion
               (forward-line 1)
               (point)))
        (end (save-excursion
               (end-of-line)
               (min (or (next-single-property-change (point) 'dired-filter-group-header)
                        (point-max))
                    (dired-subdir-max)))))
    (if collapsed
        (remove-text-properties beg end '(invisible))
      (put-text-property beg end 'invisible t))
    (save-excursion
      (-let [(beg . end) (bounds-of-thing-at-point 'line)] (delete-region beg end))
      (insert (dired-filter-group--make-header name (not collapsed))))))

(defun dired-filter-group-forward-drawer (&optional count)
  "Move point forward by COUNT drawers."
  (interactive "p")
  (--dotimes count
    (-when-let (next-drawer (progn
                              (end-of-line)
                              (or (next-single-property-change (point) 'dired-filter-group-header)
                                  (progn
                                    (goto-char (point-min))
                                    (next-single-property-change (point) 'dired-filter-group-header)))))
      (goto-char next-drawer)
      (forward-char 2))))

(defun dired-filter-group-backward-drawer (&optional count)
  "Move point backward by COUNT drawers."
  (interactive "p")
  (--dotimes count
    (-when-let (previous-drawer (progn
                              (beginning-of-line)
                              (or (previous-single-property-change (point) 'dired-filter-group-header)
                                  (progn
                                    (goto-char (point-max))
                                    (previous-single-property-change (point) 'dired-filter-group-header)))))
      (goto-char previous-drawer)
      (beginning-of-line)
      (forward-char 2))))

(defun dired-filter-group-get-groups ()
  "Return a hash table mapping drawer name to its files."
  (save-excursion
    (dired-next-subdir 0)
    (let ((groups (make-hash-table :test 'equal)))
      (while (dired-hacks-next-file)
        (let ((file (dired-utils-get-filename :local))
              (group (save-excursion
                       (dired-filter-group-backward-drawer 1)
                       (get-text-property (point) 'dired-filter-group-header))))
          (puthash group (cons file (gethash group groups nil)) groups)))
      (maphash (lambda (k v) (puthash k (nreverse (gethash k groups nil)) groups)) groups)
      groups)))

(defvar dired-filter--expanded-dirs nil
  "List of expanded subtrees.

This adds support for `dired-subtree' package.")

(defun dired-filter--expunge ()
  "Remove the files specified by `dired-filter-stack' from the listing."
  (interactive)
  (when (and dired-filter-mode
             dired-filter-stack)
    (let ((filter (dired-filter--make-filter dired-filter-stack))
          (dired-marker-char dired-filter-marker-char)
          (old-modified-p (buffer-modified-p))
          count)
      (when dired-filter-verbose (message "Filtering..."))
      (if (eval (dired-filter--mark-unmarked filter))
          (setq count (dired-do-kill-lines nil (if dired-filter-verbose "Filtered %d line%s." "")))
        (when dired-filter-verbose (message "Nothing to filter")))
      ;; we need to go over all the directories again and remove those
      ;; without children
      (when (and dired-filter-keep-expanded-subtrees
                 (featurep 'dired-subtree))
        (--each dired-filter--expanded-dirs
          (save-excursion
            (dired-utils-goto-line it)
            (when (and (/= (point) (point-max))
                       (not (dired-subtree--is-expanded-p)))
              (dired-kill-line)))))
      (set-buffer-modified-p (and old-modified-p
                                  (save-excursion
                                    (goto-char (point-min))
                                    (re-search-forward dired-re-mark nil t))))
      count)))

(defun dired-filter--mark-unmarked (filter)
  "Mark originally unmarked files according to FILTER.

If a file satisfies a filter, it is not marked.  Marked files are
removed when filtering.

Implementation note: when this function is called
`dired-marker-char' is set to a special value so that the regular
marks are preserved during filtering.  Files marked by user are
preserved even in case they should have been removed by the
filter"
  (if (and dired-filter-keep-expanded-subtrees
           (featurep 'dired-subtree))
      (progn
        (setq dired-filter--expanded-dirs nil)
        `(dired-mark-if
          (let ((file-name (dired-utils-get-filename 'no-dir)))
            (and
             file-name
             (looking-at " ")
             (if (dired-subtree--is-expanded-p)
                 (progn
                   (push (dired-get-filename) dired-filter--expanded-dirs)
                   nil)
               t)
             (not ,filter)))
          nil))
    `(dired-mark-if
      (let ((file-name (dired-utils-get-filename 'no-dir)))
        (and
         file-name
         (looking-at " ")
         (not ,filter)))
      nil)))

(defun dired-filter--mark (filter)
  "Helper used by `dired-filter-mark-by-' family.

This ORs the current selection with the one specified by selected filter.

If prefix argument \\[universal-argument] is used, unmark the matched files instead
\(including any previously marked files).

If prefix argument \\[universal-argument] \\[universal-argument] is used, mark the files that would normally
not be marked, that is, reverse the logical meaning of the
filter."
  (let* ((remove (cl-cadddr (assoc (car filter) dired-filter-alist)))
         (filter (if (equal current-prefix-arg '(16))
                     `(not ,(dired-filter--make-filter (list filter)))
                   (dired-filter--make-filter (list filter))))
         (filter (if remove `(not ,filter) filter))
         (dired-marker-char (if (equal current-prefix-arg '(4)) ?\040 dired-marker-char)))
    (eval `(dired-mark-if
            (let ((file-name (dired-utils-get-filename 'no-dir)))
              (and
               file-name
               ,filter))
            nil))))


;; filters & filter definitions
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
  (let ((fn-name (intern (concat "dired-filter-by-" (symbol-name name))))
        (fn-mark-name (intern (concat "dired-filter-mark-by-" (symbol-name name)))))
    `(progn
       (defun ,fn-name (&optional qualifier)
         ,(or (and documentation
                   (if remove
                       (concat documentation "\n\nBy default, files matched by this filter are /removed/.")
                     documentation))
              "This filter is not documented.")
         (interactive (list ,reader))
         (dired-filter--push (cons ',name qualifier))
         (when dired-filter-verbose
           (message "%s" (concat ,(format "Filter by %s added" description)
                                 (--if-let (eval ,qualifier-description)
                                     (format ": %s" it)
                                   ""))))
         (when (not dired-filter-mode)
           (dired-filter-mode 1))
         (dired-filter--update))
       (defun ,fn-mark-name (&optional qualifier)
         ,(or (and documentation
                   (concat (if remove
                               (concat documentation "\n\nBy default, files matched by this filter are /removed/.")
                             documentation)
                           "\n\nThis function only marks the matched files and does not filter the buffer view.

If prefix argument \\[universal-argument] is used, unmark the
matched files instead (including any perviously marked files).

If prefix argument \\[universal-argument] \\[universal-argument] is used, mark the files that would normally
not be marked, that is, reverse the logical meaning of the
filter."))
              "This matcher is not documented.")
         (interactive (list ,reader))
         (dired-filter--mark (cons ',name qualifier))
         (when dired-filter-verbose
           (let ((qual (--if-let (eval ,qualifier-description)
                           (format (if (equal current-prefix-arg '(16))
                                       ": (NOT %s)"
                                     ": %s") it)
                         "")))
             (message "%s" (concat
                            (if (equal current-prefix-arg '(4)) "Unm" "M")
                            (if (and (equal qual "")
                                     (equal current-prefix-arg '(16)))
                                ,(format "arked by (NOT %s)" description)
                              ,(format "arked by %s" description))
                            qual)))))
       (push (list ',name ,description ',qualifier-description
                   ,remove ',(if (= (length body) 1)
                                 `,(car body)
                               `(progn ,@body)))
             dired-filter-alist))))

;;;###autoload (autoload 'dired-filter-by-dot-files "dired-filter")
;;;###autoload (autoload 'dired-filter-mark-by-dot-files "dired-filter")
(dired-filter-define dot-files
    "Toggle current view to dot-files."
  (:description "dot-files"
   :reader nil
   :remove t)
  (string-match-p "^\\." file-name))

;;;###autoload (autoload 'dired-filter-by-name "dired-filter")
;;;###autoload (autoload 'dired-filter-mark-by-name "dired-filter")
(dired-filter-define name
    "Toggle current view to files matching QUALIFIER."
  (:description "name"
   :reader (regexp-quote (read-string "Pattern: ")))
  (string-match-p qualifier file-name))

;;;###autoload (autoload 'dired-filter-by-regexp "dired-filter")
;;;###autoload (autoload 'dired-filter-mark-by-regexp "dired-filter")
(dired-filter-define regexp
    "Toggle current view to files matching QUALIFIER as a regular expression."
  (:description "regexp"
   :reader (read-regexp "Regexp: " ))
  (string-match-p qualifier file-name))

;;;###autoload (autoload 'dired-filter-by-extension "dired-filter")
;;;###autoload (autoload 'dired-filter-mark-by-extension "dired-filter")
(dired-filter-define extension
    "Toggle current view to files with extension matching QUALIFIER.

With negative prefix argument, you can specify multiple
extensions.  If you want to filter by many extensions (that is,
match files with any of the specified extensions), this is much
prefered as the regexp will be optimized to match any of the
extensions and thus much faster than matching each extension
separately in turn and ORing the filters together."
  (:description "extension"
   :qualifier-description (format "%s" qualifier)
   :reader (let* ((file (dired-utils-get-filename))
                  (ext (and file (file-name-extension file)))
                  (exts
                   (->> (dired-utils-get-all-files 'no-dir)
                     (--group-by (file-name-extension it))
                     (-map 'car)
                     (-remove 'not))))
             (if (< (prefix-numeric-value current-prefix-arg) 0)
                 (let (extensions)
                   (while (condition-case nil
                              (progn
                                (push (completing-read "Extensions (C-g when done): "
                                                       exts
                                                       nil nil nil nil ext)
                                      extensions)
                                ;; in the second invocation, don't present a default choice
                                (setq ext nil)
                                ;; and remove the selected extension from the list of suggestions
                                (setq exts (--remove (equal (car extensions) it) exts)))
                            (quit nil)))
                   extensions)
               (completing-read
                "Extension: "
                exts
                nil nil nil nil ext))))
  (string-match-p qualifier file-name))

;;;###autoload (autoload 'dired-filter-by-omit "dired-filter")
;;;###autoload (autoload 'dired-filter-mark-by-omit "dired-filter")
(dired-filter-define omit
    "Toggle current view to files matched by `dired-omit-regexp'."
  (:description "omit"
   :qualifier-description nil
   :remove t)
  (string-match-p qualifier file-name))

;;;###autoload (autoload 'dired-filter-by-garbage "dired-filter")
;;;###autoload (autoload 'dired-filter-mark-by-garbage "dired-filter")
(dired-filter-define garbage
    "Toggle current view to files matched by `dired-garbage-files-regexp'."
  (:description "garbage"
   :qualifier-description nil
   :remove t)
  (string-match-p dired-garbage-files-regexp file-name))

;;;###autoload (autoload 'dired-filter-by-executable "dired-filter")
;;;###autoload (autoload 'dired-filter-mark-by-executable "dired-filter")
(dired-filter-define executable
    "Toggle current view to executable files."
  (:description "executable"
   :qualifier-description nil)
  (and (not (file-directory-p file-name))
       (file-executable-p file-name)))

;;;###autoload (autoload 'dired-filter-by-predicate "dired-filter")
;;;###autoload (autoload 'dired-filter-mark-by-predicate "dired-filter")
(dired-filter-define predicate
    "Toggle current view to files for which QUALIFIER returns non-nil.

QUALIFIER is a lisp sexp that can refer to the following variables:

    `name'   [string]  name of item
    `isdir'  [boolean] true if is a directory
    `issym'  [boolean] true if is a symbolic link
    `target' [string]  the linked-to name for symbolic links
    `nlinks' [integer] number of links to file
    `uid'    [integer] owner
    `gid'    [integer] group
    `atime'  [list]    access time as a list of integers (HIGH LOW USEC PSEC)
    `mtime'  [list]    modification time as a list of integers (HIGH LOW USEC PSEC)
    `ctime'  [list]    change time as a list of integers (HIGH LOW USEC PSEC)
    `size'   [integer] file size in bytes
    `modes'  [string]  file permission bits, e.g. \"-rw-r--r--\"
    `gidchg' [boolean] true if the file's gid would change if file were deleted and recreated
    `inode'  [integer] the inode of the file
    `devnum' [integer] filesystem device number

Examples:
  Mark zero-length files: `(equal 0 size)'
  Find files ending with \"elc\": `(s-ends-with? \"elc\" name)'
  Find files modified before the 01/02/2014: `(time-less-p mtime (date-to-time \"2014-02-01 00:00:00\"))'"
  (:description "predicate"
   :qualifier-description (format "%s" qualifier)
   :reader (let ((minibuffer-completing-symbol t))
             (read-from-minibuffer "Filter by predicate (form): "
                                   nil
                                   read-expression-map
                                   t
                                   'read-expression-history)))
  (eval qualifier))

;;;###autoload (autoload 'dired-filter-by-directory "dired-filter")
;;;###autoload (autoload 'dired-filter-mark-by-directory "dired-filter")
(dired-filter-define directory
    "Toggle current view to show only directories."
  (:description "directory")
  (or (looking-at dired-re-dir)
      (and (looking-at dired-re-sym)
           (file-directory-p (dired-utils-get-filename)))))

;;;###autoload (autoload 'dired-filter-by-file "dired-filter")
;;;###autoload (autoload 'dired-filter-mark-by-file "dired-filter")
(dired-filter-define file
    "Toggle current view to show only files."
  (:description "file")
  (looking-at "^[* ] -"))

;;;###autoload (autoload 'dired-filter-by-mode "dired-filter")
;;;###autoload (autoload 'dired-filter-mark-by-mode "dired-filter")
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
                       (-when-let* ((file (dired-utils-get-filename))
                                    (mode (cdr (dired-utils-match-filename-regexp
                                                file auto-mode-alist))))
                         (symbol-name mode))))))
                          `',mm))
  (-when-let (mm (cdr (dired-utils-match-filename-regexp file-name auto-mode-alist)))
    (eq mm qualifier)))

;;;###autoload (autoload 'dired-filter-by-symlink "dired-filter")
;;;###autoload (autoload 'dired-filter-mark-by-symlink "dired-filter")
(dired-filter-define symlink
    "Toggle current view to show only symbolic links."
  (:description "symlink")
  (file-symlink-p file-name))

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
     ((and (eq (car-safe top) 'or)
           (eq (car-safe top2) 'or))
      (dired-filter--push (append '(or) (cdr top) (cdr top2))))
     ((eq (car-safe top) 'or)
      (dired-filter--push (append '(or) (cdr top) `(,top2))))
     ((eq (car-safe top2) 'or)
      (dired-filter--push (append `(or ,top) (cdr top2))))
     (t (dired-filter--push `(or ,top ,top2))))
    (dired-filter--update)))

(defun dired-filter--negated-p (filter)
  "Return t if FILTER is negated, otherwise nil."
  (and (> (safe-length filter) 1)
       (eq 'not (car filter))))

;;;###autoload
(defun dired-filter-negate ()
  "Logically negate the top filter."
  (interactive)
  (let ((top (car dired-filter-stack)))
    (when (not top)
      (error "You need at least one filter on the stack"))
    (pop dired-filter-stack)
    (if (dired-filter--negated-p top)
        (dired-filter--push (cadr top))
      (dired-filter--push `(not ,top)))
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
            (if (stringp top)
                (message "Popped saved filter %s" top)
              (--if-let (let ((qualifier (cdr top)))
                          (eval (cl-caddr (assoc (car top) dired-filter-alist))))
                  (message "Popped filter %s: %s" (car top) it)
                (message "Popped filter %s" (car top))))
          (message "Filter stack was empty."))))
    (setq arg (1- arg)))
  (dired-filter--update))

;;;###autoload
(defun dired-filter-pop-all ()
  "Remove all the filters in this buffer."
  (interactive)
  (setq dired-filter-stack nil)
  (dired-filter--update))


;; filter groups
(defun dired-filter--maybe-save-stuff ()
  (when dired-filter-save-with-custom
    (if (not (fboundp 'customize-save-variable))
        (message "Not saved permanently: Customize not available")
      (customize-save-variable 'dired-filter-saved-filters
                               dired-filter-saved-filters))))

;;;###autoload
(defun dired-filter-save-filters (name filters)
  "Save the the FILTERS in this dired buffer under a NAME for later use."
  (interactive
   (if (not dired-filter-stack)
       (error "No filters currently in effect")
     (list
      (read-string "Save current filters as: ")
      dired-filter-stack)))
  (--if-let (assoc name dired-filter-saved-filters)
      (when (y-or-n-p "Filter with such name already exist; overwrite? ")
        (setcdr it filters))
    (push (cons name filters) dired-filter-saved-filters))
  (dired-filter--maybe-save-stuff))

(defun dired-filters--read-saved-filter-name (prompt)
  "Read saved filter name."
  (list
   (if (not dired-filter-saved-filters)
       (error "No saved filters")
     (completing-read (concat prompt " saved filters: ")
                      dired-filter-saved-filters nil t nil nil
                      (caar dired-filter-saved-filters)))))

;;;###autoload
(defun dired-filter-delete-saved-filters (name)
  "Delete saved filters with NAME from `dired-filter-saved-filters'."
  (interactive (dired-filters--read-saved-filter-name "Delete"))
  (setq dired-filter-saved-filters
        (--remove (equal name (car it)) dired-filter-saved-filters))
  (dired-filter--maybe-save-stuff))

;;;###autoload
(defun dired-filter-load-saved-filters (name)
  "Set this buffer's filters to filters with NAME from `dired-filter-saved-filters'."
  (interactive (dired-filters--read-saved-filter-name "Load"))
  (--when-let (assoc name dired-filter-saved-filters)
    (setq dired-filter-stack (list it))
    (unless dired-filter-mode
      (dired-filter-mode 1))
    (dired-filter--update)))

;;;###autoload
(defun dired-filter-add-saved-filters (name)
  "Add to this buffer's filters filters with NAME from `dired-filter-saved-filters'."
  (interactive (dired-filters--read-saved-filter-name "Add"))
  (--when-let (assoc name dired-filter-saved-filters)
    (push it dired-filter-stack)
    (unless dired-filter-mode
      (dired-filter-mode 1))
    (dired-filter--update)))


;; dired filter groups
(defun dired-filter-group-load-group (name)
  "Load a filter group from `dired-filter-group-saved-groups'."
  (interactive (list
                (if (not dired-filter-group-saved-groups)
                    (error "No saved filter groups")
                  (completing-read "Load filter group: "
                                   dired-filter-group-saved-groups nil t nil nil
                                   (caar dired-filter-group-saved-groups)))))
  (setq dired-filter-group name))

(define-minor-mode dired-filter-group-mode
  "Toggle filter grouping of files in Dired."
  :group 'dired-filter-group
  :lighter ""
  :keymap dired-filter-group-mode-map
  (if dired-filter-group-mode
      (dired-filter--apply)
    (revert-buffer)))


;; other interactive functions
(defun dired-filter-clone-filtered-buffer (&optional name)
  "Clone the currently filtered view of the dired buffer.

A new dired buffer with just the visible files is created, with
default filters.

If the prefix argument is non-nil, you will be prompted for a
name, otherwise a disambiguation cookie <number> is appended
after the current buffer's name."
  (interactive (list (if current-prefix-arg
                         (read-string "New buffer name: " (buffer-name))
                       (generate-new-buffer-name (buffer-name)))))
  (dired (cons name (dired-utils-get-all-files t))))

(defun dired-filter-mark-by-saved-filters (filter)
  "Mark files using saved FILTER.

The marking logic regarding prefix arguments is exactly the same
as that of `dired-filter-mark-by-name'."
  (interactive (dired-filters--read-saved-filter-name "Load"))
  (--when-let (assoc filter dired-filter-saved-filters)
    (dired-filter--mark it)))


;; mode stuff
;;;###autoload
(define-minor-mode dired-filter-mode
  "Toggle filtering of files in Dired.

When you toggle the filter mode, the filter stack and all other
state is preserved, except the display is not altered.  This
allows you to quickly toggle the active filter without need of
popping the stack and then re-inserting the filters again."
  :group 'dired-filter
  :lighter " Filter"
  (if dired-filter-mode
      (progn
        (when dired-filter-inherit-filter-stack
          (-when-let (parent (cdr (--first (f-same? (f-parent default-directory) (car it)) dired-buffers)))
            (setq dired-filter-stack (with-current-buffer parent dired-filter-stack))))
        (if (and dired-filter-show-filters
                 dired-filter-stack)
            (add-to-list 'header-line-format '("" dired-filter-header-line-format) t))
        (dired-filter--apply))
    (setq header-line-format (--remove (equal it '("" dired-filter-header-line-format)) header-line-format))
    (revert-buffer)))

(add-hook 'dired-after-readin-hook 'dired-filter--apply t)

(provide 'dired-filter)

;; Local Variables:
;; eval: (font-lock-add-keywords nil '(("(\\(dired-filter-define\\)[[:blank:]]+\\(.+\\)" (1 'font-lock-keyword-face) (2 'font-lock-function-name-face))))
;; End:

;;; dired-filter.el ends here
