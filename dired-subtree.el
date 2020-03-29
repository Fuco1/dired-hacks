;;; dired-subtree.el --- Insert subdirectories in a tree-like fashion

;; Copyright (C) 2014-2015 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Keywords: files
;; Version: 0.0.1
;; Created: 25th February 2014
;; Package-Requires: ((dash "2.5.0") (dired-hacks-utils "0.0.1") (emacs "24.3"))
;; URL: https://github.com/Fuco1/dired-hacks

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

;; Introduction
;; ------------

;; The basic command to work with subdirectories in dired is `i',
;; which inserts the subdirectory as a separate listing in the active
;; dired buffer.

;; This package defines function `dired-subtree-insert' which instead
;; inserts the subdirectory directly below its line in the original
;; listing, and indent the listing of subdirectory to resemble a
;; tree-like structure (somewhat similar to tree(1) except the pretty
;; graphics).  The tree display is somewhat more intuitive than the
;; default "flat" subdirectory manipulation provided by `i'.

;; There are several presentation options and faces you can customize
;; to change the way subtrees are displayed.

;; You can further remove the unwanted lines from the subtree by using
;; `k' command or some of the built-in "focusing" functions, such as
;; `dired-subtree-only-*' (see list below).

;; If you have the package `dired-filter', you can additionally filter
;; the subtrees with global or local filters.

;; A demo of basic functionality is available on youtube:
;; https://www.youtube.com/watch?v=z26b8HKFsNE

;; Interactive functions
;; ---------------------

;; Here's a list of available interactive functions.  You can read
;; more about each one by using the built-in documentation facilities
;; of Emacs.  It is adviced to place bindings for these into a
;; convenient prefix key map, for example C-,

;; * `dired-subtree-insert'
;; * `dired-subtree-remove'
;; * `dired-subtree-toggle'
;; * `dired-subtree-cycle'
;; * `dired-subtree-revert'
;; * `dired-subtree-narrow'
;; * `dired-subtree-up'
;; * `dired-subtree-down'
;; * `dired-subtree-next-sibling'
;; * `dired-subtree-previous-sibling'
;; * `dired-subtree-beginning'
;; * `dired-subtree-end'
;; * `dired-subtree-mark-subtree'
;; * `dired-subtree-unmark-subtree'
;; * `dired-subtree-only-this-file'
;; * `dired-subtree-only-this-directory'

;; If you have package `dired-filter', additional command
;; `dired-subtree-apply-filter' is available.

;; See https://github.com/Fuco1/dired-hacks for the entire collection.

;;; Code:

(require 'dired-hacks-utils)
(require 'dash)
(require 'cl-lib)

(defgroup dired-subtree ()
  "Insert subdirectories in a tree-like fashion."
  :group 'dired-hacks
  :prefix "dired-subtree-")

(defcustom dired-subtree-line-prefix "  "
  "A prefix put into each nested subtree.

The prefix is repeated \"depth\" times.

Alternatively, it can be a function taking one argument---the
depth---that creates the prefix."
  :type '(choice string function)
  :group 'dired-subtree)

(defcustom dired-subtree-line-prefix-face 'parents
  "Specifies how the prefix is fontified."
  :type '(radio
          (const :tag "No face applied" nil)
          (const :tag "Inherit from current subtree" subtree)
          (const :tag "Inherit from all parents" parents))
  :group 'dired-subtree)

(defcustom dired-subtree-use-backgrounds t
  "When non-nil, add a background face to a subtree listing."
  :type 'boolean
  :group 'dired-subtree)

(defcustom dired-subtree-after-insert-hook ()
  "Hook run at the end of `dired-subtree-insert'."
  :type 'hook
  :group 'dired-subtree)

(defcustom dired-subtree-after-remove-hook ()
  "Hook run at the end of `dired-subtree-remove'."
  :type 'hook
  :group 'dired-subtree)

(defcustom dired-subtree-cycle-depth 3
  "Default depth expanded by `dired-subtree-cycle'."
  :type 'natnum
  :group 'dired-subtree)

(defcustom dired-subtree-ignored-regexp
  (concat "^" (regexp-opt vc-directory-exclusion-list) "$")
  "Matching directories will not be expanded in `dired-subtree-cycle'."
  :type 'regexp
  :group 'dired-subtree)

(defgroup dired-subtree-faces ()
  "Faces used in `dired-subtree'."
  :group 'dired-subtree)

(defface dired-subtree-depth-1-face
  '((t (:background "#252e30")))
  "Background for depth 1 subtrees"
  :group 'dired-subtree-faces)

(defface dired-subtree-depth-2-face
  '((t (:background "#232a2b")))
  "Background for depth 2 subtrees"
  :group 'dired-subtree-faces)

(defface dired-subtree-depth-3-face
  '((t (:background "#212627")))
  "Background for depth 3 subtrees"
  :group 'dired-subtree-faces)

(defface dired-subtree-depth-4-face
  '((t (:background "#1e2223")))
  "Background for depth 4 subtrees"
  :group 'dired-subtree-faces)

(defface dired-subtree-depth-5-face
  '((t (:background "#1c1d1e")))
  "Background for depth 5 subtrees"
  :group 'dired-subtree-faces)

(defface dired-subtree-depth-6-face
  '((t (:background "#1a191a")))
  "Background for depth 6 subtrees"
  :group 'dired-subtree-faces)

(defvar dired-subtree-overlays nil
  "Subtree overlays in this buffer.")
(make-variable-buffer-local 'dired-subtree-overlays)


;;; Overlay manipulation
;; Maybe we should abstract the overlay-foo into some subtree
;; functions instead!!!

(defun dired-subtree--remove-overlay (ov)
  "Remove dired-subtree overlay OV."
  (setq dired-subtree-overlays
        (--remove (equal it ov) dired-subtree-overlays))
  (delete-overlay ov))

(defun dired-subtree--remove-overlays (ovs)
  "Remove dired-subtree overlays OVS."
  (mapc 'dired-subtree--remove-overlay ovs))

(defun dired-subtree--cleanup-overlays ()
  "Remove the nil values from `dired-subtree-overlays'."
  (setq dired-subtree-overlays
        (--remove (not (overlay-buffer it)) dired-subtree-overlays)))

(defun dired-subtree--get-all-ovs ()
  "Get all dired-subtree overlays in this buffer."
  (--filter (overlay-get it 'dired-subtree-depth) (overlays-in (point-min) (point-max))))

(defun dired-subtree--get-all-ovs-at-point (&optional p)
  "Get all dired-subtree overlays at point P."
  (setq p (or p (point)))
  (--filter (overlay-get it 'dired-subtree-depth) (overlays-at (point))))

(defun dired-subtree--get-ovs-in (&optional beg end)
  "Get all dired-subtree overlays between BEG and END.

BEG and END default to the region spanned by overlay at point."
  (when (not beg)
    (let ((ov (dired-subtree--get-ov)))
      (setq beg (overlay-start ov))
      (setq end (overlay-end ov))))
  (--filter (and (overlay-get it 'dired-subtree-depth)
                 (>= (overlay-start it) beg)
                 (<= (overlay-end it) end))
            (overlays-in (point-min) (point-max))))

(defun dired-subtree--get-ov (&optional p)
  "Get the parent subtree overlay at point."
  (setq p (or p (point)))
  (car (--sort (> (overlay-get it 'dired-subtree-depth)
                  (overlay-get other 'dired-subtree-depth))
               (dired-subtree--get-all-ovs-at-point p))))

(defun dired-subtree--get-depth (ov)
  "Get subtree depth."
  (or (and ov (overlay-get ov 'dired-subtree-depth)) 0))



;;; helpers
(defvar dired-subtree-preserve-properties '(dired-subtree-filter)
  "Properties that should be preserved between read-ins.")

(defun dired-subtree--after-readin (&optional subtrees)
  "Insert the SUBTREES again after dired buffer has been reverted.

If no SUBTREES are specified, use `dired-subtree-overlays'."
  (-when-let (subtrees-to-process (or subtrees dired-subtree-overlays))
    (let* ((ovs-by-depth (--sort (< (car it) (car other))
                                 (--group-by (overlay-get it 'dired-subtree-depth)
                                             subtrees-to-process)))
           (sorted-ovs (--map (cons (car it)
                                    (--map (-cons* it
                                                   (overlay-get it 'dired-subtree-name)
                                                   (-map (lambda (x) (cons x (overlay-get it x)))
                                                         dired-subtree-preserve-properties)) (cdr it)))
                              ovs-by-depth)))
      ;; (depth (path1 ov1 (prop1 . value1) (prop2 . value2)) (path2 ...))
      (--each sorted-ovs
        (--each (cdr it)
          (when (dired-utils-goto-line (cadr it))
            (dired-subtree--remove-overlay (car it))
            (dired-subtree-insert)
            (let ((ov (dired-subtree--get-ov)))
              (--each (cddr it)
                (overlay-put ov (car it) (cdr it)))
              (dired-subtree--filter-subtree ov))))))))

(defun dired-subtree--after-insert ()
  "After inserting the subtree, setup dired-details/dired-hide-details-mode."
  (if (fboundp 'dired-insert-set-properties)
      (let ((inhibit-read-only t)
            (ov (dired-subtree--get-ov)))
        (dired-insert-set-properties (overlay-start ov) (overlay-end ov)))
    (when (featurep 'dired-details)
      (dired-details-delete-overlays)
      (dired-details-activate))))

(add-hook 'dired-after-readin-hook 'dired-subtree--after-readin)

(add-hook 'dired-subtree-after-insert-hook 'dired-subtree--after-insert)

(defun dired-subtree--unmark ()
  "Unmark a file without moving point."
  (save-excursion (dired-unmark 1)))

(defun dired-subtree--dired-line-is-directory-or-link-p ()
  "Return non-nil if line under point is a directory or symlink."
  ;; We've replaced `file-directory-p' with the regexp test to
  ;; speed up filters over TRAMP.  So long as dired/ls format
  ;; doesn't change, we're good.
  ;; 'd' for directories, 'l' for potential symlinks to directories.
  (save-excursion (beginning-of-line) (looking-at "..[dl]")))

(defun dired-subtree--is-expanded-p ()
  "Return non-nil if directory under point is expanded."
  (save-excursion
    (when (dired-utils-get-filename)
      (let ((depth (dired-subtree--get-depth (dired-subtree--get-ov))))
        (dired-next-line 1)
        (< depth (dired-subtree--get-depth (dired-subtree--get-ov)))))))

(defmacro dired-subtree-with-subtree (&rest forms)
  "Run FORMS on each file in this subtree."
  (declare (debug (body)))
  `(save-excursion
     (dired-subtree-beginning)
     ,@forms
     (while (dired-subtree-next-sibling)
       ,@forms)))


;;;; Interactive

;;;###autoload
(defun dired-subtree-narrow ()
  "Narrow the buffer to this subtree."
  (interactive)
  (-when-let (ov (dired-subtree--get-ov))
    (narrow-to-region (overlay-start ov)
                      (overlay-end ov))))

;;; Navigation

;; make the arguments actually do something
;;;###autoload
(defun dired-subtree-up (&optional arg)
  "Jump up one directory."
  (interactive "p")
  (-when-let (ov (dired-subtree--get-ov))
    (goto-char (overlay-start ov))
    (dired-previous-line 1)))

;;;###autoload
(defun dired-subtree-down (&optional arg)
  "Jump down one directory."
  (interactive "p")
  (-when-let* ((p (point))
               (ov (car (--sort
                         (< (overlay-start it)
                            (overlay-start other))
                         (--remove
                          (< (overlay-start it) p)
                          (dired-subtree--get-all-ovs))))))
    (goto-char (overlay-start ov))
    (dired-move-to-filename)))

;;;###autoload
(defun dired-subtree-next-sibling (&optional arg)
  "Go to the next sibling."
  (interactive "p")
  (let ((current-ov (dired-subtree--get-ov)))
    (dired-next-line 1)
    (let ((new-ov (dired-subtree--get-ov)))
      (cond
       ((not (dired-utils-is-file-p))
        nil)
       ((< (dired-subtree--get-depth current-ov)
           (dired-subtree--get-depth new-ov))
        (goto-char (overlay-end new-ov))
        (dired-move-to-filename)
        t)
       ((> (dired-subtree--get-depth current-ov)
           (dired-subtree--get-depth new-ov))
        ;; add option to either go to top or stay at the end
        (dired-previous-line 1)
        nil)
       (t t)))))

;;;###autoload
(defun dired-subtree-previous-sibling (&optional arg)
  "Go to the previous sibling."
  (interactive "p")
  (let ((current-ov (dired-subtree--get-ov)))
    (dired-previous-line 1)
    (let ((new-ov (dired-subtree--get-ov)))
      (cond
       ;; this will need better handlign if we have inserted
       ;; subdirectories
       ((not (dired-utils-is-file-p))
        nil)
       ((< (dired-subtree--get-depth current-ov)
           (dired-subtree--get-depth new-ov))
        (goto-char (overlay-start new-ov))
        (dired-previous-line 1)
        t)
       ((> (dired-subtree--get-depth current-ov)
           (dired-subtree--get-depth new-ov))
        ;; add option to either go to top or stay at the end
        (dired-next-line 1)
        nil)
       (t t)))))

;;;###autoload
(defun dired-subtree-beginning ()
  "Go to the first file in this subtree."
  (interactive)
  (let ((ov (dired-subtree--get-ov)))
    (if (not ov)
        ;; do something when not in subtree
        t
      (goto-char (overlay-start ov))
      (dired-move-to-filename))))

;;;###autoload
(defun dired-subtree-end ()
  "Go to the first file in this subtree."
  (interactive)
  (let ((ov (dired-subtree--get-ov)))
    (if (not ov)
        ;; do something when not in subtree
        t
      (goto-char (overlay-end ov))
      (dired-previous-line 1))))

;;; Marking

;;;###autoload
(defun dired-subtree-mark-subtree (&optional all)
  "Mark all files in this subtree.

With prefix argument mark all the files in subdirectories
recursively."
  (interactive "P")
  (save-excursion
    (if all
        (let ((beg (save-excursion
                     (dired-subtree-beginning)
                     (point)))
              (end (save-excursion
                     (dired-subtree-end)
                     (point))))
          (dired-mark-files-in-region
           (progn (goto-char beg) (line-beginning-position))
           (progn (goto-char end) (line-end-position))))
      (dired-subtree-beginning)
      (save-excursion (dired-mark 1))
      (while (dired-subtree-next-sibling)
        (save-excursion (dired-mark 1))))))

;;;###autoload
(defun dired-subtree-unmark-subtree (&optional all)
  "Unmark all files in this subtree.

With prefix argument unmark all the files in subdirectories
recursively."
  (interactive)
  (let ((dired-marker-char ? ))
    (dired-subtree-mark-subtree all)))

;;; Insertion/deletion
;;;###autoload
(defun dired-subtree-revert ()
  "Revert the subtree.

This means reinserting the content of this subtree and all its
children."
  (interactive)
  (let ((inhibit-read-only t)
        (file-name (dired-utils-get-filename)))
    (-when-let* ((ov (dired-subtree--get-ov))
                 (ovs (dired-subtree--get-ovs-in)))
      (dired-subtree-up)
      (delete-region (overlay-start ov) (overlay-end ov))
      (dired-subtree--after-readin ovs)
      (when file-name
        (dired-utils-goto-line file-name)))))

(defun dired-subtree--readin (dir-name)
  "Read in the directory.

Return a string suitable for insertion in `dired' buffer."
  (with-temp-buffer
    (insert-directory dir-name dired-listing-switches nil t)
    (delete-char -1)
    (goto-char (point-min))
    (delete-region
     (progn (beginning-of-line) (point))
     (progn (forward-line
             (if (save-excursion
                   (forward-line 1)
                   (end-of-line)
                   (looking-back "\\."))
                 3 1)) (point)))
    (insert "  ")
    (while (= (forward-line) 0)
      (insert "  "))
    (delete-char -2)
    (buffer-string)))

;;;###autoload
(defun dired-subtree-insert ()
  "Insert subtree under this directory."
  (interactive)
  (when (and (dired-subtree--dired-line-is-directory-or-link-p)
             (not (dired-subtree--is-expanded-p)))
    (let* ((dir-name (dired-get-filename nil))
           (listing (dired-subtree--readin (file-name-as-directory dir-name)))
           beg end)
      (read-only-mode -1)
      (move-end-of-line 1)
      ;; this is pretty ugly, I'm sure it can be done better
      (save-excursion
        (insert listing)
        (setq end (+ (point) 2)))
      (insert "\n")
      (setq beg (point))
      (let ((inhibit-read-only t))
        (remove-text-properties (1- beg) beg '(dired-filename)))
      (let* ((ov (make-overlay beg end))
             (parent (dired-subtree--get-ov (1- beg)))
             (depth (or (and parent (1+ (overlay-get parent 'dired-subtree-depth)))
                        1))
             (face (intern (format "dired-subtree-depth-%d-face" depth))))
        (when dired-subtree-use-backgrounds
          (overlay-put ov 'face face))
        ;; refactor this to some function
        (overlay-put ov 'line-prefix
                     (if (stringp dired-subtree-line-prefix)
                         (if (not dired-subtree-use-backgrounds)
                             (apply 'concat (-repeat depth dired-subtree-line-prefix))
                           (cond
                            ((eq nil dired-subtree-line-prefix-face)
                             (apply 'concat
                                    (-repeat depth dired-subtree-line-prefix)))
                            ((eq 'subtree dired-subtree-line-prefix-face)
                             (concat
                              dired-subtree-line-prefix
                              (propertize
                               (apply 'concat
                                      (-repeat (1- depth) dired-subtree-line-prefix))
                               'face face)))
                            ((eq 'parents dired-subtree-line-prefix-face)
                             (concat
                              dired-subtree-line-prefix
                              (apply 'concat
                                     (--map
                                      (propertize dired-subtree-line-prefix
                                                  'face
                                                  (intern (format "dired-subtree-depth-%d-face" it)))
                                      (number-sequence 1 (1- depth))))))))
                       (funcall dired-subtree-line-prefix depth)))
        (overlay-put ov 'dired-subtree-name dir-name)
        (overlay-put ov 'dired-subtree-parent parent)
        (overlay-put ov 'dired-subtree-depth depth)
        (overlay-put ov 'evaporate t)
        (push ov dired-subtree-overlays))
      (goto-char beg)
      (dired-move-to-filename)
      (read-only-mode 1)
      (when (bound-and-true-p dired-filter-mode) (dired-filter-mode 1))
      (run-hooks 'dired-subtree-after-insert-hook))))

;;;###autoload
(defun dired-subtree-remove ()
  "Remove subtree at point."
  (interactive)
  (-when-let* ((ov (dired-subtree--get-ov))
               (ovs (dired-subtree--get-ovs-in
                     (overlay-start ov)
                     (overlay-end ov))))
    (let ((inhibit-read-only t))
      (dired-subtree-up)
      (delete-region (overlay-start ov)
                     (overlay-end ov))
      (dired-subtree--remove-overlays ovs)))
  (run-hooks 'dired-subtree-after-remove-hook))

;;;###autoload
(defun dired-subtree-toggle ()
  "Insert subtree at point or remove it if it was not present."
  (interactive)
  (if (dired-subtree--is-expanded-p)
      (progn
        (dired-next-line 1)
        (dired-subtree-remove)
        ;; #175 fixes the case of the first line in dired when the
        ;; cursor jumps to the header in dired rather then to the
        ;; first file in buffer
        (when (bobp)
          (dired-next-line 1)))
    (save-excursion (dired-subtree-insert))))

(defun dired-subtree--insert-recursive (depth max-depth)
  "Insert full subtree at point."
  (save-excursion
    (let ((name (dired-get-filename nil t)))
      (when (and name (file-directory-p name)
                 (<= depth (or max-depth depth))
                 (or (= 1 depth)
                     (not (string-match-p dired-subtree-ignored-regexp
                                          (file-name-nondirectory name)))))
        (if (dired-subtree--is-expanded-p)
            (dired-next-line 1)
          (dired-subtree-insert))
        (dired-subtree-end)
        (dired-subtree--insert-recursive (1+ depth) max-depth)
        (while (dired-subtree-previous-sibling)
          (dired-subtree--insert-recursive (1+ depth) max-depth))))))

(defvar dired-subtree--cycle-previous nil
  "Remember previous action for `dired-subtree-cycle'")

;;;###autoload
(defun dired-subtree-cycle (&optional max-depth)
  "Org-mode like cycle visibility:

1) Show subtree
2) Show subtree recursively (if previous command was cycle)
3) Remove subtree

Numeric prefix will set max depth"
  (interactive "P")
  (save-excursion
    (cond
     ;; prefix - show subtrees up to max-depth
     (max-depth
      (when (dired-subtree--is-expanded-p)
        (dired-next-line 1)
        (dired-subtree-remove))
      (dired-subtree--insert-recursive 1 (if (integerp max-depth) max-depth nil))
      (setq dired-subtree--cycle-previous :full))
     ;; if directory is not expanded, expand one level
     ((not (dired-subtree--is-expanded-p))
      (dired-subtree-insert)
      (setq dired-subtree--cycle-previous :insert))
     ;; hide if previous command was not cycle or tree was fully expanded
     ((or (not (eq last-command 'dired-subtree-cycle))
          (eq dired-subtree--cycle-previous :full))
      (dired-next-line 1)
      (dired-subtree-remove)
      (setq dired-subtree--cycle-previous :remove))
     (t
      (dired-subtree--insert-recursive 1 dired-subtree-cycle-depth)
      (setq dired-subtree--cycle-previous :full)))))

(defun dired-subtree--filter-up (keep-dir kill-siblings)
  (save-excursion
    (let (ov)
      (save-excursion
        (while (dired-subtree-up))
        (dired-next-line 1)
        (dired-subtree-mark-subtree t))
      (if keep-dir
          (dired-subtree-unmark-subtree)
        (dired-subtree--unmark))
      (while (and (dired-subtree-up)
                  (> (dired-subtree--get-depth (dired-subtree--get-ov)) 0))
        (if (not kill-siblings)
            (dired-subtree--unmark)
          (dired-subtree--unmark)
          (let ((here (point)))
            (dired-subtree-with-subtree
             (when (and (dired-subtree--is-expanded-p)
                        (/= (point) here))
               (dired-subtree--unmark)
               (save-excursion
                 (dired-next-line 1)
                 (dired-subtree-unmark-subtree t)))))))
      (dired-do-kill-lines)
      (dired-subtree--cleanup-overlays))))

;;;###autoload
(defun dired-subtree-only-this-file (&optional arg)
  "Remove all the siblings on the route from this file to the top-most directory.

With ARG non-nil, do not remove expanded directories in parents."
  (interactive "P")
  (dired-subtree--filter-up nil arg))

;;;###autoload
(defun dired-subtree-only-this-directory (&optional arg)
  "Remove all the siblings on the route from this directory to the top-most directory.

With ARG non-nil, do not remove expanded directories in parents."
  (interactive "P")
  (dired-subtree--filter-up t arg))

;;; filtering
(defun dired-subtree--filter-update-bs (ov)
  "Update the local filter list.

This function assumes that `dired-filter-stack' is dynamically
bound to relevant value."
  (let* ((filt (dired-filter--describe-filters))
         (before-str (if (equal filt "") nil (concat "  Local filters: " filt "\n"))))
    (overlay-put ov 'before-string before-str)))

(defun dired-subtree--filter-subtree (ov)
  "Run the filter for this subtree.

It is only safe to call this from readin.

This depends on `dired-filter' package."
  (when (featurep 'dired-filter)
    (let ((dired-filter-stack (overlay-get ov 'dired-subtree-filter)))
      (save-restriction
        (widen)
        (dired-subtree-narrow)
        (dired-filter--expunge)
        (dired-subtree--filter-update-bs ov)))))

;;;###autoload
(defun dired-subtree-apply-filter ()
  "Push a local filter for this subtree.

This depends on `dired-filter' package.

It works exactly the same as global dired filters, only
restricted to a subtree.  The global filter is also applied to
the subtree.  The filter action is read from `dired-filter-map'."
  (interactive)
  (when (featurep 'dired-filter)
    (-when-let (ov (dired-subtree--get-ov))
      (let ((dired-filter-stack (overlay-get ov 'dired-subtree-filter))
            (glob (current-global-map))
            (loc (current-local-map))
            cmd)
        (cl-flet ((dired-filter--update
                   ()
                   (save-restriction
                     (overlay-put ov 'dired-subtree-filter dired-filter-stack)
                     (widen)
                     (dired-subtree-revert)
                     (dired-subtree--filter-update-bs ov))))
          (unwind-protect
              (progn
                (use-global-map dired-filter-map)
                (use-local-map nil)
                (setq cmd (key-binding (read-key-sequence "Choose filter action: "))))
            (use-global-map glob)
            (use-local-map loc))
          (let ((p (point))
                (beg (overlay-start ov))
                (current-file (dired-utils-get-filename)))
            (unwind-protect
                (call-interactively cmd)
              (unless (dired-utils-goto-line current-file)
                (goto-char beg)
                (forward-line)
                (goto-char (min p (1- (overlay-end (dired-subtree--get-ov)))))
                (dired-move-to-filename)))))))))


;;; Here we redefine a couple of functions from dired.el to make them
;;; subtree-aware

;; If the point is in a subtree, we need to provide a proper
;; directory, not the one that would come from `dired-subdir-alist'.
(defun dired-current-directory (&optional localp)
  "Return the name of the subdirectory to which this line belongs.
This returns a string with trailing slash, like `default-directory'.
Optional argument means return a file name relative to `default-directory'."
  (let ((here (point))
        (alist (or dired-subdir-alist
                   ;; probably because called in a non-dired buffer
                   (error "No subdir-alist in %s" (current-buffer))))
        elt dir)
    (while alist
      (setq elt (car alist)
            dir (car elt)
            ;; use `<=' (not `<') as subdir line is part of subdir
            alist (if (<= (dired-get-subdir-min elt) here)
                      nil       ; found
                    (cdr alist))))
    ;; dired-subdir: modify dir here if we are in a "subtree" view
    (-when-let (parent (dired-subtree--get-ov))
      (setq dir (concat (overlay-get parent 'dired-subtree-name) "/")))
    ;; end
    (if localp
        (dired-make-relative dir default-directory)
      dir)))

;; Since the tree-inserted directory is not in the dired-subdir-alist,
;; we need to guard against nil.
(defun dired-get-subdir ()
  "Return the subdir name on this line, or nil if not on a headerline."
  ;; Look up in the alist whether this is a headerline.
  (save-excursion
    (let ((cur-dir (dired-current-directory)))
      (beginning-of-line)       ; alist stores b-o-l positions
      (and (zerop (- (point)
                     (or (dired-get-subdir-min
                          (assoc cur-dir
                                 dired-subdir-alist))
                         0))) ;; dired-subtree: return zero if current
                              ;; dir is not in `dired-subdir-alist'.
           cur-dir))))

(provide 'dired-subtree)

;;; dired-subtree.el ends here
