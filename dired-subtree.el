;;; dired-subtree.el --- Insert subdirectories in a tree-like fashion

;; Copyright (C) 2014 Matus Goljer

;; Author: Matus Goljer <matus.goljer@gmail.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Keywords: files
;; Version: 0.0.1
;; Created: 25th February 2014
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

;; See https://github.com/Fuco1/dired-hacks for the entire collection.

;;; Code:

(require 'dired-hacks-utils)
(require 'dash)

(defgroup dired-subtree ()
  "Insert subdirectories in a tree-like fashion."
  :group 'dired-hacks
  :prefix "dired-subtree-")

(defcustom dired-subtree-line-prefix "  "
  "A prefix put into each nested subtree.

The prefix is repeated \"depth\" times.

Alternatively, it can be a function taking one argument---the
depth---taht creates the prefix."
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
  "Remove the `nil' values from `dired-subtree-overlays'."
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

(defun dired-subtree--get-depth (&optional ov)
  "Get subtree depth."
  (setq ov (or ov (dired-subtree--get-ov)))
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
                                    (--map (cons (overlay-get it 'dired-subtree-name)
                                                 (-map (lambda (x) (cons x (overlay-get it x)))
                                                       dired-subtree-preserve-properties)) (cdr it)))
                              ovs-by-depth)))
      (dired-subtree--remove-overlays subtrees-to-process)
      (--each sorted-ovs
        (--each (cdr it)
          (dired-utils-goto-line (car it))
          (dired-subtree-insert)
          (let ((ov (dired-subtree--get-ov)))
            (--each (cdr it)
              (overlay-put ov (car it) (cdr it)))
            (dired-subtree--filter-subtree ov)))))))

(add-hook 'dired-after-readin-hook 'dired-subtree--after-readin)

(defun dired-subtree--unmark ()
  "Unmark a file without moving point."
  (save-excursion (dired-unmark 1)))

(defun dired-subtree--is-expanded-p ()
  "Return non-nil if directory under point is expanded."
  (save-excursion
    (-when-let (file (ignore-errors (dired-get-filename)))
      (and (file-directory-p file)
           (let ((depth (dired-subtree--get-depth)))
             (dired-next-line 1)
             (< depth (dired-subtree--get-depth)))))))


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
  (let ((ov (dired-subtree--get-ov)))
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
  (let ((dired-marker-char ? ))
    (dired-subtree-mark-subtree all)))

;;; Insertion/deletion
(defun dired-subtree--revert ()
  "Revert the subtree.

This means reinserting the content of this subtree and all its
children."
  (let ((inhibit-read-only t)
        (file-name (ignore-errors (dired-get-filename))))
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
    (kill-line 3)
    (insert "  ")
    (while (= (forward-line) 0)
      (insert "  "))
    (delete-char -2)
    (buffer-string)))

;;;###autoload
(defun dired-subtree-insert ()
  "Insert subtree under this directory."
  (interactive)
  (let* ((dir-name (dired-get-filename nil))
         (listing (dired-subtree--readin dir-name))
         beg end)
    (read-only-mode -1)
    (move-end-of-line 1)
    ;; this is pretty ugly, I'm sure it can be done better
    (save-excursion
      (insert listing)
      (setq end (+ (point) 2)))
    (newline)
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
    (read-only-mode 1)))

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
      (dired-subtree--remove-overlays ovs))))

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
                  (> (dired-subtree--get-depth) 0))
        (if (not arg)
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
  ;;"Return the subdir name on this line, or nil if not on a headerline."
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
