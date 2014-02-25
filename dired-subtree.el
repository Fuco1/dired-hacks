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

The prefix is repeated \"depth\" times."
  :type 'string
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
  '((t (:background "#212526")))
  "Background for depth 3 subtrees"
  :group 'dired-subtree-faces)

(defvar dired-subtree-overlays nil
  "Subtree overlays in this buffer.")
(make-variable-buffer-local 'dired-subtree-overlays)

(defun dired-subtree--get-all-ovs ()
  "Get all dired-subtree overlays in this buffer."
  (--filter (overlay-get it 'dired-subtree-depth) (overlays-in (point-min) (point-max))))

(defun dired-subtree--get-all-ovs-at-point (&optional p)
  "Get all dired-subtree overlays at point P."
  (setq p (or p (point)))
  (--filter (overlay-get it 'dired-subtree-depth) (overlays-at (point))))

(defun dired-subtree--get-ov (&optional p)
  "Get the parent subtree overlay at point."
  (setq p (or p (point)))
  (car (--sort (> (overlay-get it 'dired-subtree-depth)
                  (overlay-get other 'dired-subtree-depth))
               (dired-subtree--get-all-ovs-at-point p))))

(defun dired-subtree--get-depth (ov)
  "Get subtree depth."
  (or (and ov (overlay-get ov 'dired-subtree-depth)) 0))

(defun dired-subtree--after-readin ()
  "Insert the subtrees again after dired buffer has been reverted."
  (when dired-subtree-overlays
    (let ((sorted-ovs (--map (cons (car it) (--map (overlay-get it 'dired-subtree-name) (cdr it)))
                             (--sort (< (car it) (car other))
                                     (--group-by (overlay-get it 'dired-subtree-depth)
                                                 dired-subtree-overlays)))))
      (--map (delete-overlay it) dired-subtree-overlays)
      (setq dired-subtree-overlays nil)
      (--each sorted-ovs
        (--each (cdr it)
          (dired-utils-goto-line it)
          (dired-subtree-insert))))))

(add-hook 'dired-after-readin-hook 'dired-subtree--after-readin)


;;;; Interactive
;; Maybe we should abstract the overlay-foo into some subtree
;; functions instead!!!

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
        (save-excursion (dired-mark 1)))))))

;;; Insertion/deletion

;;;###autoload
(defun dired-subtree-insert ()
  "Insert subtree under this directory."
  (interactive)
  (let* ((dir-name (dired-get-filename nil))
         (listing (with-temp-buffer
                    (insert-directory dir-name dired-listing-switches nil t)
                    (delete-char -1)
                    (goto-char (point-min))
                    (kill-line 3)
                    (insert "  ")
                    (while (= (forward-line) 0)
                      (insert "  "))
                    (delete-char -2)
                    (insert "\n")
                    (buffer-string))))
    (read-only-mode -1)
    (move-end-of-line 1)
    (forward-line)
    (let ((beg (point)))
      (insert listing)
      (let* ((end (point))
             (ov (make-overlay beg end))
             (parent (dired-subtree--get-ov (1- beg)))
             (depth (or (and parent (1+ (overlay-get parent 'dired-subtree-depth)))
                        1)))
        (when dired-subtree-use-backgrounds
          (overlay-put ov 'face (intern (format "dired-subtree-depth-%d-face" depth))))
        (overlay-put ov 'line-prefix (apply 'concat (-repeat depth "  ")))
        (overlay-put ov 'dired-subtree-name dir-name)
        (overlay-put ov 'dired-subtree-parent parent)
        (overlay-put ov 'dired-subtree-depth depth)
        (push ov dired-subtree-overlays))
      (goto-char beg)
      (dired-move-to-filename))
    (read-only-mode 1)))

;; TODO: remove children as well!
(defun dired-subtree-remove ()
  "Remove subtree at point."
  (interactive)
  (-when-let (ov (dired-subtree--get-ov))
    (let ((inhibit-read-only t))
      (setq dired-subtree-overlays
            (--remove (= (overlay-start it)
                         (overlay-start ov))
                      dired-subtree-overlays))
      (dired-subtree-up)
      (delete-region (overlay-start ov)
                     (overlay-end ov))
      (delete-overlay ov))))

(defun dired-subtree-only-this-file ()
  "Remove all the siblings on the route from this file to the top-most directory."
  (interactive)
  (save-excursion
    (let (ov)
      (while (setq ov (dired-subtree--get-ov))
        (dired-subtree-mark-subtree)
        (save-excursion (dired-unmark 1))
        (dired-do-kill-lines)
        (dired-subtree-up)))))

(defun dired-subtree-only-this-directory ()
  "Remove all the siblings on the route from this directory to the top-most directory."
  (interactive)
  (save-excursion
    (dired-subtree-up)
    (let (ov)
      (while (setq ov (dired-subtree--get-ov))
        (dired-subtree-mark-subtree)
        (save-excursion (dired-unmark 1))
        (dired-do-kill-lines)
        (dired-subtree-up)))))


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
