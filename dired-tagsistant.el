;;; dired-tagsistant.el --- Tagsistant support for dired

;; Copyright (C) 2014 Matúš Goljer <matus.goljer@gmail.com>

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Version: 0.0.1
;; Created: 14th February 2014
;; Package-requires: ((dash "2.8.0") (dired-hacks-utils "0.0.1") (f "0.16") (s "1.7.0"))
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

;; See also http://tagsistant.net/

;;; Code:

(require 'dired-hacks-utils)
(require 'dash)
(require 'f)
(require 's)

(defgroup dired-tagsistant ()
  "Tagsistant support for dired."
  :group 'dired-hacks
  :prefix "dired-tagsistant-")

(defcustom dired-tagsistant-root "~/files"
  "Root where the tagsistant virtual filesystem is mounted."
  :type 'directory
  :group 'dired-tagsistant)

(defun dired-tagsistant-root ()
  "Return normalized value of `dired-tagsistant-root'."
  (file-truename (concat dired-tagsistant-root "/")))

(defcustom dired-tagsistant-better-header t
  "If non-nil, hide the tagsistant-specific noise in the header."
  :type 'boolean
  :group 'dired-tagsistant)


;; Better header display

(defun dired-tagsistant--better-header ()
  (save-excursion
    (when dired-tagsistant-better-header
      (goto-char (point-min))
      (save-match-data
        (when (search-forward (file-truename dired-tagsistant-root) nil t)
          (let ((inhibit-read-only t)
                (header (progn
                          (beginning-of-line)
                          (cond
                           ((save-excursion
                              (re-search-forward
                               (concat (dired-tagsistant-root)
                                       "store/\\(.*?\\)/@:")
                               nil t))
                            ;; TODO: Add nicer query formatting
                            (format "Query: %s" (match-string 1)))
                           ((save-excursion
                              (re-search-forward
                               (concat (dired-tagsistant-root)
                                       "store/\\(.*?\\)/@@:")
                               nil t))
                            ;; TODO: Add nicer query formatting
                            (format "Query (no resolver): %s" (match-string 1)))
                           ((save-excursion
                              (re-search-forward
                               (concat (dired-tagsistant-root)
                                       "\\(store.*?:$\\)")
                               nil t))
                            (format "Tagsistant: %s" (match-string 1)))))))
            (when header
              (put-text-property (match-beginning 0) (match-end 0) 'display header))))))))

(add-hook 'dired-after-readin-hook 'dired-tagsistant--better-header)


;; Helpers

(defun dired-tagsistant--path (dir fragments)
  "Construct a tagsistant path.

DIR is a directory under `dired-tagsistant-root'.

FRAGMENTS are parts of the path which will be joined with /."
  (let ((re (concat (dired-tagsistant-root) dir "/" (s-join "/" fragments))))
    (if (s-ends-with? "/" re) re (concat re "/"))))

(defun dired-tagsistant--store (&rest fragments)
  "Return the store directory.

Join FRAGMENTS by adding / between each two items, then append to
the end."
  (dired-tagsistant--path "store" fragments))

(defun dired-tagsistant--tags (&rest fragments)
  "Return the tags directory.

Join FRAGMENTS by adding / between each two items, then append to
the end."
  (dired-tagsistant--path "tags" fragments))

(defun dired-tagsistant--relations (&rest fragments)
  "Return the relations directory.

Join FRAGMENTS by adding / between each two items, then append to
the end."
  (dired-tagsistant--path "relations" fragments))

(defun dired-tagsistant--namespace-p (tag)
  "Return non-nil if TAG is a namespace tag."
  (s-ends-with? ":" tag))

(defun dired-tagsistant--get-tags (&optional no-namespaces)
  "Return a list of all available tags.

If NO-NAMESPACES is non-nil, do not return namespace tags."
  (let ((tagdir (dired-tagsistant--tags)))
    (--map (s-chop-prefix tagdir it)
           (let ((tags (f-directories tagdir)))
             (if no-namespaces
                 (-remove 'dired-tagsistant--namespace-p tags)
               tags)))))

(defun dired-tagsistant--get-namespace-keys (namespace)
  "Return a list of all keys in NAMESPACE."
  (let ((tagdir (dired-tagsistant--tags namespace)))
    (--map (s-chop-prefix tagdir it) (f-directories tagdir))))

(defun dired-tagsistant--get-namespace-key-values (namespace key)
  (let ((tagdir (dired-tagsistant--tags namespace key)))
    (--map (s-chop-prefix tagdir it) (f-directories tagdir))))

(defun dired-tagsistant--create-tag-maybe (tag &optional key value)
  "Create TAG if it does not exist yet.

If TAG is a namespace tag, create KEY if non-nil and VALUE if
non-nil as well."
  (let* ((parts (-remove 'null (list tag key value)))
         (path (apply 'dired-tagsistant--tags parts)))
    (unless (f-directory? path)
      (make-directory path t))))

(defun dired-tagsistant--get-files-tags (files)
  "Return an alist mapping each file in FILES to a set of its tags."
  (--map
   (cons it (with-temp-buffer
              (shell-command
               (concat "cat "
                       (shell-quote-argument it)
                       ".tags | tr -d '\\0' | sort | uniq")
               (current-buffer))
              (s-split "\n" (buffer-string) :omit-nulls)))
   files))


;; Readers

(defvar dired-tagsistant--read-history nil
  "History of tags read from the user.")

;; TODO: add prompt argument.
(defun dired-tagsistant--read-tags ()
  "Read tags interactively from user."
  (let (re tag (tags (dired-tagsistant--get-tags)))
    (while (not (string= "" tag))
      (setq tag (completing-read
                 (format "Tags %s(hit RET to end): "
                         (if re (format "[%s] " (s-join ", " (reverse re))) ""))
                 tags nil 'confirm nil 'dired-tagsistant--read-history))
      (if (dired-tagsistant--namespace-p tag)
          (progn
            (setq tag (s-join "/" (cons tag (dired-tagsistant--read-tripple-tag tag))))
            (pop dired-tagsistant--read-history)
            (push tag dired-tagsistant--read-history))
        (setq tags (--remove (equal tag it) tags)))
      (push tag re))
    (nreverse (cdr re))))

(defun dired-tagsistant--read-tripple-tag (namespace)
  "Read key, operator and value in NAMESPACE."
  (let* ((key (let ((namespaces (dired-tagsistant--get-namespace-keys namespace)))
                (completing-read
                 (format "Key [%s]: " namespace)
                 namespaces nil t nil nil (car namespaces))))
         (op (completing-read (format "Operator [%s/%s]: " namespace key)
                              '("eq" "inc" "gt" "lt") nil t nil nil "eq"))
         (value (let ((values (dired-tagsistant--get-namespace-key-values namespace key)))
                  (completing-read
                   (format "Value [%s/%s/%s]: " namespace op key)
                   values
                   nil nil nil nil (car values)))))
    (list key op value)))


;; Basic queries

;;;###autoload
(defun dired-tagsistant-some-tags (tags)
  "Display all files matching some tag in TAGS."
  (interactive (list (dired-tagsistant--read-tags)))
  (find-file (dired-tagsistant--store (s-join "/+/" tags) "@")))

;;;###autoload
(defun dired-tagsistant-all-tags (tags)
  "Display all files matching all tags in TAGS."
  (interactive (list (dired-tagsistant--read-tags)))
  (find-file (dired-tagsistant--store (s-join "/" tags) "@")))

;;;###autoload
(defun dired-tagsistant-some-tags-regexp (regexp)
  "Display all files where some of their tags matches REGEXP."
  (interactive "sRegexp: ")
  (let* ((tags (--filter (string-match-p regexp it) (dired-tagsistant--get-tags :no-namespaces))))
    (dired-tagsistant-some-tags tags)))

;;;###autoload
(defun dired-tagsistant-all-tags-regexp (regexp)
  "Display all files where all of their tags match REGEXP."
  (interactive "sRegexp: ")
  (let* ((tags (--filter (string-match-p regexp it) (dired-tagsistant--get-tags :no-namespaces))))
    (dired-tagsistant-all-tags tags)))

;;;###autoload
(defun dired-tagsistant-list-tags (files)
  "Print all tags on each file of FILES.

If FILES contains only one file, print in minibuffer, otherwise
pop a window with a list of all tags for each file."
  (interactive (list (dired-get-marked-files)))
  (let ((tags (dired-tagsistant--get-files-tags files)))
    (if (not (cdr files))
        (message "%s | %s" (f-filename (caar tags)) (s-join ", " (cdar tags)))
      (pop-to-buffer
       (with-current-buffer (get-buffer-create "*dired-tagsistant-tags*")
         (read-only-mode -1)
         (erase-buffer)
         (insert "|---+---|\n| File | Tags |\n|---+---|\n")
         (--each tags
           (insert "| " (f-filename (car it)) " | " (s-join ", " (cdr it)) " |\n"))
         (insert "|---+---|")
         (goto-char (point-min))
         (org-table-align)
         (special-mode)
         (current-buffer))))))


;; Tagging

(defun dired-tagsistant--tag (files tags method)
  "Tag FILES with TAGS using METHOD.

FILES is a list of files to tag.

TAGS is a list of tags to assign to the files.  Each tripple tag
should be represented by one string.

METHOD can be either :copy or :symlink."
  ;; create tags that do not exist
  (--each tags
    (cond
     ;; tripple tag
     ((s-matches? "/" it)
      (let ((parts (-select-by-indices '(0 1 3) (s-split "/" it))))
        (apply 'dired-tagsistant--create-tag-maybe parts)))
     (:else (dired-tagsistant--create-tag-maybe it))))
  ;; tag the files
  (let* ((store (dired-tagsistant--store (s-join "/" tags) "@@"))
         (reporter (make-progress-reporter "Tagging files" 0 (length files))))
    (--each files
      (cond
       ((eq method :symlink)
        (make-symbolic-link (f-canonical it) store))
       ((eq method :copy)
        (cond
         ((f-directory? it)
          (copy-directory it store))
         (:else
          (copy-file it store))))
       (:else (error "Unknown method")))
      (progress-reporter-update reporter it-index))
    (progress-reporter-done reporter)))

;;;###autoload
(defun dired-tagsistant-tag (files tags)
  "Tag FILES with TAGS by copying them into tagsistant store.

FILES is a list of files to tag.

TAGS is a list of tags to assign to the files.  Each tripple tag
should be represented by one string.  Non-existing tags will be
created automatically."
  (interactive (list (dired-get-marked-files)
                     (dired-tagsistant--read-tags)))
  ;; TODO: when in a query, also copy the query string to destination
  ;; and :rename, so we keep the original tag, add new ones and do not
  ;; copy the files around needlessly
  (dired-tagsistant--tag files tags :copy))

;;;###autoload
(defun dired-tagsistant-tag-symlink (files tags)
  "Tag files with TAGS by tagging symlinks pointing to them.

Symbolic links are resolved recursively and always point to the
*real* file.  This saves space in the database and make updating
of broken links much simpler.

FILES is a list of files to tag.

TAGS is a list of tags to assign to the files.  Each tripple tag
should be represented by one string.  Non-existing tags will be
created automatically."
  (interactive (list (dired-get-marked-files)
                     (dired-tagsistant--read-tags)))
  (dired-tagsistant--tag files tags :symlink))


;; Relations

;;;###autoload
(defun dired-tagsistant-add-relation (parent rel child)
  (interactive (let* ((tags (dired-tagsistant--get-tags :no-namespace))
                      (parent (completing-read (format "Parent (default \"%s\"): " (car tags))
                                               tags nil
                                               t nil nil (car tags)))
                      (rel (completing-read (format "Relation (default \"%s includes\"):" parent)
                                            (list "includes"
                                                  "excludes"
                                                  "is_equivalent")
                                            nil t nil nil "includes"))
                      (tags-child (-difference tags (list parent)))
                      (child (completing-read (format "Child (default \"%s %s %s\"): " parent rel (car tags-child))
                                              tags-child nil
                                              'confirm nil nil (car tags-child))))
                 (list parent rel child)))
  (let ((path (dired-tagsistant--relations parent rel child)))
    (unless (f-directory? path)
      (make-directory path))))

(provide 'dired-tagsistant)
;;; dired-tagsistant.el ends here
