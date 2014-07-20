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

(defun dired-tagsistant--namespace-p (tag)
  "Return non-nil if TAG is a namespace tag."
  (s-ends-with? ":" tag))

(defun dired-tagsistant--get-tags (&optional no-namespaces)
  "Return a list of all available tags.

If NO-NAMESPACES is non-nil, do not return namespace tags."
  (let ((tagdir (concat (dired-tagsistant-root) "tags/")))
    (--map (s-chop-prefix tagdir it)
           (let ((tags (f-directories tagdir)))
             (if no-namespaces
                 (-remove 'dired-tagsistant--namespace-p tags)
               tags)))))

(defun dired-tagsistant--get-namespace-keys (namespace)
  "Return a list of all keys in NAMESPACE."
  (let ((tagdir (concat (dired-tagsistant-root) "tags/" namespace "/")))
    (--map (s-chop-prefix tagdir it) (f-directories tagdir))))

(defun dired-tagsistant--get-namespace-key-values (namespace key)
  (let ((tagdir (concat (dired-tagsistant-root) "tags/" namespace "/" key "/")))
    (--map (s-chop-prefix tagdir it) (f-directories tagdir))))

(defun dired-tagsistant--store (query)
  "Return the store directory represented by QUERY."
  (concat (dired-tagsistant-root) "store/" query))


;; Readers

(defvar dired-tagsistant--read-history nil
  "History of tags read from the user.")

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
                 (format "Key [namespace %s]: " namespace)
                 namespaces nil t nil nil (car namespaces))))
         (op (completing-read "Operator: " '("eq" "inc" "gt" "lt") nil t nil nil "eq"))
         (value (let ((values (dired-tagsistant--get-namespace-key-values namespace key)))
                  (completing-read
                   (format "Value [namespace %s]: " namespace)
                   values
                   nil nil nil nil (car values)))))
    (list key op value)))


;; Basic queries

(defun dired-tagsistant-some-tags (tags)
  "Display all files matching some tag in TAGS."
  (interactive (list (dired-tagsistant--read-tags)))
  (let ((query (concat (s-join "/+/" tags) "/@")))
    (find-file (dired-tagsistant--store query))))

(defun dired-tagsistant-all-tags (tags)
  "Display all files matching all tags in TAGS."
  (interactive (list (dired-tagsistant--read-tags)))
  (let ((query (concat (s-join "/" tags) "/@")))
    (find-file (dired-tagsistant--store query))))

(defun dired-tagsistant-some-tags-regexp (regexp)
  "Display all files where some of their tags matches REGEXP."
  (interactive "sRegexp: ")
  (let* ((tags (--filter (string-match-p regexp it) (dired-tagsistant--get-tags :no-namespaces))))
    (dired-tagsistant-some-tags tags)))

(defun dired-tagsistant-all-tags-regexp (regexp)
  "Display all files where all of their tags match REGEXP."
  (interactive "sRegexp: ")
  (let* ((tags (--filter (string-match-p regexp it) (dired-tagsistant--get-tags :no-namespaces))))
    (dired-tagsistant-all-tags tags)))

(provide 'dired-tagsistant)
;;; dired-tagsistant.el ends here
