;;; dired-tagsistant.el --- Tagsistant support for dired

;; Copyright (C) 2014 Matúš Goljer <matus.goljer@gmail.com>

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Version: 0.0.1
;; Created: 14th February 2014
;; Package-requires: ((dash "2.8.0") (dired-hacks-utils "0.0.1"))
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

(provide 'dired-tagsistant)
;;; dired-tagsistant.el ends here
