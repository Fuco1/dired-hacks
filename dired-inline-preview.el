;;; dired-inline-preview.el --- Inline previews in dired buffer
;; Copyright (C) 2019 Mikael Svahnberg
;; Copyright (C) 2014-- Matúš Goljer

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
;; This is heavily based on dired-subtree by Matúš Goljer,
;; but generalises it to more different types of previews.

;;; Code:
(require 'dired-subtree)
(require 'pdf-info)

(defgroup dired-inline-preview ()
  "Inline previews in dired buffer"
  :group 'dired-hacks
  :prefix "dired-inline-preview-")

(defcustom dired-inline-preview-pdf-width 200
  "width of inlined PDF preview"
  :type 'integer
  :group 'dired-inline-preview)

(defcustom dired-inline-preview-image-height 200
  "height of inlined image preview"
  :type 'integer
  :group 'dired-inline-preview)

;; TODO: use image-type-file-name-regexps instead
(defcustom dired-inline-preview-image-extensions '("PBM" "XBM" "XPM" "GIF" "JPEG" "JPG" "TIFF" "TIF" "PNG" "SVG" "PS")
  "list of filename extensions that should be inlined as images"
  :type 'sexp
  :group 'dired-inline-preview)

(defcustom dired-inline-preview-text-lines 10
  "Number of lines to preview for text files"
  :type 'integer
  :group 'dired-inline-preview)
(defcustom dired-inline-preview-text-extensions '("TXT" "ORG" "TEX" "CSV")
  "list of filename extensions that should be inlined as text"
  :type 'sexp
  :group 'dired-inline-preview)  

(defcustom dired-inline-preview-previewers '(dired-inline-preview--pdf dired-inline-preview--image dired-inline-preview--text)
  "list of previewers to test"
  :type 'sexp
  :group 'dired-inline-preview)

(defconst dired-inline-preview--dummy-file-details " ----------   1 X X   0 Jan 01 00:01 "
  "Dummy file attributes to fool dired-hide-details into not swallowing the preview")

;;; Helpers
(defun dired-inline-preview--match-filename-extension (filename allowed-extensions)
  "return t if the FILENAME extension is in the list of ALLOWED-EXTENSIONS, otherwise return nil."
  (let* ((ext (upcase (or (file-name-extension filename) "")))
         (search-fun (apply-partially (lambda (file-extension allowed-extension) (string= file-extension (upcase (or allowed-extension "")))) ext)))
    (seq-find search-fun allowed-extensions nil)))

(defun dired-inline-preview--maybe-insert-text (text)
  (unless (string-match "^[[:blank:]]*$"
                      (buffer-substring (line-beginning-position)
                                        (line-end-position)))
    (insert text)))

(defun dired-inline-preview--fool-dired-hide-details (listing)
  "Fool dired-hide-details by inserting a dummy file attributes list"
  (with-temp-buffer
    (insert listing)
    (goto-char (point-min))
    (dired-inline-preview--maybe-insert-text dired-inline-preview--dummy-file-details)
    (while (= (forward-line) 0)
      (dired-inline-preview--maybe-insert-text dired-inline-preview--dummy-file-details))
    (buffer-string)))


;;; Previewers:
(defun dired-inline-preview--pdf (filename)
  "return first page of FILENAME or nil"
  (when (dired-inline-preview--match-filename-extension filename '("PDF"))
    (with-temp-buffer
      (insert ".")
      (insert-image (create-image (pdf-info-renderpage 1 dired-inline-preview-pdf-width filename) 'imagemagick t))
      (insert "\n")
      (buffer-string))))

(defun dired-inline-preview--image (filename)
  "return inlined image or nil"
  (when (dired-inline-preview--match-filename-extension filename dired-inline-preview-image-extensions)
    (with-temp-buffer
      (insert ".")
      (insert-image (create-image filename 'imagemagick nil :height dired-inline-preview-image-height))
      (insert "\n")
      (buffer-string))))

(defun dired-inline-preview--text (filename)
  "return inlined text or nil"
  (when (dired-inline-preview--match-filename-extension filename dired-inline-preview-text-extensions)
    (with-temp-buffer
      (insert-file-contents filename)
      (goto-char (point-min))
      (let (out)
        (dotimes (iter dired-inline-preview-text-lines out)
          (forward-line)))
      (delete-region (point) (point-max))
      (buffer-string))))


;;; Entry points:
(defun dired-inline-preview (filename)
  (interactive "P")
  (seq-some (lambda (fun) (funcall fun filename)) dired-inline-preview-previewers))

(defun dired-inline-preview-insert-preview-or-subtree (orig-fun)
  "Call the right insert function for a preview or a subtree"
  (interactive)
  (if (dired-subtree--dired-line-is-directory-or-link-p)
      (apply orig-fun nil)
    (unless (dired-subtree--is-expanded-p)
      (let* ((filename (dired-get-filename nil))
             (listing (dired-inline-preview filename)))
        (if listing (dired-subtree-insert--insert filename (dired-inline-preview--fool-dired-hide-details listing)))))))

(advice-add 'dired-subtree-insert :around #'dired-inline-preview-insert-preview-or-subtree)



(provide 'dired-inline-preview)
;;; dired-inline-preview.el ends here
