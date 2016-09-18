;;; dired-narrow.el --- Live-narrowing of search results for dired

;; Copyright (C) 2014-2015 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Version: 0.0.1
;; Created: 14th February 2014
;; Package-requires: ((dash "2.7.0") (dired-hacks-utils "0.0.1"))
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

;; This package provides live filtering of files in dired buffers.  In
;; general, after calling the respective narrowing function you type a
;; filter string into the minibuffer.  After each change the changes
;; automatically reflect in the buffer.  Typing C-g will cancel the
;; narrowing and restore the original view, typing RET will exit the
;; live filtering mode and leave the dired buffer in the narrowed
;; state.  To bring it back to the original view, you can call
;; `revert-buffer' (usually bound to `g').

;; During the filtering process, several special functions are
;; available.  You can customize the binding by changing
;; `dired-narrow-map'.

;; * `dired-narrow-next-file' (<down>) - move the point to the next file
;; * `dired-narrow-previous-file' (<up>) - move the point to the
;;   previous file
;; * `dired-narrow-enter-directory' (<right>) - descend into the
;;   directory under point and immediately go back to narrowing mode

;; You can customize what happens after exiting the live filtering
;; mode by customizing `dired-narrow-exit-action'.

;; These narrowing functions are provided:

;; * `dired-narrow'
;; * `dired-narrow-regexp'
;; * `dired-narrow-fuzzy'

;; You can also create your own narrowing functions quite easily.  To
;; define new narrowing function, use `dired-narrow--internal' and
;; pass it an apropriate filter.  The filter should take one argument
;; which is the filter string from the minibuffer.  It is then called
;; at each line that describes a file with point at the beginning of
;; the file name.  If the filter returns nil, the file is removed from
;; the view.  As an inspiration, look at the built-in functions
;; mentioned above.

;; See https://github.com/Fuco1/dired-hacks for the entire collection.

;;; Code:

(require 'dash)
(require 'dired-hacks-utils)

(require 'delsel)

(defgroup dired-narrow ()
  "Live-narrowing of search results for dired."
  :group 'dired-hacks
  :prefix "dired-narrow-")

(defvar dired-narrow-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<up>") 'dired-narrow-previous-file)
    (define-key map (kbd "<down>") 'dired-narrow-next-file)
    (define-key map (kbd "<right>") 'dired-narrow-enter-directory)
    (define-key map (kbd "C-g") 'minibuffer-keyboard-quit)
    (define-key map (kbd "RET") 'exit-minibuffer)
    (define-key map (kbd "<return>") 'exit-minibuffer)
    map)
  "Keymap used while `dired-narrow' is reading the pattern.")

(defcustom dired-narrow-exit-action 'ignore
  "Function to call after exiting minibuffer.

Function takes no argument and is called with point over the file
we should act on."
  :type '(choice (const :tag "Open file under point" dired-narrow-find-file)
                 (function :tag "Use custom function."))
  :group 'dired-narrow)

(defcustom dired-narrow-exit-when-one-left nil
  "If there is only one file left while narrowing,
exit minibuffer and call `dired-narrow-exit-action'."
  :type 'boolean
  :group 'dired-narrow)

(defcustom dired-narrow-enable-blinking t
  "If set to true highlight the chosen file shortly.
This feature works only when `dired-narrow-exit-when-one-left' is true."
  :type 'boolean
  :group 'dired-narrow)

(defcustom dired-narrow-blink-time 0.2
  "How long should be highlighted a chosen file.
Units are seconds."
  :type 'float
  :group 'dired-narrow)

(defface dired-narrow-blink
  '((t :background "#eadc62"
       :foreground "black"))
  "The face used to highlight a chosen file 
when `dired-narrow-exit-when-one-left' and `dired-narrow-enable-blinking' are true."
  :group 'dired-narrow)


;; Utils

;; this is `gnus-remove-text-with-property'
(defun dired-narrow--remove-text-with-property (prop)
  "Delete all text in the current buffer with text property PROP."
  (let ((start (point-min))
        end)
    (unless (get-text-property start prop)
      (setq start (next-single-property-change start prop)))
    (while start
      (setq end (text-property-any start (point-max) prop nil))
      (delete-region start (or end (point-max)))
      (setq start (when end
                    (next-single-property-change start prop))))))

(defvar dired-narrow-filter-function 'identity
  "Filter function used to filter the dired view.")

(defvar dired-narrow--current-file nil
  "Value of point just before exiting minibuffer.")

(defun dired-narrow--update (filter)
  "Make the files not matching the FILTER invisible.
 Return the count of visible files that are left after update."

  (let ((inhibit-read-only t)
        (visible-files-cnt 0))
    (save-excursion
      (goto-char (point-min))
      ;; TODO: we might want to call this only if the filter gets less
      ;; specialized.
      (dired-narrow--restore)
      (while (dired-hacks-next-file)
        (if (funcall dired-narrow-filter-function filter)
            (progn
              (setq visible-files-cnt (1+ visible-files-cnt))
              (when (fboundp 'dired-insert-set-properties)
                (dired-insert-set-properties (line-beginning-position) (1+ (line-end-position)))))
          (put-text-property (line-beginning-position) (1+ (line-end-position)) :dired-narrow t)
          (put-text-property (line-beginning-position) (1+ (line-end-position)) 'invisible :dired-narrow))))
    (unless (dired-hacks-next-file)
      (dired-hacks-previous-file))
    (unless (dired-utils-get-filename)
      (dired-hacks-previous-file))
    visible-files-cnt))

(defun dired-narrow--restore ()
  "Restore the invisible files of the current buffer."
  (let ((inhibit-read-only t))
    (remove-text-properties (point-min) (point-max) '(invisible))
    (when (fboundp 'dired-insert-set-properties)
      (dired-insert-set-properties (point-min) (point-max)))))


(defun dired-narrow--blink-current-file ()
  (let* ((beg (line-beginning-position))
         (end (line-end-position))
         (overlay (make-overlay beg end)))
    (overlay-put overlay 'face 'dired-narrow-blink)
    (redisplay)
    (sleep-for dired-narrow-blink-time)
    (discard-input)
    (delete-overlay overlay)))


;; Live filtering

(defvar dired-narrow-buffer nil
  "Dired buffer we are currently filtering.")

(defvar dired-narrow--minibuffer-content ""
  "Content of the minibuffer during narrowing.")

(defun dired-narrow--minibuffer-setup ()
  "Set up the minibuffer for live filtering."
  (when dired-narrow-buffer
    (add-hook 'post-command-hook 'dired-narrow--live-update nil :local)))

(add-hook 'minibuffer-setup-hook 'dired-narrow--minibuffer-setup)

(defun dired-narrow--live-update ()
  "Update the dired buffer based on the contents of the minibuffer."
  (when dired-narrow-buffer
    (let ((current-filter (minibuffer-contents-no-properties))
          visible-files-cnt)
      (with-current-buffer dired-narrow-buffer
        (setq visible-files-cnt
              (unless (equal current-filter dired-narrow--minibuffer-content)
                (dired-narrow--update current-filter)))

        (setq dired-narrow--minibuffer-content current-filter)
        (setq dired-narrow--current-file (dired-utils-get-filename))
        (set-window-point (get-buffer-window (current-buffer)) (point))

        (when (and dired-narrow-exit-when-one-left
                   visible-files-cnt
                   (= visible-files-cnt 1))
          (when dired-narrow-enable-blinking
              (dired-narrow--blink-current-file))
          (exit-minibuffer))))))

(defun dired-narrow--internal (filter-function)
  "Narrow a dired buffer to the files matching a filter.

The function FILTER-FUNCTION is called on each line: if it
returns non-nil, the line is kept, otherwise it is removed.  The
function takes one argument, which is the current filter string
read from minibuffer."
  (let ((dired-narrow-buffer (current-buffer))
        (dired-narrow-filter-function filter-function)
        (disable-narrow nil))
    (unwind-protect
        (progn
          (dired-narrow-mode 1)
          (add-to-invisibility-spec :dired-narrow)
          (setq disable-narrow (read-from-minibuffer "Filter: " nil dired-narrow-map))
          (let ((inhibit-read-only t))
            (dired-narrow--remove-text-with-property :dired-narrow))
          ;; If the file no longer exists, we can't do anything, so
          ;; set to nil
          (unless (dired-utils-goto-line dired-narrow--current-file)
            (setq dired-narrow--current-file nil)))
      (with-current-buffer dired-narrow-buffer
        (unless disable-narrow (dired-narrow-mode -1))
        (remove-from-invisibility-spec :dired-narrow)
        (dired-narrow--restore))
      (when (and disable-narrow
                 dired-narrow--current-file
                 dired-narrow-exit-action)
        (funcall dired-narrow-exit-action))
      (cond
       ((equal disable-narrow "dired-narrow-enter-directory")
        (dired-narrow--internal filter-function))))))


;; Interactive

(defun dired-narrow--regexp-filter (filter)
  (re-search-forward filter (line-end-position) t))

;;;###autoload
(defun dired-narrow-regexp ()
  "Narrow a dired buffer to the files matching a regular expression."
  (interactive)
  (dired-narrow--internal 'dired-narrow--regexp-filter))

(defun dired-narrow--string-filter (filter)
  (let ((words (split-string filter " ")))
    (--all? (save-excursion (search-forward it (line-end-position) t)) words)))

(defun dired-narrow-next-file ()
  "Move point to the next file."
  (interactive)
  (with-current-buffer dired-narrow-buffer
    (dired-hacks-next-file)))

(defun dired-narrow-previous-file ()
  "Move point to the previous file."
  (interactive)
  (with-current-buffer dired-narrow-buffer
    (dired-hacks-previous-file)))

(defun dired-narrow-find-file ()
  "Run `dired-find-file' or any remapped action on file under point."
  (interactive)
  (let ((function (or (command-remapping 'dired-find-file)
                      'dired-find-file)))
    (funcall function)))

(defun dired-narrow-enter-directory ()
  "Descend into directory under point and initiate narrowing."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "dired-narrow-enter-directory"))
  (exit-minibuffer))

;;;###autoload
(defun dired-narrow ()
  "Narrow a dired buffer to the files matching a string.

If the string contains spaces, then each word is matched against
the file name separately.  To succeed, all of them have to match
but the order does not matter.

For example \"foo bar\" matches filename \"bar-and-foo.el\"."
  (interactive)
  (dired-narrow--internal 'dired-narrow--string-filter))

(defun dired-narrow--fuzzy-filter (filter)
  (re-search-forward
   (mapconcat 'regexp-quote
              (mapcar 'char-to-string (string-to-list filter))
              ".*")
   (line-end-position) t))

;;;###autoload
(defun dired-narrow-fuzzy ()
  "Narrow a dired buffer to the files matching a fuzzy string.

A fuzzy string is constructed from the filter string by inserting
\".*\" between each letter.  This is then matched as regular
expression against the file name."
  (interactive)
  (dired-narrow--internal 'dired-narrow--fuzzy-filter))

(define-minor-mode dired-narrow-mode
  "Minor mode for indicating when narrowing is in progress."
  :lighter " dired-narrow")

(defun dired-narrow--disable-on-revert ()
  "Disable `dired-narrow-mode' after revert."
  (dired-narrow-mode -1))

(add-hook 'dired-after-readin-hook 'dired-narrow--disable-on-revert)

(provide 'dired-narrow)
;;; dired-narrow.el ends here
