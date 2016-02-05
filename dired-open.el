;;; dired-open.el --- Open files from dired using using custom actions

;; Copyright (C) 2014-2015 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
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

;; While emacs already has the `auto-mode-alist', this is often
;; insufficient.  Many times, you want to open media files, pdfs or
;; other documents with an external application.  There's remedy for
;; that too, namely `dired-guess-shell-alist-user', but that is still
;; not as convenient as just hitting enter.

;; This package adds a mechanism to add "hooks" to `dired-find-file'
;; that will run before emacs tries its own mechanisms to open the
;; file, thus enabling you to launch other application or code and
;; suspend the default behaviour.

;; By default, two additional methods are enabled,
;; `dired-open-by-extension' and `dired-open-subdir'.

;; This package also provides other convenient hooks:
;;
;; * `dired-open-xdg' - try to open the file using `xdg-open'
;; * `dired-open-guess-shell-alist' - try to open the file by
;;   launching applications from `dired-guess-shell-alist-user'
;; * `dired-open-call-function-by-extension' - call an elisp function
;;   based on extension.
;;
;; These are not used by default.

;; You can customize the list of functions to try by customizing
;; `dired-open-functions'.

;; To fall back to the default `dired-find-file', you can provide the
;; prefix argument (usually C-u) to the `dired-open-file' function.
;; This is useful for example when you configure html files to be
;; opened in browser and you want to edit the file instead of view it.

;; Note also that this package can handle calls when point is not on a
;; line representing a file---an example hook is provided to open a
;; subdirectory under point if point is on the subdir line, see
;; `dired-open-subdir'.

;; If you write your own handler, make sure they do *not* throw errors
;; but instead return nil if they can't proceed.

;; See https://github.com/Fuco1/dired-hacks for the entire collection.

;;; Code:

(require 'dired-x)
(require 'dired-hacks-utils)
(require 'dash)

(defgroup dired-open ()
  "Open files from dired using using custom actions."
  :group 'dired-hacks
  :prefix "dired-open-")

(defcustom dired-open-functions '(dired-open-by-extension dired-open-subdir)
  "List of functions to try to open a file.

Each function should accept no argument and should retrieve the
filename and/or other context by itself.  Each function should
return non-nil value if it succeeded in opening the file."
  :type 'hook
  :group 'dired-open)

(defcustom dired-open-find-file-function 'dired-find-file
  "A function that will be used if none of the `dired-open-functions' succeeded."
  :type 'function
  :group 'dired-open)


(defcustom dired-open-extensions nil
  "Alist of extensions mapping to a programs to run them in.

The filename is appended after the program."
  :type '(alist
          :key-type (string :tag "Extension")
          :value-type (string :tag "Program"))
  :group 'dired-open)

(defcustom dired-open-extensions-elisp nil
  "Alist of extensions mapping to an elisp function to be called.

The filename is passed as the only argument to the function."
  :type '(alist
          :key-type (string :tag "Extension")
          :value-type (function :tag "Function"))
  :group 'dired-open)

(defcustom dired-open-use-nohup t
  "If non-nil, use nohup(1) to keep external processes opened
even if emacs process is terminated.

This only affects the built-in handlers."
  :type 'boolean
  :group 'dired-open)

(defcustom dired-open-query-before-exit t
  "If non-nil, ask the user if they want to kill any external
processes started by `dired-open-file' when they exit emacs.

This only affects the built-in handlers."
  :type 'boolean
  :group 'dired-open)


(defun dired-open--start-process (file command)
  "Open FILE with COMMAND.

FILE is string, path to the file you want to open.  It is
resolved with `file-truename'.

Note that FILE should not be \"shell escaped\", that is handled
by this function if the shell is invoked.

COMMAND is a string representing the command to run.  If you want
to call it with any switches, these should be included in this
string as well."
  (let ((process
         (apply 'start-process "dired-open" nil
                (if dired-open-use-nohup
                    (list "sh" "-c"
                          (concat
                           "nohup "
                           command
                           " "
                           (shell-quote-argument (file-truename file))
                           " 2>&1 >/dev/null"))
                  (append (split-string command " ")
                          (list (file-truename file)))))))
    (when (and process
               (not dired-open-query-before-exit))
      (set-process-query-on-exit-flag process nil))
    process))


;;; file opening procedures
(defun dired-open-xdg ()
  "Try to run `xdg-open' to open the file under point."
  (interactive)
  (if (executable-find "xdg-open")
      (let ((file (ignore-errors (dired-get-file-for-visit))))
        (start-process "dired-open" nil
                       "xdg-open" (file-truename file)))
    nil))

(defun dired-open-by-extension ()
  "Open a file according to its extension.

The mappings from extensions to applications is specified by
`dired-open-extensions'."
  (interactive)
  (let ((file (ignore-errors (dired-get-file-for-visit)))
        process)
    (when (and file
               (not (file-directory-p file)))
      (--each-while dired-open-extensions (not process)
        (when (string-match-p (concat "\\." (regexp-quote (car it)) "\\'") file)
          (setq process (dired-open--start-process file (cdr it)))))
      process)))

(defun dired-open-guess-shell-alist ()
  "Open the file under point in an application suggested by
`dired-guess-shell-alist-user'."
  (interactive)
  (let ((file (ignore-errors (dired-get-file-for-visit)))
        process)
    (when (and file
               (not (file-directory-p file)))
      (--each-while dired-guess-shell-alist-user (not process)
        (when (string-match-p (car it) file)
          (setq process (dired-open--start-process file (eval (cadr it)))))))
    process))

(defun dired-open-call-function-by-extension ()
  "Call an elisp function on file according to its extension.

The mappings from extensions to applications is specified by
`dired-open-extensions-elisp'."
  (interactive)
  (-when-let (file (dired-utils-get-filename))
    (when (not (file-directory-p file))
      (--when-let (dired-utils-match-filename-extension file dired-open-extensions-elisp)
        (funcall (cdr it) file)
        it))))


;;; non-file opening procedures
(defun dired-open-subdir ()
  "If point is on a subdir line, open the directory under point
in a new buffer.

For example, if the point is on line

  /home/us|er/downloads

the directory /home/user is opened in new buffer."
  (interactive)
  (when (dired-get-subdir)
    (-when-let (end (save-excursion (re-search-forward "[/:]" (line-end-position) t)))
      (let ((path (buffer-substring-no-properties
                   (+ 2 (line-beginning-position))
                   (1- end))))
        (find-file path)))))


;;; main

;;;###autoload
(defun dired-open-file (&optional arg)
  "Try `dired-open-functions' to open the thing under point.

That can be either file or any other line of dired listing.

If no function succeeded, run `dired-find-file' normally.

With \\[universal-argument], run `dired-find-file' normally."
  (interactive "P")
  (when (or arg
            (not (run-hook-with-args-until-success 'dired-open-functions)))
    (funcall dired-open-find-file-function)))

(define-key dired-mode-map [remap dired-find-file] 'dired-open-file)

(provide 'dired-open)

;;; dired-open.el ends here
