;;; dired-list.el --- Create dired listings from sources

;; Copyright (C) 2014-2015 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Version: 0.0.1
;; Created: 14th February 2014
;; Package-Requires: ((dash "2.10.0"))
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

;; Produce a file listing with a shell incantation and make a dired
;; out of it!

;; This package provides one principal function, `dired-list' which
;; can be used to produce dired buffers from shell programs outputing
;; text roughly in the format of `la -ls'.

;; For most standard output formats the default filter and sentinel
;; should work, but you can also provide your own if the situation
;; requires it.

;; Most of the time you can pipe a zero-delimited list of files to ls
;; through xargs(1) using

;;   | xargs -I '{}' -0 ls -l '{}'

;; which creates a compatible listing.  For more information read the
;; documentation of `dired-list', for example by invoking

;;   C-h f dired-list RET

;; in emacs.

;; In addition to the generic interface this package implements common
;; listings (patches and extensions welcome!), these are:
;; * `dired-list-mpc'
;; * `dired-list-git-ls-files'
;; * `dired-list-hg-locate'
;; * `dired-list-locate'
;; * `dired-list-find-file'
;; * `dired-list-find-name'
;; * `dired-list-grep'

;; See https://github.com/Fuco1/dired-hacks for the entire collection.

;;; Code:
(require 'dash)
(require 'dired-hacks-utils)

(require 'grep)
(require 'find-dired)

(defun dired-list-align-size-column ()
  "Align the filesize column."
  (beginning-of-line)
  (save-match-data
    (when (and (looking-at "^  [^0-9]")
               (re-search-forward dired-move-to-filename-regexp nil t))
      (goto-char (match-beginning 7))
      (backward-char 1)
      (let* ((size-end (point))
             (size-beg (search-backward " " nil t))
             (width (and size-end (- size-end size-beg))))
        (when (and size-end (< 1 width) (< width 12))
          (goto-char size-beg)
          (insert (make-string (- 12 width) ? )))))))

(defun dired-list-default-filter (proc string)
  "Filter the output of the process to make it suitable for `dired-mode'.

This filter assumes that the input is in the format of `ls -l'."
  (let ((buf (process-buffer proc))
        (inhibit-read-only t))
    (if (buffer-name buf)
        (with-current-buffer buf
          (save-excursion
            (save-restriction
              (widen)
              (let ((beg (point-max)))
                (goto-char beg)
                (insert string)
                (goto-char beg)
                (or (looking-at "^")
                    (progn
                      (dired-list-align-size-column)
                      (forward-line 1)))
                (while (looking-at "^")
                  (insert "  ")
                  (dired-list-align-size-column)
                  (forward-line 1))
                (goto-char (- beg 3))
                (while (search-forward " ./" nil t)
                  (delete-region (point) (- (point) 2)))
                (goto-char beg)
                (beginning-of-line)
                ;; Remove occurrences of default-directory.
                (while (search-forward (concat " " default-directory) nil t)
                  (replace-match " " nil t))
                ;; remove '\ ' and replace with just a space
                (goto-char beg)
                (beginning-of-line)
                (while (search-forward "\\ " nil t)
                  (replace-match " " nil t))
                (goto-char (point-max))
                (when (search-backward "\n" (process-mark proc) t)
                  (dired-insert-set-properties (process-mark proc) (1+ (point)))
                  (move-marker (process-mark proc) (1+ (point))))))))
      (delete-process proc))))

(defun dired-list-default-sentinel (proc state)
  "Update the status/modeline after the process finishes."
  (let ((buf (process-buffer proc))
        (inhibit-read-only t))
    (if (buffer-name buf)
        (with-current-buffer buf
          (let ((buffer-read-only nil))
            (save-excursion
              (goto-char (point-max))
              (insert "\n  " state)
              (forward-char -1)     ;Back up before \n at end of STATE.
              (insert " at " (substring (current-time-string) 0 19))
              (forward-char 1)
              (setq mode-line-process (concat ":" (symbol-name (process-status proc))))
              (delete-process proc)
              (force-mode-line-update)))
          (run-hooks 'dired-after-readin-hook)
          (message "%s finished." (current-buffer))))))

(defun dired-list-kill-process ()
  "Kill the process running in the current buffer."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (and proc
         (eq (process-status proc) 'run)
         (condition-case nil
             (delete-process proc)
           (error nil)))))

(defun dired-list (dir buffer-name cmd &optional revert-function filter sentinel)
  "Present output of a command as a `dired-buffer'.

DIR is the default directory of the resulting `dired' buffer.

BUFFER-NAME is name of the created buffer.  If such buffer
exists, it is erased first.

CMD is a sh(1) invocation to produce output for dired to process.
It should be in the format similar to `ls -l'.

Optional argument REVERT-FUNCTION is used to revert (bound to
\\[revert-buffer]) the buffer.

Optional argument FILTER is a function used to post-process the
process's output after it was inserted to dired buffer.

Optional argument SENTINEL is a function called on each change of
state of the buffer's process."
  (let* ((dired-buffers nil) ;; do not mess with regular dired buffers
         (dir (file-name-as-directory (expand-file-name dir)))
         (filter (or filter 'dired-list-default-filter))
         (sentinel (or sentinel 'dired-list-default-sentinel)))
    (run-hooks 'dired-list-before-buffer-creation-hook)
    ;; TODO: abstract buffer creation
    (with-current-buffer (get-buffer-create buffer-name)
      (switch-to-buffer (current-buffer))
      (widen)
      ;; here we might want to remember some state from before, so add
      ;; a hook to do that
      (kill-all-local-variables)
      (read-only-mode -1) ;only support 24+
      (let ((inhibit-read-only t)) (erase-buffer))
      (setq default-directory dir)
      (run-hooks 'dired-before-readin-hook)
      (shell-command cmd (current-buffer))
      (insert "  " dir ":\n")
      (insert "  " cmd "\n")
      (dired-mode dir)
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map (current-local-map))
        (define-key map "\C-c\C-k" 'dired-list-kill-process)
        (use-local-map map))
      (set (make-local-variable 'dired-sort-inhibit) t)
      (set (make-local-variable 'revert-buffer-function) revert-function)
      (set (make-local-variable 'dired-subdir-alist)
           (list (cons default-directory (point-min-marker))))
      (let ((proc (get-buffer-process (current-buffer))))
        (set-process-filter proc filter)
        (set-process-sentinel proc sentinel)
        (move-marker (process-mark proc) 1 (current-buffer)))
      (setq mode-line-process '(":%s")))))

(defcustom dired-list-mpc-music-directory "~/Music"
  "MPD Music directory."
  :type 'directory
  :group 'dired-list)

;;;###autoload
(defun dired-list-mpc (query)
  "Search mpd(1) database using QUERY and display results as a `dired' buffer."
  (interactive "sMPC search query: ")
  (let ((dired-list-before-buffer-creation-hook
         '((lambda () (cd dired-list-mpc-music-directory)))))
    (dired-list dired-list-mpc-music-directory
                (concat "mpc " query)
                (concat "mpc search "
                        query
                        " | tr '\\n' '\\000' | xargs -I '{}' -0 ls -l '{}' &")
                `(lambda (ignore-auto noconfirm)
                   (dired-list-mpc ,query)))))

;;;###autoload
(defun dired-list-git-ls-files (dir)
  "List all files in DIR managed by git and display results as a `dired' buffer."
  (interactive "DDirectory: ")
  (dired-list dir
              (concat "git ls-files " dir)
              (concat "git ls-files -z | xargs -I '{}' -0 ls -l '{}' &")
              `(lambda (ignore-auto noconfirm) (dired-list-git-ls-files ,dir))))

;;;###autoload
(defun dired-list-hg-locate (dir)
  "List all files in DIR managed by mercurial and display results as a `dired' buffer."
  (interactive "DDirectory: ")
  (dired-list dir
              (concat "hg locate " dir)
              (concat "hg locate -0 | xargs -I '{}' -0 ls -l '{}' &")
              `(lambda (ignore-auto noconfirm) (dired-list-hg-locate ,dir))))

;;;###autoload
(defun dired-list-locate (needle)
  "Locate(1) all files matching NEEDLE and display results as a `dired' buffer."
  (interactive "sLocate: ")
  (dired-list "/"
              (concat "locate " needle)
              (concat "locate " (shell-quote-argument needle) " -0 | xargs -I '{}' -0 ls -ld '{}' &")
              `(lambda (ignore-auto noconfirm) (dired-list-locate ,needle))))


;; taken from grep.el/rgrep
(defun dired-list--get-ignored-stuff ()
  "Return an argument to find which ignores uninteresting directories and files.

Directories are taken form `grep-find-ignored-directories', files
are taken from `grep-find-ignored-files'."
  (concat
   (and grep-find-ignored-directories
        (concat "-type d "
                (shell-quote-argument "(")
                ;; we should use shell-quote-argument here
                " -path "
                (mapconcat
                 #'(lambda (ignore)
                     (cond ((stringp ignore)
                            (shell-quote-argument
                             (concat "*/" ignore)))
                           ((consp ignore)
                            (and (funcall (car ignore) dir)
                                 (shell-quote-argument
                                  (concat "*/"
                                          (cdr ignore)))))))
                 grep-find-ignored-directories
                 " -o -path ")
                " "
                (shell-quote-argument ")")
                " -prune -o "))
   (and grep-find-ignored-files
        (concat (shell-quote-argument "!") " -type d "
                (shell-quote-argument "(")
                ;; we should use shell-quote-argument here
                " -name "
                (mapconcat
                 #'(lambda (ignore)
                     (cond ((stringp ignore)
                            (shell-quote-argument ignore))
                           ((consp ignore)
                            (and (funcall (car ignore) dir)
                                 (shell-quote-argument
                                  (cdr ignore))))))
                 grep-find-ignored-files
                 " -o -name ")
                " "
                (shell-quote-argument ")")
                " -prune -o "))))

;;;###autoload
(defun dired-list-find-file (dir cmd)
  "Run find(1) on DIR.

By default, directories matching `grep-find-ignored-directories'
and files matching `grep-find-ignored-files' are ignored.

If called with raw prefix argument \\[universal-argument], no
files will be ignored."
  (interactive (let ((base-cmd (concat "find . "
                                  (if current-prefix-arg "" (dired-list--get-ignored-stuff))
                                  " -ls &")))
                 (list (read-directory-name "Directory: " nil nil t)
                       (read-from-minibuffer
                        "Find command: "
                        (cons base-cmd (string-match-p "-ls &" base-cmd))))))
  (let ((short-cmd (save-match-data
                     (if (string-match ".* -prune -o \\(.*?\\) -ls &" cmd)
                         (match-string 1 cmd)
                       cmd))))
    (dired-list dir
                (concat "find " dir ": " short-cmd)
                cmd
                `(lambda (ignore-auto noconfirm) (dired-list-find-file ,dir ,cmd)))))

;;;###autoload
(defun dired-list-find-name (dir pattern)
  "Search DIR recursively for files matching the globbing pattern PATTERN,
and run dired on those files.

PATTERN is a shell wildcard (not an Emacs regexp) and need not be quoted.

By default, directories matching `grep-find-ignored-directories'
and files matching `grep-find-ignored-files' are ignored.

If called with raw prefix argument \\[universal-argument], no
files will be ignored."
  (interactive "DDirectory: \nsPattern: ")
  (dired-list dir
              (concat "find " dir ": " pattern)
              (concat "find . " (if current-prefix-arg "" (dired-list--get-ignored-stuff)) " -name " (shell-quote-argument pattern) " -ls &")
              `(lambda (ignore-auto noconfirm) (dired-list-find-name ,dir ,pattern))))

(defun dired-list-grep (dir regexp)
  "Recursively find files in DIR containing regexp REGEXP and start Dired on output."
  (interactive "DDirectory: \nsRegexp: \n")
  (dired-list dir
              (concat "find grep " dir ": " regexp)
              (concat "find . " (dired-list--get-ignored-stuff)
                      " \\( -type f -exec " grep-program " " find-grep-options
                      " -e " (shell-quote-argument regexp) " {} \\; \\) -ls &")
              `(lambda (ignore-auto noconfirm) (dired-list-find-grep ,dir ,regexp))))

(provide 'dired-list)
;;; dired-list.el ends here
