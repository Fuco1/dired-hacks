(require 's)
(require 'dash)

(defvar dired-columns-list '(name-only ext size date))
(defvar dired-columns-name-columns '(name name-only))
(defvar dired-columns-name-column-width 40)


;;; extractors
(defvar dired-columns-permissions-regexp "  \\([dl-][rwx-]\\{9\\}\\)"
  "Regexp matching permissions column.")

(defvar dired-columns-user-regexp "  [dl-][rwx-]\\{9\\} \\(.*?\\) "
  "Regexp matching user column.")

(defvar dired-columns-group-regexp "  [dl-][rwx-]\\{9\\} \\(?:.*?\\)[ ]+\\(.*?\\) "
  "Regexp matching group column.")

(defvar dired-columns-size-regexp "  [dl-][rwx-]\\{9\\} \\(?:.*?\\)[ ]+\\(?:.*?\\)[ ]+\\(.*?\\) "
  "Regexp matching size column.")

(defvar dired-columns-date-regexp"  [dl-][rwx-]\\{9\\} \\(?:.*?\\)[ ]+\\(?:.*?\\)[ ]+\\(?:.*?\\) \\(............\\) "
  "Regexp matching date column.")

(defun dired-columns--regexp-extractor (regexp)
  (save-excursion
    (when (re-search-forward regexp nil t)
      (match-string 1))))

(defun dired-columns--name-extractor ()
  (dired-get-filename))

(defun dired-columns--name-only-extractor ()
  (-when-let (name (dired-get-filename 'no-dir))
    (if (file-directory-p name)
        name
      (if (string-match "\\`\\([^.].*\\)\\.\\(.*\\)\\'" name)
          (match-string 1 name)
        name))))

(defun dired-columns--ext-extractor ()
  (save-excursion
    (-when-let (name (dired-get-filename 'no-dir))
      (if (file-directory-p name)
          nil
        (let* ((bofn (progn
                       (dired-move-to-filename)
                       (point)))
               (eofn (dired-move-to-end-of-filename))
               (bext (--if-let (search-backward "." bofn t)
                         ;; if there's no extension, we shouldn't hide the filename
                         (if (= it bofn) eofn it)
                       eofn)))
          (when (/= eofn bext)
            (buffer-substring (1+ bext) eofn)))))))

(defun dired-columns--size-extractor ()
  (dired-columns--regexp-extractor dired-columns-size-regexp))

(defun dired-columns--date-extractor ()
  (dired-columns--regexp-extractor dired-columns-date-regexp))

(defun dired-columns--collect ()
  "Collect all the columns on current line.

The columns are picked according to `dired-columns-list'."
  (--map
   (let ((extractor (intern (format "dired-columns--%s-extractor" it))))
     (funcall extractor))
   dired-columns-list))


;;; formatters

(defun dired-columns--name-formatter (value)
  (s-pad-right 60 " " value))

(defun dired-columns--name-only-formatter (value)
  (s-pad-right 60 " " value))

(defun dired-columns--ext-formatter (value)
  (s-pad-right 5 " " value))

(defun dired-columns--size-formatter (value)
  (s-pad-left 8 " " value))

(defun dired-columns--date-formatter (value)
  (concat "  " value))

(defun dired-columns--replace-lines ()
  (let* ((parts (--split-with (not (memq it dired-columns-name-columns)) dired-columns-list))
         (name-only (memq 'name-only dired-columns-list))
         (front (car parts))
         (back (cdadr parts))
         (front-func (--map (cons (intern (format "dired-columns--%s-extractor" it))
                                  (intern (format "dired-columns--%s-formatter" it))) front))
         (back-func (--map (cons (intern (format "dired-columns--%s-extractor" it))
                                 (intern (format "dired-columns--%s-formatter" it))) back)))
    (let ((inhibit-read-only t))
      (while (= (forward-line) 0)
        (ignore-errors
          (let* ((line (dired-columns--replace-line front-func back-func))
                 (is-dir (file-directory-p (dired-get-filename)))
                 (bofn (save-excursion (dired-move-to-filename) (point)))
                 (eofn (save-excursion (dired-move-to-filename) (dired-move-to-end-of-filename) (point)))
                 (bext (if is-dir eofn
                         (save-excursion
                           (dired-move-to-filename)
                           (dired-move-to-end-of-filename)
                           (--if-let (search-backward "." bofn t)
                               ;; if there's no extension, we shouldn't hide the filename
                               (if (= it bofn) eofn it)
                             eofn))))
                 (fn-length (- bext bofn))
                 (after-start (if (and name-only
                                       (/= bext eofn)
                                       (not is-dir))
                                  bext
                                (save-excursion
                                  (dired-move-to-filename)
                                  (dired-move-to-end-of-filename)
                                  (insert " ")
                                  (point))))
                 (after-end eofn))
            (put-text-property
             (+ 2 (line-beginning-position))
             bofn
             'display
             (car line))
            (put-text-property
             (min after-start after-end)
             (max after-start after-end)
             'display
             (concat (make-string (max (- dired-columns-name-column-width fn-length)) ? ) (cdr line)))))))))

(defun dired-columns--replace-line (front-func back-func)
  (let ((columns-front (--map (funcall (cdr it) (funcall (car it))) front-func))
        (columns-back (--map (funcall (cdr it) (funcall (car it))) back-func)))
    (cons (apply 'concat columns-front)
          (apply 'concat columns-back))))
