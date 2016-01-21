;; -*- lexical-binding: t -*-

(require 'f)
(require 'dash)
(require 'shut-up)
(let ((project-dir (f-parent (f-dirname (f-this-file)))))
  (add-to-list 'load-path project-dir))
(require 'dired-filter)

(defun with-temp-fs--init (spec &optional path)
  (setq path (or path "."))
  (cond
   ((listp spec)
    (cond
     ;; non-empty file
     ((and (stringp (car spec))
           (stringp (cadr spec)))
      (with-temp-file (concat path "/" (car spec))
        (insert (cadr spec))))
     ;; directory
     ((and (stringp (car spec))
           (consp (cadr spec)))
      (make-directory (concat path "/" (car spec)) t)
      (mapc (lambda (s) (with-temp-fs--init s (concat path "/" (car spec)))) (cadr spec)))
     ;; recursive spec, this should probably never happen
     (t (mapc (lambda (s) (with-temp-fs--init s path)) spec))))
   ;; directory specified using a string
   ((and (stringp spec)
         (string-match-p "/\\'" spec))
    (make-directory (concat path "/" spec) t))
   ;; empty file
   ((stringp spec)
    (f-touch (f-join path spec)))
   (t (error "Invalid syntax: %s" spec))))

(defmacro with-temp-fs (spec &rest forms)
  (declare (indent 1))
  `(let ((temp-root (make-temp-file "temp-fs-" t)))
     (with-temp-buffer
       (setq default-directory temp-root)
       (mapc (lambda (s) (with-temp-fs--init s ".")) ,spec)
       (unwind-protect
           (progn
             ,@forms)
         (delete-directory temp-root t)))))

(put 'dir 'lisp-indent-function '1)

(defmacro with-dired (filter-stack &rest body)
  (declare (indent 1))
  `(shut-up
     (dired default-directory)
     (setq dired-filter-stack ,filter-stack)
     (dired-filter-mode 1)
     ,@body))

(defmacro with-dired-groups (filter-groups &rest body)
  (declare (indent 1))
  `(shut-up
     (dired default-directory)
     (setq dired-filter-group-saved-groups ,filter-groups)
     (dired-filter-group-mode 1)
     ,@body))

(buttercup-define-matcher :to-equal-as-string-set (a b)
  (let ((a-sorted (-sort 'string< a))
        (b-sorted (-sort 'string< b)))
    (if (equal a-sorted b-sorted)
        (cons t (format "Expected %S not to `equal' %S" a-sorted b-sorted))
      (cons nil (format "Expected %S to `equal' %S" a-sorted b-sorted)))))

(describe "Dired Filter"

  (describe "Dired dot-files filter"

    (it "should hide dotfiles we don't want to see"
      (with-temp-fs '(".foo" "bar")
        (with-dired '((dot-files))
          (expect (dired-utils-get-all-files :local) :to-equal-as-string-set '("bar"))))))

  (describe "Dired name filter"

    (it "should keep lines matching the name"
      (with-temp-fs '("foo.o" "bar.a" "bar.h" "quux.c")
        (with-dired '((name . "bar"))
          (expect (dired-utils-get-all-files :local) :to-equal-as-string-set '("bar.a" "bar.h"))))))

  (describe "Dired regexp filter"

    (it "should keep files matching the name as regexp"
      (with-temp-fs '("foo.o" "bar.a" "bar.h" "quux.c")
        (with-dired '((name . "b.r\\."))
          (expect (dired-utils-get-all-files :local) :to-equal-as-string-set '("bar.a" "bar.h")))))

    (it "should keep files or directories matching the name as regexp"
      (with-temp-fs '("foo.o" "bar.a" "bar.h" "quux.c" "bur.d/")
        (with-dired '((name . "b.r\\."))
          (expect (dired-utils-get-all-files :local) :to-equal-as-string-set '("bar.a" "bar.h" "bur.d")))))

    (it "should be able to match extensions"
      (with-temp-fs '("foo.o" "bar.a" "bar.h" "quux.c")
        (with-dired '((name . "\\.a$"))
          (expect (dired-utils-get-all-files :local) :to-equal-as-string-set '("bar.a"))))))

  (describe "Dired omit filter"

    (it "should hide ignored files"
      (with-temp-fs '("bar.o" "bar.a" "bar.h" "bar.c")
        (with-dired '((omit))
          (expect (dired-utils-get-all-files :local) :to-equal-as-string-set '("bar.h" "bar.c"))))))

  (describe "Dired and meta-filter"

    (describe "Combining positive filters"

      (it "should keep lines matching all the filters"
        (with-temp-fs '("foo" "bar" "bax/" "qux/")
          (with-dired '((file) (name . "bar"))
            (expect (dired-utils-get-all-files :local) :to-equal-as-string-set '("bar")))))

      (it "should commute"
        (with-temp-fs '("foo" "bar" "bax/" "qux/")
          (let ((this (with-dired '((name . "bax") (directory))
                        (dired-utils-get-all-files :local)))
                (other (with-dired '((directory) (name . "bax"))
                         (dired-utils-get-all-files :local))))
            (expect this :to-equal-as-string-set other))))

      (it "should work with more than two filters"
        (with-temp-fs '("foo" "bar" "bax/" "qux/" "bar.c" "bar.h" "barfux.c" "barbara/")
          (with-dired '((name . "bar") (file) (extension . "c"))
            (expect (dired-utils-get-all-files :local) :to-equal-as-string-set '("bar.c" "barfux.c"))))))

    (describe "Combining positive and negative filters"

      (it "should keep lines matching positive filters after removing those matched by negative filters"
        (with-temp-fs '(".bar" ".foo" "foo" "bar")
          (with-dired '((dot-files) (name . "bar"))
            (expect (dired-utils-get-all-files :local) :to-equal-as-string-set '("bar")))))

      (it "should commute"
        (with-temp-fs '("bar.o" "bar.a" "bar.h" "bar.c" "foo.h" "foo.c" "foo.o")
          (let ((this (with-dired '((name . "bar") (omit))
                        (dired-utils-get-all-files :local)))
                (other (with-dired '((omit) (name . "bar"))
                         (dired-utils-get-all-files :local))))
            (expect this :to-equal-as-string-set other))))

      (it "should work as two positive filters if we negate the negative one"
        (with-temp-fs '(".bar" ".foo" "foo" "bar")
          (with-dired '((not (dot-files)) (name . "bar"))
            (expect (dired-utils-get-all-files :local) :to-equal-as-string-set '(".bar"))))))

    (describe "Combining negative filters"

      (it "should remove lines matching any of the filters (deMorgan's law)"
        (with-temp-fs '(".bar" ".foo" "foo.o" "bar")
          (with-dired '((omit) (dot-files))
            (expect (dired-utils-get-all-files :local) :to-equal-as-string-set '("bar")))))))

  (describe "Dired or meta-filter"

    (describe "Combining positive filters"

      (it "should keep lines matching either of the filters"
        (with-temp-fs '("bar.o" "bar.a" "bar.h" "bar.c" "foo.h" "foo.c" "foo.o")
          (with-dired '((or (extension . "o") (name . "bar")))
            (expect (dired-utils-get-all-files :local) :to-equal-as-string-set '("bar.a" "bar.o" "foo.o" "bar.h" "bar.c"))))))

    (describe "Combining negative and positive filters"

      (it "should create exceptions for negative filters"
        (with-temp-fs '(".bar" ".foo" "foo" "bar")
          ;; throw out all dotfiles except those having bar in the name
          (with-dired '((or (dot-files) (name . "bar")))
            (expect (dired-utils-get-all-files :local) :to-equal-as-string-set '("bar" "foo" ".bar")))))

      (it "should commute"
        (with-temp-fs '("bar.a" "foo.o" "foo" "bar")
          ;; throw out all dotfiles except those having bar in the name
          (let ((this (with-dired '((or (name . "bar") (omit)))
                        (dired-utils-get-all-files :local)))
                (other (with-dired '((or (omit) (name . "bar")))
                         (dired-utils-get-all-files :local))))
            (expect this :to-equal-as-string-set other)))))


    (describe "Combining negative filters"

      (it "should remove lines matching both filters (deMorgan's law)"
        (with-temp-fs '(".bar.o" ".foo.txt" "foo" "bar")
          ;; throw out all dotfiles with "omit" extension
          (with-dired '((or (dot-files) (omit)))
            (expect (dired-utils-get-all-files :local) :to-equal-as-string-set '("bar" "foo" ".foo.txt"))))))))

(describe "Dired Filter Groups"

  (it "should group lines according to filters"
    (with-temp-fs '("foo/" "bar/" "baz.tex" "baz.bib" "normal-file.txt")
      (with-dired-groups '(("default"
                            ("Directories" (directory))
                            ("LaTeX" (extension "tex" "bib"))))
        (let ((groups (dired-filter-group-get-groups)))
          (expect (gethash "Directories" groups) :to-equal-as-string-set '("." ".." "foo" "bar"))
          (expect (gethash "LaTeX" groups) :to-equal-as-string-set '("baz.tex" "baz.bib"))))))

  (it "should not create empty drawers when groups overlap [#57]"
    (with-temp-fs '("a.py" "b.py" "regular" "another")
      (with-dired-groups '(("default"
                            ("Python" (extension "py"))
                            ("B" (name . "b"))))
        (let ((groups (dired-filter-group-get-groups)))
          (expect (gethash "B" groups) :to-equal-as-string-set '("b.py"))
          (expect (gethash "Python" groups) :to-equal-as-string-set '("a.py"))))

      (with-dired-groups '(("default"
                            ("B" (name . "b"))
                            ("Python" (extension "py"))))
        (let ((groups (dired-filter-group-get-groups)))
          (expect (gethash "B" groups) :to-equal-as-string-set nil)
          (expect (gethash "Python" groups) :to-equal-as-string-set '("a.py" "b.py")))))))
