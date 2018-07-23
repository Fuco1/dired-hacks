;; -*- lexical-binding: t -*-

(require 'assess)
(require 'dash)
(require 'shut-up)
(require 'dired-filter)

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

(buttercup-define-matcher :to-be-file (file)
  (if (file-regular-p file)
      (cons t (format "Expected %S not to be a file" file))
    (cons nil (format "Expected %S to be a file" file))))

(buttercup-define-matcher :to-be-directory (dir)
  (if (file-directory-p dir)
      (cons t (format "Expected %S not to be a directory" dir))
    (cons nil (format "Expected %S to be a directory" dir))))

(buttercup-define-matcher :to-contain (file content)
  (if (with-temp-buffer
        (insert-file-contents file)
        (equal (buffer-string) content))
      (cons t (format "Expected the content of %S not to `equal' %S" file content))
    (cons nil (format "Expected the content of %S to `equal' %S" file content))))

(describe "Dired Filter"

  (describe "Dired dot-files filter"

    (it "should hide dotfiles we don't want to see"
      (assess-with-filesystem '(".foo" "bar")
        (with-dired '((dot-files))
          (expect (dired-utils-get-all-files :local) :to-have-same-items-as '("bar"))))))

  (describe "Dired name filter"

    (it "should keep lines matching the name"
      (assess-with-filesystem '("foo.o" "bar.a" "bar.h" "quux.c")
        (with-dired '((name . "bar"))
          (expect (dired-utils-get-all-files :local) :to-have-same-items-as '("bar.a" "bar.h"))))))

  (describe "Dired regexp filter"

    (it "should keep files matching the name as regexp"
      (assess-with-filesystem '("foo.o" "bar.a" "bar.h" "quux.c")
        (with-dired '((name . "b.r\\."))
          (expect (dired-utils-get-all-files :local) :to-have-same-items-as '("bar.a" "bar.h")))))

    (it "should use case-sensitive match if qualifier contains upper-case letters"
      (assess-with-filesystem '("foo.o" "FOO.o")
        (with-dired '((name . "F"))
          (expect (dired-utils-get-all-files :local) :to-have-same-items-as '("FOO.o")))))

    (it "should keep files or directories matching the name as regexp"
      (assess-with-filesystem '("foo.o" "bar.a" "bar.h" "quux.c" "bur.d/")
        (with-dired '((name . "b.r\\."))
          (expect (dired-utils-get-all-files :local) :to-have-same-items-as '("bar.a" "bar.h" "bur.d")))))

    (it "should be able to match extensions"
      (assess-with-filesystem '("foo.o" "bar.a" "bar.h" "quux.c")
        (with-dired '((name . "\\.a$"))
          (expect (dired-utils-get-all-files :local) :to-have-same-items-as '("bar.a"))))))

  (describe "Dired omit filter"

    (it "should hide ignored files"
      (assess-with-filesystem '("bar.o" "bar.a" "bar.h" "bar.c")
        (with-dired '((omit))
          (expect (dired-utils-get-all-files :local) :to-have-same-items-as '("bar.h" "bar.c"))))))

  (describe "Dired and meta-filter"

    (describe "Combining positive filters"

      (it "should keep lines matching all the filters"
        (assess-with-filesystem '("foo" "bar" "bax/" "qux/")
          (with-dired '((file) (name . "bar"))
            (expect (dired-utils-get-all-files :local) :to-have-same-items-as '("bar")))))

      (it "should commute"
        (assess-with-filesystem '("foo" "bar" "bax/" "qux/")
          (let ((this (with-dired '((name . "bax") (directory))
                        (dired-utils-get-all-files :local)))
                (other (with-dired '((directory) (name . "bax"))
                         (dired-utils-get-all-files :local))))
            (expect this :to-have-same-items-as other))))

      (it "should work with more than two filters"
        (assess-with-filesystem '("foo" "bar" "bax/" "qux/" "bar.c" "bar.h" "barfux.c" "barbara/")
          (with-dired '((name . "bar") (file) (extension . "c"))
            (expect (dired-utils-get-all-files :local) :to-have-same-items-as '("bar.c" "barfux.c"))))))

    (describe "Combining positive and negative filters"

      (it "should keep lines matching positive filters after removing those matched by negative filters"
        (assess-with-filesystem '(".bar" ".foo" "foo" "bar")
          (with-dired '((dot-files) (name . "bar"))
            (expect (dired-utils-get-all-files :local) :to-have-same-items-as '("bar")))))

      (it "should commute"
        (assess-with-filesystem '("bar.o" "bar.a" "bar.h" "bar.c" "foo.h" "foo.c" "foo.o")
          (let ((this (with-dired '((name . "bar") (omit))
                        (dired-utils-get-all-files :local)))
                (other (with-dired '((omit) (name . "bar"))
                         (dired-utils-get-all-files :local))))
            (expect this :to-have-same-items-as other))))

      (it "should work as two positive filters if we negate the negative one"
        (assess-with-filesystem '(".bar" ".foo" "foo" "bar")
          (with-dired '((not (dot-files)) (name . "bar"))
            (expect (dired-utils-get-all-files :local) :to-have-same-items-as '(".bar"))))))

    (describe "Combining negative filters"

      (it "should remove lines matching any of the filters (deMorgan's law)"
        (assess-with-filesystem '(".bar" ".foo" "foo.o" "bar")
          (with-dired '((omit) (dot-files))
            (expect (dired-utils-get-all-files :local) :to-have-same-items-as '("bar")))))))

  (describe "Dired or meta-filter"

    (describe "Combining positive filters"

      (it "should keep lines matching either of the filters"
        (assess-with-filesystem '("bar.o" "bar.a" "bar.h" "bar.c" "foo.h" "foo.c" "foo.o")
          (with-dired '((or (extension . "o") (name . "bar")))
            (expect (dired-utils-get-all-files :local) :to-have-same-items-as '("bar.a" "bar.o" "foo.o" "bar.h" "bar.c"))))))

    (describe "Combining negative and positive filters"

      (it "should create exceptions for negative filters"
        (assess-with-filesystem '(".bar" ".foo" "foo" "bar")
          ;; throw out all dotfiles except those having bar in the name
          (with-dired '((or (dot-files) (name . "bar")))
            (expect (dired-utils-get-all-files :local) :to-have-same-items-as '("bar" "foo" ".bar")))))

      (it "should commute"
        (assess-with-filesystem '("bar.a" "foo.o" "foo" "bar")
          ;; throw out all dotfiles except those having bar in the name
          (let ((this (with-dired '((or (name . "bar") (omit)))
                        (dired-utils-get-all-files :local)))
                (other (with-dired '((or (omit) (name . "bar")))
                         (dired-utils-get-all-files :local))))
            (expect this :to-have-same-items-as other)))))


    (describe "Combining negative filters"

      (it "should remove lines matching both filters (deMorgan's law)"
        (assess-with-filesystem '(".bar.o" ".foo.txt" "foo" "bar")
          ;; throw out all dotfiles with "omit" extension
          (with-dired '((or (dot-files) (omit)))
            (expect (dired-utils-get-all-files :local) :to-have-same-items-as '("bar" "foo" ".foo.txt"))))))))

(describe "Dired Filter Groups"

  (it "should group lines according to filters"
    (assess-with-filesystem '("foo/" "bar/" "baz.tex" "baz.bib" "normal-file.txt")
      (with-dired-groups '(("default"
                            ("Directories" (directory))
                            ("LaTeX" (extension "tex" "bib"))))
        (let ((groups (dired-filter-group-get-groups)))
          (expect (gethash "Directories" groups) :to-have-same-items-as '("." ".." "foo" "bar"))
          (expect (gethash "LaTeX" groups) :to-have-same-items-as '("baz.tex" "baz.bib"))))))

  (it "should not create empty drawers when groups overlap [#57]"
    (assess-with-filesystem '("a.py" "b.py" "regular" "another")
      (with-dired-groups '(("default"
                            ("Python" (extension "py"))
                            ("B" (name . "b"))))
        (let ((groups (dired-filter-group-get-groups)))
          (expect (gethash "B" groups) :to-have-same-items-as '("b.py"))
          (expect (gethash "Python" groups) :to-have-same-items-as '("a.py"))))

      (with-dired-groups '(("default"
                            ("B" (name . "b"))
                            ("Python" (extension "py"))))
        (let ((groups (dired-filter-group-get-groups)))
          (expect (gethash "B" groups) :to-have-same-items-as nil)
          (expect (gethash "Python" groups) :to-have-same-items-as '("a.py" "b.py")))))))
