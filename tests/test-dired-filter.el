;; -*- lexical-binding: t -*-

(require 'f)
(require 'dash)
(require 'shut-up)
(let ((project-dir (f-parent (f-dirname (f-this-file)))))
  (add-to-list 'load-path project-dir))
(require 'dired-filter)

(defun with-temp-fs--init (spec &optional path)
  (setq path (or path ""))
  (cond
   ((listp spec)
    (cond
     ((and (stringp (car spec))
           (stringp (cadr spec)))
      (with-temp-file (f-join path (car spec))
        (insert (cadr spec))))
     ((eq 'dir (car spec))
      (make-directory (f-join path (cadr spec)))
      (with-temp-fs--init (cddr spec) (f-join path (cadr spec))))
     (t (mapcar (lambda (s) (with-temp-fs--init s path)) spec))))
   ((stringp spec)
    (f-touch (f-join path spec)))))

(defmacro with-temp-fs (spec &rest forms)
  (declare (indent 1))
  `(let ((temp-root (make-temp-file "temp-fs-" t)))
     (with-temp-buffer
       (setq default-directory temp-root)
       (mapcar (lambda (s) (with-temp-fs--init s "")) ,spec)
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

(buttercup-define-matcher :to-equal-as-string-set (a b)
  (let ((a-sorted (-sort 'string< a))
        (b-sorted (-sort 'string< b)))
    (if (equal a-sorted b-sorted)
        (cons t (format "Expected %S not to `equal' %S" a-sorted b-sorted))
      (cons nil (format "Expected %S to `equal' %S" a-sorted b-sorted)))))

(describe "Dired dot-files filter"

  (it "should hide dotfiles we don't want to see"
    (with-temp-fs '(".foo" "bar")
      (with-dired '((dot-files))
        (expect (dired-utils-get-all-files :local) :to-equal-as-string-set '("bar"))))))

(describe "Dired omit filter"

  (it "should hide ignored files"
    (with-temp-fs '("bar.o" "bar.a" "bar.h" "bar.c")
      (with-dired '((omit))
        (expect (dired-utils-get-all-files :local) :to-equal-as-string-set '("bar.h" "bar.c"))))))
