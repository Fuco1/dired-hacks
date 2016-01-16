;; -*- lexical-binding: t -*-

(require 'f)
(require 'shut-up)
(let ((project-dir (f-parent (f-dirname (f-this-file)))))
  (add-to-list 'load-path project-dir))
(require 'dired-filter)

(describe "Dired dot-files filter"

  (it "should hide dotfiles we don't want to see"
    (let ((test-dir (make-temp-file "df-" t)))
      (f-touch (f-join test-dir ".foo"))
      (f-touch (f-join test-dir "bar"))

      (unwind-protect
          (shut-up
            (dired test-dir)
            (setq dired-filter-stack '((dot-files)))
            (dired-filter-mode 1)
            (expect (length (dired-utils-get-all-files)) :to-equal 1))
        (delete-directory test-dir t)))))


(describe "Dired omit filter"

  (it "should hide ignored files"
    (let ((test-dir (make-temp-file "df-" t)))
      (f-touch (f-join test-dir "bar.o"))
      (f-touch (f-join test-dir "bar.a"))
      (f-touch (f-join test-dir "bar.h"))
      (f-touch (f-join test-dir "bar.c"))

      (unwind-protect
          (shut-up
            (dired test-dir)
            (setq dired-filter-stack '((omit)))
            (dired-filter-mode 1)
            (expect (length (dired-utils-get-all-files)) :to-equal 2))
        (delete-directory test-dir t)))))
