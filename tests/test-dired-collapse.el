;; -*- lexical-binding: t -*-

(require 'assess)

(require 'dired-collapse)

(describe "Dired collapse"

  (it "should collapse a nonempty directory when it only has a single child directory"
    (assess-with-filesystem '(("foo/bar/" ("baz" "baaz")))
      (unwind-protect
          (shut-up
            (dired default-directory)
            (dired-collapse-mode)
            (goto-char (point-min))
            (expect (search-forward "foo/bar" nil t) :to-be-truthy))
        (dired-collapse-mode -1))))

  (it "should collapse a nonempty directory when it only has a single child file"
    (assess-with-filesystem '(("foo/bar/" ("baz")))
      (unwind-protect
          (shut-up
            (dired default-directory)
            (dired-collapse-mode)
            (goto-char (point-min))
            (expect (search-forward "foo/bar/baz" nil t) :to-be-truthy))
        (dired-collapse-mode -1)))))
