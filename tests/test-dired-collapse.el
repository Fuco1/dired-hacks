;; -*- lexical-binding: t -*-

(require 'assess)

(require 'dired-collapse)

(describe "Dired collapse"

  (describe "Collapse"

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

  (describe "Overlay"

    (describe "When the leaf directory is empty"

      (it "should extend the shadow overlay over it when it is a root directory"
        (assess-with-filesystem '(("foo/"))
          (unwind-protect
              (shut-up
                (dired default-directory)
                (dired-collapse-mode)
                (goto-char (point-min))
                (expect (search-forward "foo" nil t) :to-be-truthy)
                (backward-char 1)
                (expect (overlays-at (point)) :to-be-truthy))
            (dired-collapse-mode -1))))

      (it "should extend the shadow overlay over it when it is a nested directory"
        (assess-with-filesystem '(("foo/bar/"))
          (unwind-protect
              (shut-up
                (dired default-directory)
                (dired-collapse-mode)
                (goto-char (point-min))
                (expect (search-forward "foo/bar" nil t) :to-be-truthy)
                (backward-char 1)
                (expect (overlays-at (point)) :to-be-truthy))
            (dired-collapse-mode -1)))))

    (describe "When the leaf directory is not empty"

      (it "should not extend the shadow overlay over the single non-directory child of a directory"
        (assess-with-filesystem '(("foo/" ("bar")))
          (unwind-protect
              (shut-up
                (dired default-directory)
                (dired-collapse-mode)
                (goto-char (point-min))
                (expect (search-forward "foo/bar" nil t) :to-be-truthy)
                (backward-char 1)
                (expect (overlays-at (point)) :not :to-be-truthy))
            (dired-collapse-mode -1))))

      (it "should not extend the shadow overlay over the non-empty directory with multiple children"
        (assess-with-filesystem '(("foo/" ("bar" "baz")))
          (unwind-protect
              (shut-up
                (dired default-directory)
                (dired-collapse-mode)
                (goto-char (point-min))
                (expect (search-forward "foo" nil t) :to-be-truthy)
                (backward-char 1)
                (expect (overlays-at (point)) :not :to-be-truthy))
            (dired-collapse-mode -1))))

      (it "should not extend the shadow overlay over the single non-empty directory child of a directory"
        (assess-with-filesystem '(("foo/bar/" ("foo" "bar")))
          (unwind-protect
              (shut-up
                (dired default-directory)
                (dired-collapse-mode)
                (goto-char (point-min))
                (expect (search-forward "foo/bar" nil t) :to-be-truthy)
                (backward-char 1)
                (expect (overlays-at (point)) :not :to-be-truthy))
            (dired-collapse-mode -1)))))))
