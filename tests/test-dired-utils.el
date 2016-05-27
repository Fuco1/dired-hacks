;; -*- lexical-binding: t -*-

(require 'dash)
(require 'dired-hacks-utils)

(describe "Dired utils match filename regexp"

  (it "should match a pattern in the middle of the filename"
    (expect (dired-utils-match-filename-regexp
             "afooa"
             (list (cons "bar" 1)
                   (cons "foo" 2)
                   (cons "fo" 3)))
            :to-equal (cons "foo" 2)))

  (it "should match a filename against regexp pattern"
    (expect (dired-utils-match-filename-regexp
             "foo"
             (list (cons "bar" 1)
                   (cons "f.o" 2)
                   (cons "fod" 3)))
            :to-equal (cons "f.o" 2))

    (expect (dired-utils-match-filename-regexp
             "foooo/baz"
             (list (cons "bar" 1)
                   (cons "fo+" 2)
                   (cons "fod" 3)))
            :to-equal (cons "fo+" 2))

    (expect (dired-utils-match-filename-regexp
             "foooo/bar"
             (list (cons "bar" 1)
                   (cons "fo+" 2)
                   (cons "fod" 3)))
            :to-equal (cons "bar" 1))))

(describe "Dired utils match filename extension"

  (it "should match the file's extension against the pattern"
    (expect (dired-utils-match-filename-extension
             "foo.txt"
             (list (cons "c" 1)
                   (cons "txt" 2)
                   (cons "h" 3)))
            :to-equal (cons "txt" 2)))

  (it "should match not match the extension as a regexp"
    (expect (dired-utils-match-filename-extension
             "foo.*a"
             (list (cons "x" 1)
                   (cons "*a" 2)
                   (cons "y" 3)))
            :to-equal (cons "*a" 2))))
