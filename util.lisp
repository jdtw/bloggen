;;;; util.lisp

(in-package #:bloggen)

(defun markdown-p (path)
  "tests whether or not the file is a markdown file.
Recognizes the extenstions 'md', 'markdown', and 'text'"
  (let ((ext (pathname-type path)))
    (or (equalp ext "md")
        (equalp ext "markdown")
        (equalp ext "text"))))
