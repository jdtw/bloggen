;;;; util.lisp

(in-package #:bloggen)

(defun markdown-p (path)
  "tests whether or not the file is a markdown file.
Recognizes the extenstions 'md', 'markdown', and 'text'"
  (let ((ext (getf (file-ext (file-ext path)) :ext)))
    (or (equalp ext "md")
        (equalp ext "markdown")
        (equalp ext "text"))))

(defun file-ext (path)
  "creates a plist that contains the file name and extension"
  (let ((result (nth-value 1 (scan-to-strings
                              "(^[\\w\\.]+)\\.([^\\.]+$)"
                              (file-namestring path)))))
    (list :file (aref result 0) :ext (aref result 1))))
