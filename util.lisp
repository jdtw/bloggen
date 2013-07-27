;;;; util.lisp

(in-package #:bloggen)

(defun markdown-p (path)
  "tests whether or not the file is a markdown file.
Recognizes the extenstions 'md', 'markdown', and 'text'"
  (let ((ext (file-ext path)))
    (or (equalp ext "md")
        (equalp ext "markdown")
        (equalp ext "text"))))

(defun file-ext (path)
  "returns the file extension and the file name"
  (if (not (directory-pathname-p path))
   (let ((result (nth-value 1 (scan-to-strings
                               "(^[\\w\\.]+)\\.([^\\.]+$)"
                               (file-namestring path)))))
     (if result
         (values (aref result 1) (aref result 0))
         (values "" (file-namestring path))))
   (error (format nil "~a must not be a directory" path))))
