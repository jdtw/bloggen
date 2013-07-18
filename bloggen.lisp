;;;; bloggen.lisp

(in-package #:bloggen)

;;; "bloggen" goes here. Hacks and glory await!

(defun markdown-p (path)
  (let ((ext (getf (file-ext (file-ext path)) :ext)))
    (or (equalp ext "md")
        (equalp ext "markdown")
        (equalp ext "text"))))

(defun file-ext (path)
  (let ((result (nth-value 1 (scan-to-strings
                              "(^[\\w\\.]+)\\.([^\\.]+$)"
                              (file-namestring path)))))
    (list :file (aref result 0) :ext (aref result 1))))
