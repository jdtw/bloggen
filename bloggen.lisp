;;;; bloggen.lisp

(in-package #:bloggen)

;; start hunchentoot server for testing
(let ((acceptor nil))
  (defun serve (root &key (port 8080))
    (when acceptor
      (stop-serving))
    (setf acceptor (make-instance 'acceptor :port port :document-root root))
    (start acceptor))
  (defun stop-serving ()
    (stop acceptor)))

(defun get-markdown (path)
  (multiple-value-bind (doc html)
      (markdown path :stream nil)
    (let* ((*current-document* doc)
           (title (document-property :title))
           (date (document-property :date))
           (author (document-property :author)))
      (list :title title :date date :author author :html html))))
