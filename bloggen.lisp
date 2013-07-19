;;;; bloggen.lisp

(in-package #:bloggen)

;;; "bloggen" goes here. Hacks and glory await!
(let ((acceptor nil))
  (defun serve (root &key (port 8080))
    (when acceptor
      (stop-serving))
    (setf acceptor (make-instance 'acceptor :port port :document-root root))
    (start acceptor))
  (defun stop-serving ()
    (stop acceptor)))
