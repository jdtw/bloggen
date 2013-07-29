;;;; bloggen.lisp

(in-package #:bloggen)

;; start hunchentoot server for testing
(let ((acceptor nil))
  (defun serve (root &key (port 8080))
    (when acceptor (stop-serving))
    (setf acceptor (make-instance 'acceptor :port port :document-root root))
    (start acceptor))
  (defun stop-serving ()
    (stop acceptor)
    (setf acceptor nil)))

(defun get-markdown (path)
  (multiple-value-bind (doc html)
      (markdown path :stream nil)
    (let* ((*current-document* doc)
           (title (document-property :title))
           (date (document-property :date))
           (author (document-property :author))
           (template (document-property :template)))
      (list :title title :date date :author author
            :html html :template (or template "post.tmpl")))))

(defparameter *site-root* nil)

(defparameter *template-directory* "templates/"
  "The default template dir relative to *site-root*")

(defparameter *destination-directory* "site/"
  "Where the processed files should be placed. Can
either be relative to *site-root* or absolute")

(defun get-template (tmpl)
  (or (file-exists-p
       (merge-pathnames-as-file *site-root*
                                *template-directory*
                                tmpl))
      (error (format nil "~a doesn't exist." tmpl))))

(defun get-destination (name)
  (ensure-directories-exist
   (merge-pathnames-as-file *site-root*
                            *destination-directory*
                            (format nil "~a.html" name))))

(defun md->html (path)
  (when (and (not (directory-pathname-p path)) (markdown-p path))
    (let ((md (get-markdown path))
          (html-file (get-destination (nth-value 1 (file-ext (pathname-name path)))))
          (*string-modifier* #'identity))
      (with-open-file (out html-file
                           :direction :output
                           :if-exists :supersede
                           :element-type 'character
                           :external-format :utf-8)
        (fill-and-print-template
         (get-template (getf md :template))
         (list :body (remove-if (lambda (c) (eq #\Return c))
                                (getf md :html)))
         :stream out)
        (format t "wrote ~a~%" html-file)))))

(defun compile-blog (root &optional (destination *destination-directory*))
  (labels ((test (path)
             (if (directory-pathname-p path)
                 (let ((dirname (car (last (pathname-directory path)))))
                   (not (eq (aref dirname 0) #\_)))
                 (markdown-p path))))
    (let ((*site-root* root)
          (*destination-directory* destination))
      (walk-directory root #'md->html
                      :directories :breadth-first
                      :test #'test)))  )


