;;;; bloggen.lisp

(in-package #:bloggen)

;;;;;;
;; Directories and directory helpers.
;;;;;;

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

;;;;;;
;; Have hunchentoot serve the site
;; for testing purposes
;;;;;;

(let ((acceptor nil))
  (defun serve (&optional (port 8080))
    (when acceptor (stop-serving))
    (setf acceptor (make-instance 'acceptor :port port :document-root root))
    (start acceptor))
  (defun stop-serving ()
    (stop acceptor)
    (setf acceptor nil)))

;;;;;;
;; Markdown
;;;;;;

(defun get-markdown (path)
  (multiple-value-bind (doc html)
      (markdown path :stream nil)
    (let ((*current-document* doc))
      (values
       (document-property :title)
       (document-property :date)
       (document-property :author)
       (or (document-property :template)
           "post.tmpl")
       html))))

(defun md->html (path)
  (when (and (not (directory-pathname-p path)) (markdown-p path))
    (multiple-value-bind (title date author template html)
        (get-markdown path)
      (let ((html-file (get-destination (nth-value 1 (file-ext (pathname-name path)))))
            (*string-modifier* #'identity))
        (with-open-file (out html-file
                             :direction :output
                             :if-exists :supersede
                             :element-type 'character
                             :external-format :utf-8)
          (fill-and-print-template
           (get-template template)
           (list :body (remove-if (lambda (c) (eq #\Return c)) html)
                 :title title
                 :date date
                 :author author)
           :stream out)
          (format t "wrote ~a~%" html-file))))))

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


