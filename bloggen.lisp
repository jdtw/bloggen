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
           (author (document-property :author))
           (template (document-property :template)))
      (list :title title :date date :author author
            :html html :template (or template "post.tmpl")))))

(defun compile-blog (root &optional (destination "site/"))
  "Compiles blog. The 'root' directory must have the
following subdirectories:

    root
    |_ posts
    |_ templates

All other subdirectories will be copied directly to the
destination directory, and all markdown files will be
processed."
  (let ((dest (if (pathname-relative-p destination)
                  (merge-pathnames-as-directory root destination)
                  destination)))
    (labels ((test (path)
               (if (directory-pathname-p path)
                   (let ((dirname (car (last (pathname-directory path)))))
                     (not (eq (aref dirname 0) #\_)))
                   (markdown-p path)))
             (fn (path)
               (when (not (directory-pathname-p path))
                 (let ((md (get-markdown path))
                       (name (nth-value 1 (file-ext (pathname-name path)))))
                   (with-open-file (html (merge-pathnames (concatenate 'string name ".html") dest)
                                         :direction :output :if-exists :supersede :external-format :utf-8)
                     (princ (getf md :html) html))))))
      (ensure-directories-exist dest)
      (walk-directory root #'fn :directories :breadth-first :test #'test))))
