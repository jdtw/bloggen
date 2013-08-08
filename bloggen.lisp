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

(defun get-destination-html (name &optional (relative-path #p""))
  (ensure-directories-exist
   (merge-pathnames-as-file *site-root*
                            *destination-directory*
                            relative-path
                            (format nil "~a.html" name))))

(defun root->relative (path)
  "Given a path that start with *site-root*, returns
a relative path from site root"
  (let ((root-list (pathname-directory *site-root*))
        (path-list (pathname-directory path)))
    (assert (>= (length path-list) (length root-list)))
    (loop
       with relative = path-list
       for i in root-list
       for j in path-list
       do (if (equalp i j)
              (pop relative)
              (error "'path' must contain *site-root*"))
       finally (return (if relative
                           (make-pathname :directory (cons :relative relative))
                           nil)))))

;;;;;;
;; Have hunchentoot serve the site
;; for testing purposes
;;;;;;

(let ((acceptor nil))
  (defun serve (root &optional (port 8080))
    (when acceptor (stop-serving))
    (setf acceptor (make-instance 'acceptor
                                  :port port
                                  :document-root root))
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
      (list 
       :title (document-property :title)
       :date (document-property :date)
       :date (document-property :author)
       :template (document-property :template)
       :body html))))

;; TODO: use actual templates. This manual creation is temporary.
(let ((indexes (make-hash-table :test #'equal)))
  (defun add-item (dir md)
    (push (getf md :title) (gethash dir indexes)))
  (defun generate-indexes ()
    "Each subdirectory must have its own index template. E.g., if
there is a directory 'posts/', there must be a template posts.index.tmpl"
    (loop
       for dir being the hash-keys in indexes
       using (hash-value items) do
         (let ((list (format nil "<ul>~{<li>~a</li>~}</ul>" (reverse items))))
           (fill-template (get-destination-html "index" dir)
                          (list :template (format nil
                                                  "~a.index.tmpl"
                                                  (first (last (pathname-directory dir))))
                                :body list))))
    ;; reset the hash table.
    (setf indexes (make-hash-table :test #'equal))))

(defun fill-template (file plist)
  "'plist' must contain :template and :body properties. All of the
properties are passed to the template specified by :template."
  (let ((*string-modifier* #'identity)
        (html-template:*warn-on-creation* nil)
        (template (create-template-printer (get-template (getf plist :template))
                                           :element-type 'character
                                           :external-format :utf-8
                                           :force t ;; for debugging
                                           )))
    (with-open-file (out
                     file
                     :direction :output
                     :if-exists :supersede
                     :element-type 'character
                     :external-format :utf-8)
      (setf (getf plist :body)
            (remove-if (lambda (c) (eq #\Return c)) (getf plist :body)))       
      (fill-and-print-template template plist :stream out)
      (format t "wrote ~a~%" file))))

(defun md->html (path)
  ;; if it's a file, and that file is a markdown file...
  (when (and (not (directory-pathname-p path)) (markdown-p path))
    (let* ((md (get-markdown path))
           (relative-dir (root->relative path))
           (file (get-destination-html (pathname-name path)
                                       (or relative-dir #p""))))
      (fill-template file md)
      (when relative-dir
        (add-item relative-dir md)))))

(defun compile-blog (root &key (destination *destination-directory*) serve)
  (labels ((test (path)
             (if (directory-pathname-p path)
                 (let ((dirname (car (last (pathname-directory path)))))
                   (not (eq (aref dirname 0) #\_)))
                 (markdown-p path))))
    (let ((*site-root* root)
          (*destination-directory* destination))
      (walk-directory root
                      #'md->html
                      :directories :breadth-first
                      :test #'test)
      (generate-indexes)
      (when serve
        (serve (merge-pathnames-as-directory *site-root*
                                             *destination-directory*))))))
