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

(defparameter *post-directory* "posts/"
  "Blog posts")

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

(defun post-p (path)
  (and (not (directory-pathname-p path))
       (string= (car (last (pathname-directory path)))
                (string-trim "/" *post-directory*))))

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
       :template (or (document-property :template) "post.tmpl")
       :body html))))

;; TODO: use actual templates. This manual creation is temporary.
(let ((posts nil))
  (defun add-post (md)
    (push (getf md :title) posts))
  (defun generate-index ()
    (let ((list (format nil "<ul>~{<li>~a</li>~}</ul>" (reverse posts))))
      (setf posts nil)
      (fill-template (get-destination "index")
                     (list :template "index.tmpl"
                           :body list)))))

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
  (when (and (not (directory-pathname-p path)) (markdown-p path))
    (let ((md (get-markdown path))
          (file (get-destination (nth-value 1 (file-ext (pathname-name path))))))
      (fill-template file md)
      (when (post-p path)
        (add-post md)))))

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
      (generate-index)
      (when serve
        (serve (merge-pathnames-as-directory *site-root*
                                             *destination-directory*))))))
