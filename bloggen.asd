;;;; bloggen.asd

(asdf:defsystem #:bloggen
  :serial t
  :description "A simple blog generator in Common Lisp."
  :author "John Woood <j@jdtw.us>"
  :license "MIT"
  :depends-on (#:cl-markdown
               #:cl-fad
               #:html-template
               #:hunchentoot
               #:cl-ppcre)
  :components ((:file "package")
               (:file "util")
               (:file "bloggen")
               (:static-file "README.md")
               (:static-file "LICENSE")))

