;;;; bloggen.asd

(asdf:defsystem #:bloggen
  :serial t
  :description "A simple blog generator in Common Lisp."
  :author "John Woood <j@jdtw.us>"
  :license "MIT"
  :depends-on (#:cl-markdown
               #:html-template)
  :components ((:file "package")
               (:file "bloggen")))

