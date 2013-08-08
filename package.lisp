;;;; package.lisp

(defpackage #:bloggen
  (:use #:cl)
  (:shadowing-import-from #:hunchentoot
                          #:start
                          #:stop
                          #:acceptor)
  (:shadowing-import-from #:markdown
                          #:markdown
                          #:document-property
                          #:*current-document*)
  (:shadowing-import-from #:cl-fad
                          #:pathname-relative-p
                          #:directory-pathname-p
                          #:merge-pathnames-as-file
                          #:merge-pathnames-as-directory
                          #:file-exists-p
                          #:walk-directory)
  (:shadowing-import-from #:html-template
                          #:create-template-printer
                          #:fill-and-print-template
                          #:*string-modifier*))

