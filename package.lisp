;;;; package.lisp

(defpackage #:bloggen
  (:use #:cl)
  (:shadowing-import-from #:hunchentoot
                          #:start
                          #:stop
                          #:acceptor)
  (:shadowing-import-from #:ppcre
                          #:scan-to-strings)
  (:shadowing-import-from #:markdown
                          #:markdown
                          #:document-property
                          #:*current-document*)
  (:shadowing-import-from #:cl-fad
                          #:pathname-relative-p
                          #:directory-pathname-p
                          #:walk-directory)
  (:shadowing-import-from #:html-template
                          #:fill-and-print-template
                          #:*string-modifier*))

