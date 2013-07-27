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
                          #:merge-pathnames-as-directory
                          #:directory-pathname-p
                          #:walk-directory))

