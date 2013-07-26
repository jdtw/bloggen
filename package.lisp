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
                          #:*current-document*))

