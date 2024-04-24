
(uiop:define-package file-finder
  (:documentation "File object finder, one package for the two project files (file class, finder predicates).")
  (:use #:common-lisp)
  (:import-from #:serapeum
                #:export-always
                #:->)
  (:local-nicknames (#:alex #:alexandria)
                    (#:sera #:serapeum))
  (:export
   ;; File class and readers:
   ;; (use M-x slime-export-class)
   #:file
   #:path
   #:inode
   #:link-count
   #:kind
   #:size
   #:disk-usage
   #:creation-date
   ;; second class:
   #:file+mime
   #:mime-type
   #:mime-encoding
   #:description))

;; predicates.lisp symbols are exported with serapeum:export-always in the file.
