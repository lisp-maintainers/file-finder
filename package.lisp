(uiop:define-package fof/package
  (:nicknames fof)
  (:documentation "File object finder.
This is the meta package which includes all others.")
  (:import-from #:trivial-package-local-nicknames)
  (:import-from #:named-readtables)
  (:use-reexport
   #:fof/file
   #:fof/predicates))
