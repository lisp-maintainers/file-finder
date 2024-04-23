(uiop:define-package file-finder/package
  (:nicknames file-finder)
  (:documentation "File object finder.
This is the meta package which includes all others.")
  (:import-from #:trivial-package-local-nicknames)
  (:import-from #:named-readtables)
  (:use-reexport
   #:file-finder/file
   #:file-finder/predicates))
