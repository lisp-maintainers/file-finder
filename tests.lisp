
(in-package :file-finder)

(uiop:add-package-local-nickname :finder :file-finder)

;; We find at least 1 file.
;; (#F"~/quicklisp/local-projects/fof/file.lisp")
(let ((res (finder:finder (path~ "fil") (extension= "lisp"))))
  (print res)
  (assert (length res)))
