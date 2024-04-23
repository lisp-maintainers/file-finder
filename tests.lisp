
(uiop:add-package-local-nickname :finder :file-finder)

;; We find at least 1 file.
;; (#F"~/quicklisp/local-projects/fof/file.lisp")
(let ((res (finder:finder (file-finder/p:path~ "fil") (file-finder/p:extension= "lisp"))))
  (print res)
  (assert (length res)))
