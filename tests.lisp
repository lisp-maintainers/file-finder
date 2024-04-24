
(in-package :file-finder)

(uiop:add-package-local-nickname :finder :file-finder)

;; We find at least 1 file.
;; (#F"~/quicklisp/local-projects/fof/file.lisp")
(let ((res (finder (path~ "fil") (extension= "lisp"))))
  (print res)
  (assert (length res)))


(let ((file (first (finder "readme.org"))))
  (assert (not (equal 1970
                      ;; creation-date returns 1970
                      ;; (local-time:timestamp-year (creation-date file))))))
                      (local-time:timestamp-year (modification-date file))))))
