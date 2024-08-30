
(in-package file-finder)

(export-always 'date<)
(defun date< (timestamp)
  "Return a file predicate that matches on modification time less recent than TIMESTAMP."
  (lambda (file)
    (local-time:timestamp< (modification-date file) timestamp)))

(export-always 'date>)
(defun date> (timestamp)
  "Return a file predicate that matches on modification time more recent than TIMESTAMP."
  (lambda (file)
    (local-time:timestamp> (modification-date file) timestamp)))

(export-always 'extension=)
(defun extension= (extension &rest more-extensions)
  "Return a predicate for files that match one of the provided extensions."
  (lambda (file)
    (some (sera:equals (extension file))
          (cons extension more-extensions))))

;; (export-always 'kind=)
;; (-> kind= (file-kind &rest file-kind) function)
;; (defun kind= (kind &rest more-kinds)
;;   "Return a predicate for files that match one of the provided `file-kind's."
;;   (lambda (file)
;;     (some (sera:eqs (kind file))
;;           (cons kind more-kinds))))

;; (deftype user-specifier ()
;;   "A `string' identifies the user name, a `fixnum' the user ID."
;;   `(or string fixnum))

;; (export-always 'user=)
;; (-> user= (user-specifier &rest user-specifier) function)
;; (defun user= (user &rest more-users)
;;   "Return a predicate for files that match one of the provided `user-specifier's."
;;   (lambda (file)
;;     (let ((user-name (user file)))
;;       (some (lambda (user)
;;               (typecase user
;;                 (string (string= user-name user))
;;                 (number (= user (user-id file)))))
;;             (cons user more-users)))))

;; (deftype group-specifier ()
;;   "A `string' identifies the group name, a `fixnum' the group ID."
;;   `(or string fixnum))

;; (export-always 'group=)
;; (-> group= (group-specifier &rest group-specifier) function)
;; (defun group= (group &rest more-groups)
;;   "Return a predicate for files that match one of the provided `group-specifier's."
;;   (lambda (file)
;;     (let ((group-name (group file)))
;;       (some (lambda (group)
;;               (typecase group
;;                 (string (string= group-name group))
;;                 (number (= group (group-id file)))))
;;             (cons group more-groups)))))


(export-always 'path~)
(defun path~ (path-element &rest more-path-elements)
  "Return a predicate that matches when one of the path elements is contained in
the file path.

Example:

(file-finder:finder* :predicates (list (path~ \"file\" \"lisp\")))
;; => (#F\"mediafile.lisp\" #F\"file.lisp\" #F\"predicates.lisp\")"
  (apply #'match-path path-element more-path-elements))

(export-always 'every-path~)
(defun every-path~ (path-element &rest more-path-elements)
  "Return a predicate that matches when all the path elements are contained in
the file path.

Example:

(file-finder:finder* :predicates (list (every-path~ \"file\" \"lisp\")))
;; => (F\"file.lisp\")"
  (apply #'every-match-path path-element more-path-elements))

(export-always 'path$)
(defun path$ (path-suffix &rest more-path-suffixes)
  "Return a predicate that matches when one of the path suffixes matches
the file path."
  (apply #'match-path-end path-suffix more-path-suffixes))

(export-always 'name=)
(defun name= (name &rest more-names)
  "Return a predicate that matches when one of the names matches, case sensitive, the file basename."
  (lambda (file)
    (some (lambda (name)
            (string= name (basename file)))
          (cons name more-names))))

(export-always 'iname=)
(defun iname= (name &rest more-names)
  "Return a predicate that matches when one of the names matches, case insensitive, the file basename."
  (lambda (file)
    (some (lambda (name)
            (equalp name (basename file)))
          (cons name more-names))))

(export-always 'name~)
(defun name~ (name &rest more-names)
  "Return a predicate that matches when one of the names is contained in the
file basename (case sensitive). "
  (lambda (file)
    (some (lambda (name)
            (str:contains? name (basename file)))
          (cons name more-names))))

(export-always 'iname~)
(defun iname~ (name &rest more-names)
  "Return a predicate that matches when one of the names is contained in the
file basename (case insensitive). "
  (lambda (file)
    (some (lambda (name)
            (str:contains? name (basename file)) :ignore-case t)
          (cons name more-names))))

(export-always 'every-name~)
(defun every-name~ (name &rest more-names)
  "Return a predicate that matches when all the names are contained in the
file basename."
  (lambda (file)
    (every (lambda (name)
            (str:contains? name (basename file)))
          (cons name more-names))))

(defun depth< (level &optional (root (file *default-pathname-defaults*)))
  "Return a predicate that matches when the argument file is in a subdirectory
of ROOT less deep than LEVEL."
  (apply #'match-depth< level root))

;; (export-always 'executable?)
;; (defun executable? (file)
;;   "Return T when this file has executable permissions.

;; Example:

;; (file-finder:finder* :predicates (list #'executable?))"
;;   (intersection
;;    (permissions file)
;;    '(:user-exec :group-exec :other-exec)))

(export-always 'hidden?)
(defun hidden? (file)
  "Return T if this is a 'hidden' file.

Unix only (hidden files start with \".\" (dot)."
  (str:starts-with? "." (basename file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: needs to find a portable way for windows.
;;       it could be moved into an extension package.
;; (export-always 'elf-binary?)
;; (defun elf-binary? (file)
;;   (and (slot-boundp file 'mime-type)
;;        (string= "application/x-executable" (mime-type file))))

;; (export-always 'elf-library?)
;; (defun elf-library? (file)
;;   (and (slot-boundp file 'mime-type)
;;        (ppcre:scan "application/x-sharedlib" (first (mime-type file)))))
