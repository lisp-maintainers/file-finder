
(in-package file-finder)

(defvar *touch-command* "touch") ; TODO: `utime' syscall binding is missing from Osicat.

;; TODO: Add option to follow symlinks or not.

;; TODO: Run multiple disk writes within a transation?
;; Need proper POSIX bindings.  Can Osicat do all of them?
;; Could we edit files virtually?  Does that even make sense?

(export-always 'file-kind)
(deftype file-kind ()
  `(member :directory
           :character-device
           :block-device
           :regular-file
           :symbolic-link
           :socket
           :pipe))

(defclass file ()
    ((path :initform (error "Path required")
           :initarg :path
           :type string
           :reader path)
     (inode :initform 0
            :reader inode)
     (link-count :initform 0
                 :reader link-count)
     (kind :initform :regular-file              ; "kind" because `type' is reserved by CL.
           :type file-kind
           :reader kind)
     (size :initform 0
           :reader size)
     (disk-usage :initform 0
                 :reader disk-usage)
     ;; (user-id 0)
     ;; (group-id 0)
     ;; TODO: Include blocks?
     (creation-date :initform (local-time:unix-to-timestamp 0)
                    :reader creation-date)
     (modification-date :initform (local-time:unix-to-timestamp 0)
                        :reader modification-date)
     (access-date :initform (local-time:unix-to-timestamp 0)
                  :reader access-date)
     ;; (permissions '()
     ;;              :type (or null
     ;;                        (cons #.(cons 'member (mapcar #'first osicat::+permissions+)))))
     )
    ;; (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name))
    ;; Easily export symbols at once: M-x slime-export-class => exports 8 symbols at once.
    ;; (:export-slot-names-p t)
    ;; (:export-class-name-p t)
  )

;; (defmethod (setf user-id) (id (file file))
;;   (osicat-posix::chown (path file) id (group-id file))
;;   (setf (slot-value file 'user-id) id))

;; (defmethod (setf group-id) (id (file file))
;;   (osicat-posix::chown (path file) (user-id file) id)
;;   (setf (slot-value file 'group-id) id))

;; TODO: For now, date setters rely on GNU touch.  Find a portable version.
(defmethod (setf modification-date) (timestamp (file file))
  "Set both the `modification-date' and the `access-date' of FILE."
  (uiop:run-program (list *touch-command*
                          (format nil "--date=~a" (local-time:format-rfc3339-timestring nil timestamp))
                          (path file)))
  (setf (slot-value file 'modification-date) timestamp)
  (setf (slot-value file 'access-date) timestamp))

(defmethod (setf access-date) (timestamp (file file))
  (uiop:run-program (list *touch-command*
                          "-a"
                          (format nil "--date=~a" (local-time:format-rfc3339-timestring nil timestamp))
                          (path file)))
  (setf (slot-value file 'modification-date) timestamp))

;; (defmethod (setf permissions) (permissions (file file))
;;   (setf (osicat:file-permissions (path file)) permissions)
;;   (setf (slot-value file 'permissions) permissions))

(defmethod uiop-path ((file file))
  (uiop:parse-native-namestring (path file)))

(defmethod uiop-path ((s string))
  (uiop:parse-native-namestring s))

(defmethod uiop-path ((p pathname))
  p)

(defmethod path ((s string))
  "Useful so that `path' can be called both on a `file' or a `string'.
A trailing separator is automatically append for directories, if missing.
This is to be consistent with the `path' method for `file'. "
  (or (ignore-errors (path (file s)))
      s))

(defmethod path ((p pathname))
  "Useful so that `path' can be called both on a `file' or a `pathname'.
A trailing separator is automatically append for directories, if missing.
This is to be consistent with the `path' method for `file'."
  (or (ignore-errors (path (file p)))
      (namestring p)))

(defmethod (setf path) (new-path (file file))
  "Set FILE to a NEW-PATH.
This renames the file."
  (unless (handler-case (or (ignore-errors (rename-file (path file) new-path))
                            ;; Fallback since `rename-file' does not work cross-device.
                            (uiop:run-program (list "mv" (path file) new-path)))

            (error (c)
              (warn "Renaming ~s to ~s failed: ~a" (path file) new-path c)
              :error))
    (setf (path file) new-path)))

;; (export-always 'user)
;; (defmethod user ((file file))
;;   "Return the name of the user owning the file."
;;   (nth-value 0 (alex:assoc-value (osicat:user-info (user-id file)) :name)))

(defun read-/etc/group ()
  (let ((content (alex:read-file-into-string "/etc/group")))
    (mapcar (alex:curry #'str:split ":") (str:split (string #\newline) content))))

;; (defun group-id->name (id)
;;   (let ((result (find (write-to-string id) (read-/etc/group)
;;                       :key #'caddr :test #'string=)))
;;     (when result
;;       (first result))))

;; (export-always 'group)
;; (defmethod group ((file file))
;;   "Return the name of the group owning the file."
;;   (group-id->name (group-id file)))

(export-always 'extension)
(defmethod extension ((file file))
  "Return the file extension.
If none, return the empty string unlike `pathname-type'."
  (or (pathname-type (path file))
      ""))

(defmethod (setf extension) (new-extension (file file))
  "Set the FILE extension to NEW-EXTENSION.
This renames the file."
  (setf (path file)
        (str:concat (namestring (uiop:pathname-directory-pathname (path file)))
                    (pathname-name (path file))
                    "."
                    new-extension)))

(export-always 'directory?)
(defmethod directory? ((file file))
  (eq (kind file) :directory))

(export-always 'file?)
(defmethod file? ((file file))
  (eq (kind file) :regular-file))

(export-always 'file=?)
(defun file=? (file1 file2)
  "Return true if FILE1 and FILE2 point to the same file.
They might not be the same objects."
  (and (typep file1 'file) (typep file2 'file)
       (string= (path file1)
                (path file2))))

(export-always 'separator)
(defun separator (&optional char?)
  (if char?
      (uiop:directory-separator-for-host)
      (string (uiop:directory-separator-for-host))))

(export-always 'basename)
(defmethod basename ((file file))
  "Return the file basename (including the extension).
This returns the directory name for directories."
  (let* ((path (path file))
         (path  (if (str:ends-with? (separator) path)
                    (subseq path 0 (1- (length path)))
                    path))
	 (last-separator (or (position (separator :char)
				    path :from-end t)
			     0)))
    (subseq path
            (1+ last-separator))))

(export-always 'exists?)
(defmethod exists? ((file file))
  (and
   (if (directory? file)
       (uiop:directory-exists-p (uiop-path file))
       (uiop:file-exists-p (uiop-path file)))
   file))

(export-always 'parent)
(defmethod parent ((file file))
  "Return the parent directory of FILE."
  (file
   (if (directory? file)
       (uiop:pathname-parent-directory-pathname (uiop-path file))
       (uiop:pathname-directory-pathname (uiop-path file)))))

(export-always 'current-directory)
(defun current-directory ()
  (file *default-pathname-defaults*))

(export-always 'with-current-directory)
(defmacro with-current-directory ((&optional file) &body body)
  "Call BODY while the POSIX current working directory is set to FILE.
This is just like `uiop:with-current-directory' except that it takes a `file'
object."
  `(uiop:call-with-current-directory ,(path file) #'(lambda () ,@body)))

(defmethod disk-usage* ((file file))
  "Compute recursive `disk-usage' of FILE if a directory.
Return the new disk-usage."
  (if (directory? file)
      (if (list-directory file)
          (reduce #'+ (mapcar #'disk-usage* (list-directory file)))
          (size file))
      (slot-value file 'disk-usage)))

(defmethod disk-usage ((file file))
  "Return FILE `disk-usage'.
If FILE is a directory and it's disk-usage is 0 (never computed before), set it
with `disk-usage*' and return the new value."
  (if (or (file? file)
          (/= 0 (slot-value file 'disk-usage)))
      (slot-value file 'disk-usage)
      (disk-usage* file)))

(defun depth (file parent)
  "Return NIL if FILE is not a child of PARENT."
  (if (file=? file parent)
      0
      (unless (file=? (parent file) file)
        (or (when (file=? (parent file) parent)
              1)
            (alex:when-let ((level (depth (parent file) parent)))
              (1+ level))))))

(defun parent? (file parent)
  "Return true if PARENT is a parent of FILE."
  (sera:true (depth file parent)))

(export-always 'relative-path)
(defmethod relative-path ((path pathname) &optional (parent-directory (current-directory)))
  "Return PATH relative to PARENT-DIRECTORY.
If PARENT-DIRECTORY is not a parent of PATH, return PATH."
  (let ((file-finder-path (uiop:unix-namestring path)))
    (if (str:starts-with? (path parent-directory)
                          file-finder-path)
        (subseq file-finder-path (length (path parent-directory)))
        file-finder-path)))

(defmethod relative-path ((file file) &optional (parent-directory (current-directory)))
  "Return path of FILE relative to PARENT-DIRECTORY.
If PARENT-DIRECTORY is not a parent of FILE, return FILE's path."
  (if (str:starts-with? (path parent-directory)
                        (path file))
      (subseq (path file) (length (path parent-directory)))
      (path file)))

(defun shorten-home (path)
  (let ((home (uiop:getenv "HOME")))
    (if (str:starts-with? home path)
        (str:replace-first home "~" path)
        path)))

(declaim (ftype (function (string &key
                                  (:abbreviation-length integer)
                                  (:abbreviation-threshold integer)
                                  (:abbreviate-home? boolean)
                                  (:ellipsis string))
                          string)
                shorten-path))
(defun shorten-path (path &key (abbreviation-length 1) ; TODO: Is there a library for this?
                            (abbreviation-threshold 80)
                            (abbreviate-home? t)
                            (ellipsis "…"))
  "When ABBREVIATE-HOME?, abbreviate user home directory regardless of
ABBREVIATION-THRESHOLD."
  (let* ((path (if abbreviate-home?
                   (shorten-home path)
                   path))
         (length (length path)))
    (if (or (<= abbreviation-threshold 0)
            (<= length abbreviation-threshold))
        path
        (let ((elements (str:split (separator) path :omit-nulls t)))
          (if elements
              (labels ((shorten (dir)
                         (if (<= (length dir) (+ abbreviation-length (length ellipsis)))
                             dir
                             (str:concat
                              (subseq dir 0 abbreviation-length)
                              ellipsis)))
                       (maybe-shorten (dir-list total-length)
                         (if (or (null dir-list)
                                 (<= total-length abbreviation-threshold))
                             dir-list
                             (let ((short (shorten (first dir-list))))
                               (cons short
                                     (maybe-shorten (rest dir-list)
                                                    (- total-length
                                                       (- (length (first dir-list))
                                                          (length short)))))))))
                (the (values string &optional)
                     (str:concat (when (str:starts-with? (separator) path)
                                   (separator))
                                 (str:join
                                  (separator)
                                  (append
                                   (maybe-shorten (butlast elements) length)
                                   (list (first (last elements)))))
                                 (when (str:ends-with? (separator) path)
                                   (separator)))))
              path)))))


(defparameter +ls-time-format+
  '(:short-month #\space (:day 2 #\ ) #\space  (:hour 2) #\: (:min 2)))


(export-always '*print-reader-macro*)
(declaim (type string *print-reader-macro*))
(defvar *print-reader-macro* "#F")

(export-always '*print-relative-path?*)
(declaim (type boolean *print-relative-path?*))
(defvar *print-relative-path?* nil)

(export-always '*print-abbreviate-home?*)
(declaim (type boolean *print-abbreviate-home?*))
(defvar *print-abbreviate-home?* t
  "Whether to abbreviate the user home directory to '~'.")

(export-always '*print-abbreviation-threshold*)
(declaim (type integer *print-abbreviation-threshold*))
(defvar *print-abbreviation-threshold* 80
  "Abbreviate printout if strictly longer than this value.
Set to 0 to stop abbreviating.")

(export-always '*print-abbreviation-length*)
(declaim (type integer *print-abbreviation-length*))
(defvar *print-abbreviation-length* 3
  "Maximum abbreviation length, for each directory.")

(export-always '*print-size?*)
(declaim (type boolean *print-size?*))
(defvar *print-size?* nil)

(export-always '*print-date?*)
(declaim (type boolean *print-date?*))
(defvar *print-date?* nil)

(defun print-file (file stream
                   &key
                     (reader-macro *print-reader-macro*)
                     (relative-path? *print-relative-path?*)
                     (abbreviation-length *print-abbreviation-length*)
                     (abbreviation-threshold *print-abbreviation-threshold*)
                     (abbreviate-home? *print-abbreviate-home?*)
                     (size? *print-size?*)
                     (date? *print-date?*))
  (let ((path (if relative-path?
                  (relative-path file)
                  (path file))))
    (format stream "~a\"~a~a~a\""
            reader-macro
            (shorten-path path :abbreviation-length abbreviation-length
                               :abbreviation-threshold abbreviation-threshold
                               :abbreviate-home? abbreviate-home?)
            (if (and (directory? file)
                     (not (str:ends-with? "/" (path file))))
                "/" "")
            (str:concat
             (when size?
               (str:concat " " (sera:format-human-size nil (size file) :space nil)))
             (when date?
               (str:concat " " (local-time:format-timestring nil (modification-date file)
                                                             :format +ls-time-format+)))))))

;; TODO: Support `*print-pretty*'?
;; TODO: `*print-readably*'?
;; TODO: Auto-update file when mtime changes?  Wouldn't it be too slow?
(defmethod print-object ((file file) stream)
  (print-file file stream))

(export-always 'file)
(defmethod initialize-instance :after ((file file) &key)
  (let* ((path (path file))
         (native-path (ignore-errors
                       (uiop:ensure-pathname path
                                             :truename t
                                             :want-existing t)))
         (native-namestring (uiop:native-namestring native-path)))

    (unless (or (uiop:file-exists-p native-path)
                (uiop:directory-exists-p native-path))
      (error "~s is not a file path and does not exist" (or native-path path)))
    ;; TODO: What do we do with non-existent files (e.g. unsaved emacs buffers)?  Just return nil?
    (setf (slot-value file 'path) native-namestring)

    (setf (slot-value file 'creation-date)
          ;; we get 1970
          (ignore-errors
           ;; unsaved emacs buffers starting with .# are annoying.
           (local-time:universal-to-timestamp
            (file-attributes:creation-time native-namestring))))

    (setf (slot-value file 'modification-date)
          (ignore-errors
           (local-time:universal-to-timestamp
            (file-attributes:modification-time native-namestring))))

    (setf (slot-value file 'access-date)
          (ignore-errors
           (local-time:universal-to-timestamp
            (file-attributes:access-time native-namestring))))


    ;; Use `lstat' to _not_ follow symlinks, unlike `stat'.
    ;; (let ((stat (ignore-errors (osicat-posix:lstat native-path))))
    ;;   (if stat
    ;;       ;; From Osicat's `file-permissions':
    ;;       (flet ((stat-permissions (stat)
    ;;                (let ((mode (osicat-posix:stat-mode stat)))
    ;;                  (loop for (name . value) in osicat::+permissions+
    ;;                        when (plusp (logand mode value))
    ;;                          collect name))))
    ;;         (setf
    ;;          (slot-value file 'inode) (osicat-posix:stat-ino stat)
    ;;          (slot-value file 'link-count) (osicat-posix:stat-nlink stat)
    ;;          (slot-value file 'kind) (osicat:file-kind native-path) ; TODO: Don't recall `stat'.
    ;;          (slot-value file 'size) (osicat-posix:stat-size stat)
    ;;          (slot-value file 'disk-usage) (* 512 (osicat-posix:stat-blocks stat)) ; 512 as per (2)stat.
    ;;          (slot-value file 'user-id) (osicat-posix:stat-uid stat)
    ;;          (slot-value file 'group-id) (osicat-posix:stat-gid stat)
    ;;          ;; (slot-value file 'permissions)
    ;;          ;; (stat-permissions stat)
    ;;          ))
    ;;       ;; Errors may happen in particular for broken symlinks, see
    ;;       ;; https://github.com/osicat/osicat/issues/40
    ;;       (warn "Failed to retrieve ~s metadata" (slot-value file 'path))))
    ))

(defun file (path)
  (make-instance 'file :path (if (typep path 'file)
                                 (path path)
                                 path)))

(defun file-reader (stream char1 char2)
  (declare (ignore char1 char2))
  (let ((path-string (read stream)))
    (check-type path-string string)
    (file path-string)))

(export-always 'syntax)
(named-readtables:defreadtable file-finder::syntax
  (:merge :standard)
  (:dispatch-macro-char #\# #\f 'file-reader))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(export-always 'list-directory)
(defun list-directory (&optional (path (current-directory)) sort)
  "Return entries in PATH.
By default, directories come first.
If SORT is non nil, sort them alphabetically.
Second value is the list of directories, third value is the non-directories."
  ;; TODO: Use locale to sort?
  (let* ((subdirs (mapcar #'file (uiop:subdirectories (uiop-path path))))
         (subfiles (mapcar #'file (uiop:directory-files (uiop-path path))))
         (result (append subdirs subfiles)))
    (values
     (if sort
         (sort result #'string< :key #'path)
         result)
     subdirs
     subfiles)))

(export-always '*include-directories*)
(defvar *include-directories* nil
  "When non-nil `finder' includes directories in its search results.")
(export-always '*include-hidden-files*)
(defvar *include-hidden-files* nil
  "When non-nil `finder' includes hidden files in the result.")
(export-always '*descend-hidden-directories*)
(defvar *descend-hidden-directories* nil
  "When non-nil `finder' descends into hidden directories as well.")

(export-always '*exclude-directories*)
(defparameter *exclude-directories* (list #p"node_modules/")
  "Exclude these directories. Typically: node_modules.

Use pathnames with #p instead of simple strings.")

(export-always '*file-constructor*)
(defvar *file-constructor* #'file
  "Function that takes a path and returns a `file'-like object.")

(defun ensure-match-path-predicates (predicates)
  "If a string is given inside PREDICATES, return a `match-path' predicate.

This allows an easier use of finder*."
  (reverse
   (loop for pred in (uiop:ensure-list predicates)
         if (stringp pred)
           collect (match-path pred)
         else
           collect pred)))

(export-always 'finder*)
(defun finder* (&key
                  (root (current-directory))
                  predicates
                  recur-predicates)
  "List FILES (including directories) that satisfy all PREDICATES.
Without PREDICATES, list all files.

Recur in all subdirectories by default.
With RECUR-PREDICATES, recur only in subdirectories that satisfy the list of predicates.
"
  (let ((result '())
        (predicates (ensure-match-path-predicates predicates)))
    (uiop:collect-sub*directories
     (uiop:ensure-directory-pathname (path root))
     (constantly t)
     (lambda (dir)
       (every (alex:rcurry #'funcall (file dir)) recur-predicates))
     (lambda (subdirectory)
       (setf result (nconc result
                           (let ((subfiles (mapcar *file-constructor*
                                                   (append (uiop:subdirectories subdirectory)
                                                           (uiop:directory-files subdirectory)))))
                             (if predicates
                                 (delete-if (lambda (file)
                                              (notevery (alex:rcurry #'funcall file) predicates))
                                            subfiles)
                                 subfiles))))))
    result))

(defun match-path (path-element &rest more-path-elements)
  "Return a predicate that matches when one of the path elements is contained in
the file path.

This is the one used when we give a string for predicate.

Useful for `finder'."
  (lambda (file)
    (some (lambda (elem)
            (str:contains? elem (path file)))
          (cons path-element more-path-elements))))

(defun every-match-path (path-element &rest more-path-elements)
  "Return a predicate that matches when all path elements are contained in the file path.

Useful for `finder' or `finder*', but giving strings one after the other should be sufficient."
  (lambda (file)
    (every (lambda (elem)
             (str:containsp elem (path file)))
           (cons path-element more-path-elements))))

#+(or)
(finder "pack" "lisp")
;; is equal to:

#+(or)
(equal (finder* :predicates (list "pack" "lisp"))
       (finder* :predicates (every-match-path "pack" "lisp")))


(defun match-path-end (path-suffix &rest more-path-suffixes)
  "Return a predicate that matches when one of the path suffixes matches
the file path.
Useful for `finder'."
  (lambda (file)
    (some (lambda (suffix)
            (str:ends-with? (namestring suffix) (path file)))
          (cons path-suffix more-path-suffixes))))

(defun match-depth< (level &optional (root (current-directory)))
  "Return a predicate that matches when the argument file is in a subdirectory
of ROOT less deep than LEVEL."
  (lambda (file)
    (< (depth file root) level)))

(deftype function-specifier ()
  `(or function
       (and symbol (satisfies fboundp))))

(defun %specifier->predicate (specifier)
  (cond
    ((and specifier
          (stringp specifier))
     (match-path specifier))
    ((and specifier
          (pathnamep specifier))
     (match-path-end specifier))
    ((consp specifier)
     (apply #'alex:disjoin
            (mapcar #'%specifier->predicate specifier)))
    ((and specifier
          (typep specifier 'function-specifier))
     specifier)
    (t
     (error "Unknown predicate specifier: ~a" specifier))))

(export-always 'finder)
(defun finder (&rest predicate-specifiers) ; TODO: Add convenient regexp support?  Case-folding? Maybe str:*ignore-case* is enough.
  "List files in current directory that satisfy all PREDICATE-SPECIFIERS.
Directories are ignored.
Without PREDICATE-SPECIFIERS, list all files.

A predicate specifier can be:

- a string, in which case it is turned into (match-path STRING);
- a pathname, in which case it is turned into (match-path-end PATHNAME);
- a list of predicates, in which case it is turned into (apply #'alexandria:disjoin PREDICATES);
- a function (a predicate).

Passing a list of predicates connects them with a logical OR.

Ignore all directories specified in *exclude-directories* (node_modules).

Examples:

(finder \"pred\" \"lisp\")  ; => list all files that have these two strings in their path name.
;; => \"lisp\" matches \"quicklisp/local-projects/\".

(finder \"file\" (extension= \"lisp\")) ; => list all files whose path contain \"file\" with a \".lisp\" extension.
;; =>
(#F\"~/quicklisp/local-projects/file-finder/predicates.lisp\")

(finder (list \"file\" (extension= \"lisp\"))) ; => list all files matching one or the other predicate
; => many more results.


For a more tunable finder, see `finder*'."
  (labels ()
    (finder* :root (current-directory)
             :recur-predicates (append (unless *descend-hidden-directories*
                                         (list (complement #'hidden?)))

                                       ;; exclude node_modules.
                                       (when *exclude-directories*
                                         (mapcar #'complement
                                                 (mapcar #'%specifier->predicate
                                                         *exclude-directories*))))

             :predicates (append (unless *include-directories*
                                   (list (complement #'directory?)))
                                 (unless *include-hidden-files*
                                   (list (complement #'hidden?)))
                                 (mapcar #'%specifier->predicate
                                         predicate-specifiers)

                                 ;; exclude node_modules.
                                 (when *exclude-directories*
                                   (mapcar #'complement
                                           (mapcar #'%specifier->predicate
                                                   *exclude-directories*)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `ls -l' proof-of-concept replacement.

;; (defun permissions->unix (permissions)
;;   (format nil "~a~a~a~a~a~a~a~a~a"
;;           (if (find :user-read permissions) "r" "-")
;;           (if (find :user-write permissions) "w" "-")
;;           (if (find :user-exec permissions) "x" "-")
;;           (if (find :group-read permissions) "r" "-")
;;           (if (find :group-write permissions) "w" "-")
;;           (if (find :group-exec permissions) "x" "-")
;;           (if (find :other-read permissions) "r" "-")
;;           (if (find :other-write permissions) "w" "-")
;;           (if (find :other-exec permissions) "x" "-")))

;; (defun max-width (files reader &key (key #'write-to-string))
;;   (apply #'max (mapcar #'length
;;                        (mapcar (lambda (file)
;;                                  (funcall key (funcall reader file)))
;;                                files))))

;; (defun ls-l (&key human-readable?)
;;   "Mimicks Unix' `ls -l'."
;;   ;; TODO: Add support for file arguments?
;;   (let* ((current-dir-entries (finder* :recur-predicates (list (constantly nil))))
;;          (size-column-width (max-width current-dir-entries #'size)))
;;     (dolist (file current-dir-entries)
;;       (format t (str:concat "~a~a ~a ~a ~a ~" (write-to-string size-column-width) "@a ~a ~a~%")
;;               (if (directory? file) "d" "-")
;;               (permissions->unix (permissions file))
;;               (link-count file)
;;               (user file)
;;               (group file)
;;               (if human-readable?
;;                   (serapeum:format-file-size-human-readable nil (size file))
;;                   (size file))
;;               (local-time:format-timestring nil (modification-date file)
;;                                             :format +ls-time-format+)
;;               (relative-path file)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

