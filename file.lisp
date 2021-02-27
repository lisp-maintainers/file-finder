(uiop:define-package fof/file
  (:documentation "File class.")
  (:use #:common-lisp)
  (:import-from #:alexandria)
  (:import-from #:hu.dwim.defclass-star #:defclass*)
  (:import-from #:local-time)
  (:import-from #:magicffi)
  (:import-from #:serapeum #:export-always)
  (:import-from #:str)
  (:import-from #:trivia #:match))
(in-package fof/file)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria)
  (trivial-package-local-nicknames:add-package-local-nickname :sera :serapeum))

(defvar *chown-command* "chown")
(defvar *touch-command* "touch") ; TODO: `utime' syscall binding is missing from Osicat.

;; TODO: Run multiple disk writes within a transation?
;; Need proper POSIX bindings.  Can Osicat do that?
;; Could we edit files virtually?  Does that even make sense?

;; TODO: Implement disk-usage for directories.

;; TODO: Replace magicffi with trivial-mime once we can get MIME encoding
;; (https://github.com/Shinmera/trivial-mimes/issues/8), description, and fix
;; the probe-file issue.

(defclass* file ()
    ((path (error "Path required")
           :type string
           :reader t)
     (inode 0
            :reader t)
     (link-count 0
                 :reader t)
     (kind :regular-file              ; "kind" because `type' is reserved by CL.
           :type (member :directory
                         :character-device
                         :block-device
                         :regular-file
                         :symbolic-link
                         :socket
                         :pipe)
           :reader t)
     (size 0
           :reader t)
     (user-id 0)
     (group-id 0)
     ;; TODO: Include blocks?
     (creation-date (local-time:unix-to-timestamp 0)
                    :reader t)
     (modification-date (local-time:unix-to-timestamp 0))
     (access-date (local-time:unix-to-timestamp 0))
     (permissions '()
                  :type (or null
                            (cons #.(cons 'member (mapcar #'first osicat::+permissions+))))))
    (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name))
    (:export-slot-names-p t)
    (:export-class-name-p t))

;; TODO: Rewrite user-id and group-id using `osicat-posix::chown'.
(defmethod (setf user-id) (id (file file))
  (uiop:run-program (list *chown-command* (write-to-string id) (path file)))
  (setf (slot-value file 'user-id) id))

(defmethod (setf group-id) (id (file file))
  (uiop:run-program (list *chown-command* (format nil "~a:~a" (user-id file) id)
                          (path file)))
  (setf (slot-value file 'group-id) id))

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
                          (format nil "-a" "--date=~a" (local-time:format-rfc3339-timestring nil timestamp))
                          (path file)))
  (setf (slot-value file 'modification-date) timestamp))

(defmethod path ((s string))
  "Useful so that `path' can be called both on a `file' or a `string'."
  s)

(defmethod path ((p pathname))
  "Useful so that `path' can be called both on a `file' or a `pathname'."
  (namestring p))

(export-always 'user)
(defmethod user ((file file))
  "Return the name of the user owning the file."
  (nth-value 0 (alex:assoc-value (osicat:user-info (user-id file)) :name)))

(defun read-/etc/group ()
  (let ((content (alex:read-file-into-string "/etc/group")))
    (mapcar (alex:curry #'str:split ":") (str:split (string #\newline) content))))

(defun group-id->name (id)
  (let ((result (find (write-to-string id) (fof/file::read-/etc/group)
                      :key #'caddr :test #'string=)))
    (when result
      (first result))))

(export-always 'group)
(defmethod group ((file file))
  "Return the name of the group owning the file."
  (group-id->name (group-id file)))

(export-always 'extension)
(defmethod extension ((file file))
  "Return the file extension.
If none, return the empty string unlike `pathname-type'."
  (or (pathname-type (path file))
      ""))

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
         (last-separator (position (separator :char)
                                   path :from-end t)))
    (subseq path
            (1+ last-separator))))

(export-always 'parent)
(defmethod parent ((file file))
  "Return the parent directory of FILE."
  (file
   (if (directory? file)
       (uiop:pathname-parent-directory-pathname (path file))
       (uiop:pathname-directory-pathname (path file)))))

(defun depth (file parent)
  (cond
    ((file=? file parent)
     0)
    ((file=? file (parent file))
     0)
    (t
     (1+ (depth (parent file) parent)))))

(export-always 'relative-path)
(defmethod relative-path ((file file) &optional (parent-directory *default-pathname-defaults*))
  "Return PATH relative to PARENT-DIRECTORY.
If PARENT-DIRECTORY is not a parent of PATH, return PATH."
  (setf parent-directory (path (uiop:ensure-directory-pathname parent-directory)))
  (if (str:starts-with? parent-directory
                        (path file))
      (subseq (path file) (length parent-directory))
      (path file)))

(defun shorten-home (path)
  (let ((home (uiop:getenv "HOME")))
    (if (str:starts-with? home path)
        (str:replace-first home "~" path)
        path)))

(defun shorten-path (path &key (abbreviation-length 1) ; TODO: Is there a library for this?
                            (abbreviate-home t)
                            (ellipsis "…"))
  (let* ((path (if abbreviate-home
                   (shorten-home path)
                   path))
         (elements
           (str:split (separator) path
                      :omit-nulls t)))
    (if elements
        (str:concat (when (str:starts-with? (separator) path)
                      (separator))
                    (str:join
                     (separator)
                     (append
                      (mapcar (lambda (dir)
                                (if (<= (length dir) (+ abbreviation-length (length ellipsis)))
                                    dir
                                    (str:concat
                                     (subseq dir 0 abbreviation-length)
                                     ellipsis)))
                              (butlast elements))
                      (list (first (last elements)))))
                    (when (str:ends-with? (separator) path)
                      (separator)))
        path)))


(defparameter +ls-time-format+
  '(:short-month #\space (:day 2 #\ ) #\space  (:hour 2) #\: (:min 2)))

(defun make-object-printer (&key (abbreviation-length 2) size? date?)
  (lambda (file stream)
    (format stream "#F\"~a~a~a\""
            (if (= 0 abbreviation-length)
                (path file)
                (shorten-path (path file) :abbreviation-length abbreviation-length))
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
;; TODO: Don't referine the method, instead use a defvar.
;; TODO: Add `ls' convenience function.
;; TODO: Auto-update file when mtime changes?  Wouldn't it be too slow?
(defmethod print-object ((file file) stream)
  (funcall (make-object-printer) file stream))

(defun print-object-default ()
  (defmethod print-object ((file file) stream)
    (funcall (make-object-printer) file stream)))

(defun print-object-with-size ()
  (defmethod print-object ((file file) stream)
    (funcall (make-object-printer :size? t) file stream)))

(defun print-object-with-date ()
  (defmethod print-object ((file file) stream)
    (funcall (make-object-printer :size? t :date? t) file stream)))

(export-always 'file)
(defmethod initialize-instance :after ((file file) &key)
  (let* ((path (path file))
         (native-path (uiop:truename* (if (pathnamep path)
                                          path
                                          (uiop:parse-native-namestring path)))))
    (unless (or (uiop:file-exists-p native-path)
                (uiop:directory-exists-p native-path))
      (error "~s is not a file path" (or native-path path)))
    ;; TODO: What do we do with non-existent files (e.g. unsaved emacs buffers)?  Just return nil?
    (let ((stat (ignore-errors (osicat-posix:stat native-path))))
      (when stat
        ;; From Osicat's `file-permissions':
        (flet ((stat-permissions (stat)
                 (let ((mode (osicat-posix:stat-mode stat)))
                   (loop for (name . value) in osicat::+permissions+
                         when (plusp (logand mode value))
                           collect name))))
          (setf
           (slot-value file 'path) (uiop:unix-namestring native-path)
           (slot-value file 'inode) (osicat-posix:stat-ino stat)
           (slot-value file 'link-count) (osicat-posix:stat-nlink stat)
           (slot-value file 'kind) (osicat:file-kind native-path) ; TODO: Don't recall `stat'.
           (slot-value file 'size) (osicat-posix:stat-size stat)
           (slot-value file 'user-id) (osicat-posix:stat-uid stat)
           (slot-value file 'group-id) (osicat-posix:stat-gid stat)
           (slot-value file 'creation-date) (local-time:unix-to-timestamp (osicat-posix:stat-ctime stat))
           (slot-value file 'modification-date) (local-time:unix-to-timestamp (osicat-posix:stat-mtime stat))
           (slot-value file 'access-date) (local-time:unix-to-timestamp (osicat-posix:stat-atime stat))
           (slot-value file 'permissions) (stat-permissions stat)))))))

(defun file (path)
  (make-instance 'file :path path))

(defun read-until (stream delimiter)
  "Return the string read until DELIMITER."
  (concatenate 'string
               (loop :for char = (read-char stream nil :eof)
                     :while (and (not (eq char :eof))
                                 (not (char= char delimiter)))
                     :collect char)))

(defun file-reader (stream char1 char2)
  (declare (ignore char1 char2))
  (read-until stream #\")
  (file (read-until stream #\")))

(export-always 'readtable)
(named-readtables:defreadtable readtable
  (:merge :standard)
  (:dispatch-macro-char #\# #\f #'file-reader))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(export-always 'list-directory)
(defun list-directory (&optional (path *default-pathname-defaults*) sort)
  "Return entries in PATH.
By default, directories come first.
If SORT is non nil, sort them alphabetically.
Second value is the list of directories, third value is the non-directories."
  ;; TODO: Use locale to sort?
  (let* ((subdirs (mapcar #'file (uiop:subdirectories path)))
         (subfiles (mapcar #'file (uiop:directory-files path)))
         (result (append subdirs subfiles)))
    (values
     (if sort
         (sort result #'string< :key #'path)
         result)
     subdirs
     subfiles)))

(export-always '*finder-include-directories*)
(defvar *finder-include-directories* t  ; TODO: Use?
  "When non-nil `finder' includes directories.")

(export-always '*finder-constructor*)
(defvar *finder-constructor* #'file
  "Function that takes a path and returns a `file'-like object.")

(export-always 'finder*)
(defun finder* (&key
                (root *default-pathname-defaults*)
                (exclude-directories? nil)
                (max-depth 0)
                predicates)
  "List FILES (including directories) that satisfy all PREDICATES.
Without PREDICATES, list all files.

When MAX-DEPTH is 0, recurse indefinitely."
  (let ((result '())
        (root-file (file root)))
    (uiop:collect-sub*directories
     (uiop:ensure-directory-pathname root)
     (constantly t)
     (if (<= max-depth 0)
         (constantly t)
         (lambda (dir)
           (< (depth (file dir) root-file) max-depth)))
     (lambda (subdirectory)
       (setf result (nconc result
                           (let ((subfiles (mapcar *finder-constructor*
                                                   (append (when exclude-directories? (list subdirectory))
                                                           (uiop:directory-files subdirectory)))))
                             (if predicates
                                 (delete-if (lambda (file)
                                              (notevery (lambda (pred) (funcall pred file))
                                                        predicates))
                                            subfiles)
                                 subfiles))))))
    result))

(defun match-path (path-element &rest more-path-elements)
  "Return a predicate that matches when one of the path elements is contained in
the file path.
Useful for `finder'."
  (lambda (file)
    (some (lambda (elem)
            (str:contains? elem (path file)))
          (cons path-element more-path-elements))))

(defun match-path-end (path-suffix &rest more-path-suffixes)
  "Return a predicate that matches when one of the path suffixes matches
the file path.
Useful for `finder'."
  (lambda (file)
    (some (lambda (suffix)
            (str:ends-with? (namestring suffix) (path file)))
          (cons path-suffix more-path-suffixes))))

(export-always 'finder)
(defun finder (&rest predicate-specifiers) ; TODO: Add convenient regexp support?  Case-folding? Maybe str:*ignore-case* is enough.
  "List files in current directory that satisfy all PREDICATE-SPECIFIERS
Directories are ignored.
Without PREDICATE-SPECIFIERS, list all files.

A predicate specifier can be:

- a string, in which case it is turned into (match-path STRING);
- a pathname, in which case it is turned into (match-path-end PATHNAME);
- a list of predicates, in which case it is turned into (apply #'alexandria:disjoin PREDICATES);
- a function (a predicate).

For a more tunable finder, see `finder*'."
  (labels ((specifier->predicate (specifier)
             (match specifier
               ((and s (type string))
                (match-path s))
               ((and s (type pathname))
                (match-path-end s))
               ((cons pred1 more-preds)
                (apply #'alex:disjoin
                       (mapcar #'specifier->predicate
                               (cons pred1 more-preds))))
               ((and pred (type function))
                pred)
               (other
                (error "Unknown predicate specifier: ~a" other)))))
    (finder* :root *default-pathname-defaults*
             :exclude-directories? t
             :predicates (mapcar #'specifier->predicate
                                 predicate-specifiers))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter %magic-cookie-mime nil
  "Internal storage for `magic-cookie-mime'.")
(defun magic-cookie-mime ()
  "Return internal, persistent MIME cookie for `magicffi' calls.
Benchmark on thousands of files shows that
keeping the same cookie saves about 15% of time. "
  (when (or (not %magic-cookie-mime)
            (not (magicffi:open-magic-p %magic-cookie-mime)))
    (setf %magic-cookie-mime (magicffi:magic-open '(:symlink :mime)))
    (magicffi:magic-load %magic-cookie-mime))
  %magic-cookie-mime)

(defparameter %magic-cookie-description nil
  "Internal storage for `magic-cookie-description'.")
(defun magic-cookie-description ()
  "Same as `magic-cooke-mime' but for `file' descriptions.
See `%description'."
  (when (or (not %magic-cookie-description)
            (not (magicffi:open-magic-p %magic-cookie-description)))
    (setf %magic-cookie-description (magicffi:magic-open '(:symlink)))
    (magicffi:magic-load %magic-cookie-description))
  %magic-cookie-description)

(defun %mime-type+encoding (path)
  "Return a pair of MIME type and MIME encoding for PATH."
  (str:split "; "
             (magicffi:magic-file (magic-cookie-mime) path)))

(defun %description (path)
  "Return the PATH description as per the `file' UNIX command."
  (magicffi:magic-file (magic-cookie-description) path))

;; TODO: Include the description or do it in another class?  Could be slower.  Benchmark.
(defclass* file+mime (file)
    ((mime-type ""
                :reader t)
     (mime-encoding ""
                    :reader t)
     (description ""
                  :reader t))
    (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name))
    (:export-slot-names-p t)
    (:export-class-name-p t))

(defmethod initialize-instance :after ((file file+mime) &key)
  (let ((mime-type+encoding (%mime-type+encoding (path file))))
    (setf (slot-value file 'mime-type) (first mime-type+encoding)
          (slot-value file 'mime-encoding) (second mime-type+encoding)
          (slot-value file 'description) (%description (path file)))))

(defun file+mime (path)
  (make-instance 'file+mime :path path))

(defun finder*+mime (root &rest predicates)
  (let ((*finder-constructor* #'file+mime))
    (finder* :root root
             :predicates predicates)))

(defun finder+mime (&rest predicates)
  (let ((*finder-constructor* #'file+mime))
    (apply #'finder predicates)))
