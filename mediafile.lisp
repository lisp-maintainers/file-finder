(uiop:define-package fof/mediafile
  (:documentation "Mediafile class (audio, video, pictures).")
  (:use #:common-lisp)
  (:import-from #:hu.dwim.defclass-star #:defclass*)
  (:import-from #:serapeum #:export-always)
  (:use #:fof)
  (:import-from #:fof/ffprobe))
(in-package fof/mediafile)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria)
  (trivial-package-local-nicknames:add-package-local-nickname :sera :serapeum))

;; TODO: Add reader table + #mf print-object.

(defclass* mediafile (file+mime)        ; REVIEW: Include MIME?
    ((media-format nil
                   :type (or null fof/ffprobe:media-format))
     (media-streams '()
                    :type (or null
                              (cons fof/ffprobe:media-stream))))
    (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name))
    (:export-accessor-names-p t)
    (:export-class-name-p t))

(defmethod initialize-instance :after ((file mediafile) &key)
  (let ((probe (fof/ffprobe:ffprobe (path file))))
    (setf (media-format file) (first probe)
          (media-streams file) (rest probe))))

(defmethod print-object ((file mediafile) stream)
  (let ((*print-reader-macro* "#MF"))
    (fof/file::print-file file stream)))

(defun mediafile-reader (stream char1 char2)
  (declare (ignore char1 char2))
  (when (eq (read-char stream nil :eof) #\f)
    (let ((path-string (read stream)))
      (check-type path-string string)
      (mediafile path-string))))

(export-always 'syntax)
(named-readtables:defreadtable fof/mediafile::syntax
  (:merge :standard)
  ;; TODO: Can we merge fof/file::syntax instead of redefining #f?
  (:dispatch-macro-char #\# #\f #'fof/file::file-reader)
  (:dispatch-macro-char #\# #\m #'mediafile-reader))

(export-always 'mediafile)
(defun mediafile (path)
  (make-instance 'mediafile :path path))

(export-always 'mediafinder*)
(defun mediafinder* (&rest args
                           &key
                           (root *default-pathname-defaults*)
                           predicates
                           recur-predicates)
  (declare (ignore root predicates recur-predicates))
  (let ((*finder-constructor* #'mediafile))
    (apply #'finder* args)))

(export-always 'mediafinder)
(defun mediafinder (&rest predicate-specifiers)
  (let ((*finder-constructor* #'mediafile))
    (apply #'finder predicate-specifiers)))

(export-always 'width)
(defmethod width ((file mediafile))
  (fof/ffprobe:width
   (find-if #'plusp (media-streams file)
            :key #'fof/ffprobe:width)))

(export-always 'height)
(defmethod height ((file mediafile))
  (fof/ffprobe:height
   (find-if #'plusp (media-streams file)
            :key #'fof/ffprobe:height)))

(export-always 'tags)
(defmethod tags ((file mediafile))
  ;; TODO: Get tags from streams too?
  (fof/ffprobe:tags (media-format file)))
