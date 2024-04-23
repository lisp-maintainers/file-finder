#-asdf3.1 (warning "`fof' used to require ASDF 3.1, after our update of <2024-04-23> that may not be the case anymore.")
;; <2024-04-23> After simplification and removal of inferred system, does this hold?

(in-package #:asdf-user)

(defsystem "file-finder"
  :version "0.2.0"
  :author "Pierre Neidhardt <mail@ambrevar.xyz>"
  :maintainer "vindarel <@ vindarel mailz.org>"
  :homepage ""
  :licence "GPL3+"
  :description "File finder. Enable rapid file search, inspection and manipulation."
  ;; :class :package-inferred-system
  :depends-on (;; "fof/package"
               "alexandria"
               "serapeum"
               "local-time"
               "magicffi"
               "str"
               "trivia")

  :components ((:file "file")
               (:file "predicates")
               ;; final package re-exporting the others:
               (:file "package")))
