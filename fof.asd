#-asdf3.1 (error "`fof' requires ASDF 3.1")

(defsystem "fof"
  :version "0.2.0"
  :author "Pierre Neidhardt <mail@ambrevar.xyz>"
  :homepage "https://gitlab.com/ambrevar/fof"
  :licence "GPL3+"
  :description "File-object finder. Enable rapid file search, inspection and manipulation."
  ;; :class :package-inferred-system
  :depends-on ("fof/package"
               "alexandria"
               "serapeum"
               "hu.dwim.defclass-star"
               "local-time"
               "magicffi"
               "str"
               "trivia"
               ))
