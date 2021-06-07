#-asdf3.1 (error "`fof' requires ASDF 3.1")

(defsystem "fof"
  :version "0.2.0"
  :author "Pierre Neidhardt <mail@ambrevar.xyz>"
  :homepage "https://gitlab.com/ambrevar/fof"
  :licence "GPL3+"
  :class :package-inferred-system
  :depends-on ("fof/package"))

(defsystem "fof/mf"
  :class :package-inferred-system
  :depends-on ("fof/mediafile"))
