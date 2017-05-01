(in-package #:cl-user)

(asdf:defsystem cl-data-structures
  :name "cl-data-structures"
  :version "0.0.0"
  :license "MIT"
  :author "Lisp Mechanics"
  :maintainer "Lisp Mechanics"
  :depends-on (:iterate :alexandria :serapeum :prove)
  :serial T
  :components ((:file "package")
               (:module api
                :pathname "src/api"
                :serial T
                :components ((:file "trait-classes")
                             (:file "generics")
                             (:file "conditions")))
               (:module utils
                :pathname "src/utils"
                :serial T
                :components ((:file "package")
                             (:file "macros")
                             (:file "types")
                             (:file "ordered-algorithms")
                             (:file "lists")
                             (:file "modification-algorithms")))
               (:module dicts
                :pathname "src/dicts"
                :serial T
                :components ((:file "packages")
                             (:file "trait-classes")
                             (:file "hamt/internal")
                             (:file "hamt/api")))))
