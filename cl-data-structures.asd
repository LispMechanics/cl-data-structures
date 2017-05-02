(in-package #:cl-user)

(asdf:defsystem cl-data-structures
  :name "cl-data-structures"
  :version "0.0.0"
  :license "MIT"
  :author "Lisp Mechanics"
  :maintainer "Lisp Mechanics"
  :depends-on (:iterate :alexandria :serapeum :prove)
  :serial T
  :pathname "src"
  :components ((:file "package")
               (:module "api"
                :components ((:file "trait-classes")
                             (:file "generics")
                             (:file "conditions")))
               (:module "utils"
                :components ((:file "package")
                             (:file "macros")
                             (:file "types")
                             (:file "ordered-algorithms")
                             (:file "lists")
                             (:file "modification-algorithms")))
               (:module "dicts"
                :components ((:file "packages")
                             (:file "trait-classes")
                             (:module "hamt"
                              :components ((:file "internal")
                                           (:file "api")))))))
