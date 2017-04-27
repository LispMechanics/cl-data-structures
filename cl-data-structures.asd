(in-package #:cl-user)

(asdf:defsystem cl-data-structures
  :name "cl-data-structures"
  :version "0.0.0"
  :license "MIT"
  :author "Lisp Mechanics"
  :maintainer "Lisp Mechanics"
  :depends-on (:iterate :alexandria :serapeum)
  :serial T
  :components ((:module utils
                :pathname "src/utils"
                :serial T
                :components ((:file "package")
                             (:file "macros")
                             (:file "ordered-algorithms")
                             (:file "modification-algorithms")))
               (:module dicts
                :pathname "src/dicts"
                :serial T
                :components ((:file "package")
                             (:file "hamt")))))
