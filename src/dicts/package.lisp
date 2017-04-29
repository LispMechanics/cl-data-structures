(defpackage :cl-data-structures.dicts
  (:use :common-lisp :iterate :alexandria :serapeum :cl-ds.utils)
  (:nicknames #:cl-ds.dicts)
  (:shadowing-import-from :iterate :collecting :summing :in)
  (:export
   :fundamental-hash-tree-container
   :read-max-depth
   :hash-find
   :hash-remove
   :hash-insert
   :functional-dictionary
   :make-functional-dictionary
   :hash-map))


(in-package :cl-data-structures.dicts)


(export 'fundamental-hash-tree-container)
(export 'read-max-depth)
(export 'hash-find)
(export 'hash-remove)
(export 'hash-insert)
(export 'functional-dictionary)
(export 'make-functional-dictionary)
(export 'hash-map)
