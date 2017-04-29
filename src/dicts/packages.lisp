(defpackage :cl-data-structures.dicts
  (:use :common-lisp)
  (:nicknames :cl-ds.dicts)
  (:export
   :dictionary))


(defpackage :cl-data-structures.dicts.hamt
  (:use :common-lisp :iterate :alexandria :serapeum :cl-ds.utils)
  (:nicknames :cl-ds.dicts.hamt)
  (:shadowing-import-from :iterate :collecting :summing :in)
  (:export
   :read-max-depth
   :make-functional-hamt-dictionary
   :make-mutable-hamt-hamt-dictionary
   :functional-hamt-dictionary-at))

