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

   :hamt-dictionary-at

   :mutable-hamt-dictionary-insert!
   :mutable-hamt-dictionary-update!

   :functional-hamt-dictionary-insert
   :functional-hamt-dictionary-at
   :functional-hamt-dictionary-add
   :functional-hamt-dictionary-erase
   :functional-hamt-dictionary-update))

