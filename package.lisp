(defpackage :cl-data-structures
  (:use :common-lisp)
  (:nicknames :cl-ds)
  (:export
   ;; generic functions
   :at
   :erase
   :erase!
   :add
   :add!
   :insert
   :size
   :update
   :update!
   :become-functional
   :mutable-p
   :functional-p
   ;; trait classes
   :functional
   :mutable
   ))
