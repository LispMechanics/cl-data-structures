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
   :empty
   :size
   :update
   :update!
   :become-functional
   :become-mutable
   :mutable-p
   :functional-p
   ;; trait classes
   :fundamental-container
   :functional
   :mutable))
