(defpackage :cl-data-structures.utils
  (:use :common-lisp :iterate :alexandria :serapeum)
  (:nicknames :cl-ds.utils)
  (:shadowing-import-from :iterate :collecting :summing :in)
  (:export
   :extendable-vector
   :lazy-let
   :with-vectors
   :bind-lambda
   :merge-ordered-vectors
   :cond+
   :swapop
   :erase-from-vector
   :pop-last
   :cond-compare
   :insert-or-replace
   :try-find
   :try-find-cell
   :try-remove))


(in-package :cl-ds.utils)

