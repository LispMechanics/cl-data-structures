(defpackage :cl-data-structures.utils
  (:use :common-lisp :iterate :alexandria :serapeum)
  (:nicknames #:cl-ds.utils)
  (:shadowing-import-from :iterate :collecting :summing :in))


(in-package :cl-ds.utils)


(export 'lazy-let)
(export 'with-vectors)
(export 'bind-lambda)
(export 'merge-ordered-vectors)
(export 'cond+)
(export 'swapop)
(export 'erase-from-vector)
(export 'lastpop)
(export 'cond-compare)
