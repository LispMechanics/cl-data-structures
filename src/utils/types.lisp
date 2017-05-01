(in-package :cl-ds.utils)


(deftype extendable-vector ()
  `(and vector
        (not simple-array)
        (satisfies array-has-fill-pointer-p)))


(deftype index ()
  `(integer 0 ,ARRAY-TOTAL-SIZE-LIMIT))
