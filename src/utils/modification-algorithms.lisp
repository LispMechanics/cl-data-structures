(in-package :cl-ds.utils)


(-> pop-last (extendable-vector) t)
(defun pop-last (vector)
  "If fill pointer is larger than zero reduce fill pointer and returns last vector element"
  (unless (zerop (fill-pointer vector))
    (prog1
        (aref vector (1- (fill-pointer vector)))
      (decf (fill-pointer vector)))))


(-> erase-from-vector (extendable-vector list) vector)
(defun erase-from-vector (vector indexes)
  "Remove elements under indexes from vector, preserving order of elements. Reduce fill-pointer."
  (iterate (for el in indexes)
    (for i from 0)
    (for index = (- el i))
    (replace vector vector
             :start1 index
             :start2 (1+ index))
    (finally (progn (decf (fill-pointer vector) (1+ i))
                    (return vector)))))


(-> swapop (extendable-vector index) vector)
(defun swapop (vector index)
  "Swaps element under INDEX with last element. Pops last element and returns VECTOR.
   :values and parameters"
  (unless (zerop (fill-pointer vector))
    (rotatef (aref vector (1- (fill-pointer vector)))
             (aref vector index))
    (decf (fill-pointer vector)))
  vector)
