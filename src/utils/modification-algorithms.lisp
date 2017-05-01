(in-package :cl-ds.utils)


(-> pop-last (extendable-vector) t)
(defun pop-last (vector)
  "If fill pointer is larger than zero reduce fill pointer and returns last vector element.

 @b(Side effects:) reduce fill pointer of vector.

 @b(Exceptional situations:) will return nil if fill pointer is equal 0."
  (unless (zerop (fill-pointer vector))
    (prog1
        (aref vector (1- (fill-pointer vector)))
      (decf (fill-pointer vector)))))


(-> erase-from-vector (extendable-vector &rest index) vector)
(defun erase-from-vector (vector &rest indexes)
  "Remove elements under indexes from vector, preserving order of elements.

 @b(Side effects:) reduce fill pointer."
  (iterate
    (for el in indexes)
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

   @b(Values and parameters:) vector -- input vector, index -- under which index element will be removed?

   @b(Side Effects:) changes element under INDEX to last element in the vector, reduces fill-pointer."
  (unless (zerop (fill-pointer vector))
    (rotatef (aref vector (1- (fill-pointer vector)))
             (aref vector index))
    (decf (fill-pointer vector)))
  vector)
