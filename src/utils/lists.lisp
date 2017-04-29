(in-package :cl-data-structures.utils)


(defun insert-or-replace (set element &key (test #'eql) (key #'identity))
  "Insert element into set if it is not already here"
  (let ((last-cell nil)
        (result nil))
    (iterate
      (for elt in set)
      (for rest on set)
      (cond+ ((funcall test (funcall key elt) (funcall key element))
              last-cell)
        ((nil nil) (progn (push elt result)
                          (setf last-cell result)))
        ((nil t) (push elt result))
        ((t nil) (progn (push element result)
                        (setf last-cell result
                              (cdr last-cell) (cdr rest))
                        (return-from insert-or-replace (values result t elt))))
        ((t t) (progn (push element result)
                      (setf (cdr last-cell) (cdr rest))
                      (return-from insert-or-replace (values result t elt))))))
    (values (cons element set) nil nil)))

