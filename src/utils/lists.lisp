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


(-> try-remove (t list &key (:test (-> (t t) boolean)) (:key (-> (t) t))) list)
(defun try-remove (item list &key (test #'eql) (key #'identity))
  (iterate
    (for elt in list)
    (with removed = nil)
    (if (funcall test
                 (funcall key elt)
                 item)
        (setf removed t)
        (collect elt into result at start))
    (finally (return (values result removed)))))


(defun try-find (item list &key (test #'eql) (key #'identity))
  (let ((r nil)
        (f nil))
    (iterate
      (for elt in list)
      (when (funcall test
                     (funcall key elt)
                     item)
        (setf r elt
              f t)
        (leave)))
    (values r f)))
