(in-package :cl-data-structures.utils)


(defun insert-or-replace (set element &key (test #'eql) (list-key #'identity) (item-key #'identity))
  "Insert element into set if it is not already here"
  (let ((last-cell nil)
        (result nil))
    (iterate
      (for elt in set)
      (for rest on set)
      (cond+ ((funcall test (funcall list-key elt) (funcall item-key element))
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


(-> try-remove (t list &key (:test (-> (t t) boolean)) (:list-key (-> (t) t)) (:item-key (-> (t) t))) (values list boolean t))
(defun try-remove (item list &key (test #'eql) (list-key #'identity) (item-key #'identity))
  (iterate
    (for elt in list)
    (with removed = nil)
    (if (funcall test
                 (funcall list-key elt)
                 (funcall item-key item))
        (setf removed t)
        (collect elt into result at start))
    (finally (return (values result removed)))))


(defun try-find-cell (item list &key (test #'eql) (key #'identity))
  (iterate
    (for elt on list)
    (when (funcall test
                   (funcall key (car elt))
                   item)
      (return-from try-find-cell elt))))


(defun try-find (item list &key (test #'eql) (key #'identity))
  (multiple-value-bind (r) (try-find-cell item list :test test :key key)
    (values (car r)
            (not (null r)))))

