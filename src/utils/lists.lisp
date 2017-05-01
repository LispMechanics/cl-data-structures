(in-package :cl-data-structures.utils)


(-> insert-or-replace (list t &key (:test (-> (t t) boolean)) (:list-key (-> (t) t)) (:item-key (-> (t) t)) (:preserve-order boolean))
    list)
(defun insert-or-replace (list element &key (test #'eql) (list-key #'identity) (item-key #'identity) (preserve-order nil))
  "Insert element into set if it is not already here. Returns three values:
   @begin(list)
    @item(first -- new list)
    @item(second -- was any item replaced?)
    @item(third -- old value that was replaced (or nil if there was no such value))
   @end(list)"
  (iterate
    (with last-cell = nil)
    (with result = nil)
    (with replaced = nil)
    (with value = nil)
    (for sublist on list)
    (for elt = (car sublist))
    (if (funcall test (funcall list-key elt) (funcall item-key element))
        (progn
          (push element result)
          (setf replaced t
                value elt))
        (push elt result))
    (unless last-cell
      (setf last-cell result))
    (when (and replaced last-cell (not preserve-order))
      (setf (cdr last-cell)
            (cdr sublist))
      (finish))
    (finally (return (values (let ((r (if preserve-order
                                          (nreverse result)
                                          result)))
                               (if replaced
                                   r
                                   (cons element r)))
                             replaced
                             value)))))


(-> try-remove (t list &key (:test (-> (t t) boolean)) (:key (-> (t) t)) (:preserve-order boolean))
    (values list boolean t))
(defun try-remove (item list &key (test #'eql) (key #'identity) (preserve-order nil))
  "Try to remove first item matching from the list. Returns three values:
   @begin(list)
    @item(first -- new list)
    @item(second -- did anything was removed?)
    @item(third -- value that was removed (or nil if nothing was removed))
   @end(list)"
  (iterate
    (for sublist on list)
    (for elt = (car sublist))
    (with removed = nil)
    (with value = nil)
    (with last-cell = nil)
    (if (funcall test
                 (funcall key elt)
                 key item)
        (setf removed t
              value elt)
        (collect elt into result at start))
    (unless last-cell
      (setf last-cell result))
    (when (and removed last-cell (not preserve-order))
      (setf (cdr last-cell) (cdr sublist))
      (finish))
    (finally (return (values (if preserve-order
                                 (nreverse result)
                                 result)
                             removed
                             value)))))


(defun try-find-cell (item list &key (test #'eql) (key #'identity))
  "Returns first matching sublist"
  (iterate
    (for elt on list)
    (when (funcall test
                   (funcall key (car elt))
                   item)
      (return-from try-find-cell elt))))


(defun try-find (item list &key (test #'eql) (key #'identity))
  "Returns first matching elements as first value and boolean telling if it was found as second"
  (let ((r (try-find-cell item list :test test :key key)))
    (values (car r)
            (not (null r)))))

