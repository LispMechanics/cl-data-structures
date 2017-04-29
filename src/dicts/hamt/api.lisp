(in-package :cl-ds.dicts.hamt)


(defclass fundamental-hamt-container (cl-ds:fundamental-container)
  ((%root :type (or hash-node bottom-node null)
          :accessor access-root
          :initarg :root
          :documentation "Hash node pointing to root of the whole hash tree.")
   (%hash-fn :type (-> (x) fixnum)
             :reader read-hash-fn
             :initarg :hash-fn
             :documentation "Closure used for key hashing. Setted by the user.")
   (%remove-fn :type (-> (item bottom-node (-> t t) boolean) (values bottom-node boolean))
               :reader read-remove-fn
               :initarg :remove-fn
               :documentation "Closure used for removing items from bottom level lists. @b(Not) exposed in any way to user.")
   (%last-node-fn :type (-> (item t equal) t)
                  :reader read-last-node-fn
                  :initarg :last-node-fn
                  :documentation "Closure used for finding items in the bottom node")
   (%insert-fn :type (-> (item t (-> (t t) boolean)) list)
               :reader read-insert-fn
               :initarg :insert-fn
               :documentation "Closure used for adding new item into bottom level lists. @b(Not) exposed in any way to user.")
   (%equal-fn :type (-> (t t) boolean)
              :reader read-equal-fn
              :initarg :equal-fn
              :documentation "Closure used for comparing items at the bottom level lists.")
   (%max-depth :initarg :max-depth
               :type (integer 0 10)
               :reader read-max-depth
               :documentation "Maximal depth of tree.")
   (%shallow :initarg :shallow
             :type boolean
             :reader read-shallow
             :initform t
             :documentation "If set to nil, depth will be always maximal"))
  (:documentation "Base class of other containers. Acts as any container for bunch of closures (those vary depending on the concrete container) and root of the tree."))


(defclass hamt-dictionary (fundamental-hamt-container
                           cl-ds.dicts:dictionary)
  ())


(defclass functional-hamt-dictionary (hamt-dictionary
                                      cl-ds:functional)
  ())


(defclass mutable-hamt-dictionary (hamt-dictionary
                                   cl-ds:mutable)
  ())


(-> make-functional-hamt-dictionary ((-> (t) fixnum)
                                     (-> (t t) boolean)
                                     &key (:max-depth positive-fixnum) (:shallow boolean))
    functional-hamt-dictionary)
(defun make-functional-hamt-dictionary (hash-fn equal-fn &key (max-depth 8) (shallow t))
"
@b(Arguments and Values:)

@begin(list)
@item(hash-fn -- function that will be used to hash keys. Should return fixnum.)
@item(equal-fn -- function that will be used to resolve hash conflicts.)
@end(list)

@b(Description:)
Constructs and returns new functional-hamt-dictionary object.
"
  (make-instance 'functional-hamt-dictionary
                 :equal-fn equal-fn
                 :hash-fn hash-fn
                 :root nil
                 :max-depth max-depth
                 :shallow shallow
                 :remove-fn (lambda (item node equal) (declare (type conflict-node node)
                                                               (type (-> (t t) boolean)))
                              (multiple-value-bind (result removed)
                                  (try-remove item
                                              (access-conflict node)
                                              :test equal
                                              :key #'car)
                                (values (cond ((null result) nil)
                                              (removed (make-instance 'conflict-node
                                                                      :conflict result))
                                              (t node))
                                        removed)))
                 :last-node-fn (lambda (item node equal)
                                 (find item (access-conflict node) :key #'car :test equal))
                 :insert-fn (lambda (item node equal)
                              (declare (ignore equal))
                              (make-instance 'conflict-node
                                             :conflict (cons item (when node (access-conflict node)))))
                 :equal-fn equal-fn))


#|
(-> make-functional-key-tree-container ((-> (t) fixnum)
                                        positive-fixnum
                                        &key (:max-depth positive-fixnum) (:shallow boolean))
    functional-key-tree-container)
(let ((empty-box (make-instance 'box-node))) ;default node, avoid allocating empty nodes without reason
  (defun make-functional-key-tree-container (hash-fn &key (max-depth 8) (shallow t))
    (make-instance 'functional-key-tree-container
                   :equal-fn #'eq
                   :hash-fn hash-fn
                   :max-depth max-depth
                   :root (make-instance 'hash-node)
                   :shallow shallow
                   :remove-fn (lambda (item node equal) (declare (ignore node equal item))
                                (values empty-box t))
                   :last-node-fn (lambda (item node equal)
                                   (declare (ignore item equal))
                                   (read-content node))
                   :insert-fn (lambda (item node equal) (declare (ignore node equal))
                                (make-instance 'box-node
                                               :content (cadr item))))))
|#


(-> functional-hamt-dictionary-at (functional-hamt-dictionary t) (values t boolean))
(defun functional-hamt-dictionary-at (container location)
  (with-hash-tree-functions container
    (multiple-value-bind (r f)
        (find-in-hash (access-root container)
                      (hash-fn location)
                      location
                      container)
      (values (cdr r)
              f))))


(defmethod cl-ds:at ((container functional-hamt-dictionary) location)
  (functional-hamt-dictionary-at container location))


(-> functional-hamt-dictionary-erase (functional-hamt-dictionary t) (values functional-hamt-dictionary boolean))
(defun functional-hamt-dictionary-erase (dict location)
  (with-hash-tree-functions container
    (multiple-value-bind (new-root removed)
        (remove-from-hash (access-root container)
                          (hash-fn location)
                          location
                          container)
      (values (make-instance (type-of container)
                             :equal-fn (read-equal-fn container)
                             :hash-fn (read-hash-fn container)
                             :root new-root
                             :remove-fn (read-remove-fn container)
                             :last-node-fn (read-last-node-fn container)
                             :insert-fn (read-insert-fn container)
                             :equal-fn (read-equal-fn container)
                             :max-depth (read-max-depth container)
                             :shallow (read-shallow container))
              removed))))


(defmethod cl-ds:erase ((container functional-hamt-dictionary) location)
  (functional-hamt-dictionary-erase container location))


(defun functional-hamt-dictionary-insert (container location new-value)
  (with-hash-tree-functions container
    (let ((result (insert-into-hash (access-root container)
                                    (hash-fn location)
                                    (list location new-value)
                                    container)))
      (make-instance (type-of container)
                     :equal-fn (read-equal-fn container)
                     :hash-fn (read-hash-fn container)
                     :root result
                     :remove-fn (read-remove-fn container)
                     :last-node-fn (read-last-node-fn container)
                     :insert-fn (read-insert-fn container)
                     :equal-fn (read-equal-fn container)
                     :max-depth (read-max-depth container)
                     :shallow (read-shallow container)))))


(defmethod cl-ds:insert ((container functional-hamt-dictionary) location new-value)
  (functional-hamt-dictionary-insert container location new-value))


(defun mutable-hamt-dictionary-insert! (container location new-value)
  (with-hash-tree-functions container
    (let ((result (insert-into-hash! (access-root container)
                                     (hash-fn location)
                                     (list location new-value)
                                     container)))
      (setf (access-root container) result)
      container)))


(defmethod (setf cl-ds:at) (new-value (container mutable-hamt-dictionary) location)
  (mutable-hamt-dictionary-insert! container location new-value))


(defgeneric hash-map (fn obj))


(defmethod hash-map (fn (obj functional-hamt-dictionary))
  (map-hash-tree (lambda (content)
                   (declare (type conflict-node content))
                   (with-accessors ((conflict access-conflict)) content
                     (map nil
                          (lambda (key.value)
                            (destructuring-bind (key . value) key.value
                              (funcall fn key value)))
                          conflict)))
                 (access-root obj))
  obj)



