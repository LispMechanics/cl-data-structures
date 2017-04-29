(in-package :cl-ds.dicts.hamt)


(defclass functional-hamt-dictionary (hamt-dictionary
                                      cl-ds:functional)
  ())


(defclass mutable-hamt-dictionary (hamt-dictionary
                                   cl-ds:mutable)
  ())


(-> make-functional-hamt-dictionary ((-> (t) fixnum)
                                     (-> (t t) boolean)
                                     &key (:max-depth positive-fixnum))
    functional-hamt-dictionary)
(defun make-functional-hamt-dictionary (hash-fn equal-fn &key (max-depth 8))
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
(defun functional-hamt-dictionary-erase (container location)
  (let ((old-value nil))
    (with-hash-tree-functions container
      (multiple-value-bind (new-root removed)
          (modify-copy-hamt (access-root container)
                            (hash-fn location)
                            container
                            (lambda (bottom)
                              (multiple-value-bind (list removed)
                                  (try-remove location
                                              (and bottom (access-conflict bottom))
                                              :test (lambda (ex r)
                                                      (when-let ((result (equal-fn (car ex) r)))
                                                        (setf old-value (cdr ex))
                                                        result)))
                                (values (if removed
                                            (and list (make-conflict-node list))
                                            bottom)
                                        removed))))
        (values (if removed (make-instance (type-of container)
                                           :equal-fn (read-equal-fn container)
                                           :hash-fn (read-hash-fn container)
                                           :root new-root
                                           :remove-fn (read-remove-fn container)
                                           :last-node-fn (read-last-node-fn container)
                                           :insert-fn (read-insert-fn container)
                                           :equal-fn (read-equal-fn container)
                                           :max-depth (read-max-depth container))
                    container)
                removed
                old-value)))))


(defmethod cl-ds:erase ((container functional-hamt-dictionary) location)
  (functional-hamt-dictionary-erase container location))


(defun functional-hamt-dictionary-insert (container location new-value)
  (let ((old nil)
        (rep nil))
    (with-hash-tree-functions container
      (let ((result (modify-copy-hamt (access-root container)
                                      (hash-fn location)
                                      container
                                      (lambda (bottom)
                                        (multiple-value-bind (next-list replaced old-value)
                                            (insert-or-replace (and bottom (access-conflict bottom))
                                                               (list* location new-value)
                                                               :test (read-equal-fn container)
                                                               :key #'car)
                                          (setf old (cdr old-value)
                                                rep replaced)
                                          (values (make-conflict-node next-list)
                                                  t))))))
        (values (make-instance (type-of container)
                               :equal-fn (read-equal-fn container)
                               :hash-fn (read-hash-fn container)
                               :root result
                               :remove-fn (read-remove-fn container)
                               :last-node-fn (read-last-node-fn container)
                               :insert-fn (read-insert-fn container)
                               :equal-fn (read-equal-fn container)
                               :max-depth (read-max-depth container))
                rep
                old)))))


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


(defun functional-hamt-dictionary-update (container location new-value)
  ())


(defmethod cl-ds:update ((container functional-hamt-dictionary) location new-value)
  (functional-hamt-dictionary-update container location new-value))
