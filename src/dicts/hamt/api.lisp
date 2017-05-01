(in-package :cl-ds.dicts.hamt)


(defclass functional-hamt-dictionary (hamt-dictionary
                                      cl-ds:functional)
  ())


(defclass mutable-hamt-dictionary (hamt-dictionary
                                   cl-ds:mutable)
  ())


(-> make-functional-hamt-dictionary ((-> (t) fixnum)
                                     (-> (t t) boolean)
                                     &key (:max-depth (integer 1 11)))
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
  (assert (<= max-depth 10))
  (assert (> max-depth 0))
  (assure functional-hamt-dictionary (make-instance 'functional-hamt-dictionary
                                                    :hash-fn hash-fn
                                                    :root nil
                                                    :max-depth max-depth
                                                    :equal-fn equal-fn)))


(defun make-mutable-hamt-dictionary (hash-fn equal-fn &key (max-depth 8))
  (assure mutable-hamt-dictionary (make-instance 'mutable-hamt-dictionary
                                                 :equal-fn equal-fn
                                                 :hash-fn hash-fn
                                                 :root nil
                                                 :max-depth max-depth)))


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


(-> hamt-dictionary-at (hamt-dictionary t) (values t boolean))
(defun hamt-dictionary-at (container location)
  (with-hash-tree-functions container
    (multiple-value-bind (r f)
        (let* ((hash (hash-fn location))
               (root (access-root container))
               (node (hash-do (node index) (root hash))))
          (if (typep node 'bottom-node)
              (try-find location
                        (access-conflict node)
                        :test (read-equal-fn container)
                        :key #'car)
            (values nil nil)))
      (values (cdr r)
              f))))


(defmethod cl-ds:at ((container hamt-dictionary) location)
  (hamt-dictionary-at container location))


(-> functional-hamt-dictionary-erase (functional-hamt-dictionary t) (values functional-hamt-dictionary boolean))
(defun functional-hamt-dictionary-erase (container location)
  (let ((old-value nil))
    (with-hash-tree-functions container
      (multiple-value-bind (new-root removed)
          (modify-copy-hamt (access-root container)
                            (hash-fn location)
                            container
                            (lambda (bottom)
                              (multiple-value-bind (list removed value)
                                  (try-remove location
                                              (and bottom (access-conflict bottom))
                                              :test (read-equal-fn container)
                                              :key #'car)
                                (setf old-value (cdr value))
                                (values (if removed
                                            (and list (make-conflict-node list))
                                            bottom)
                                        removed))))
        (values (if removed (make-instance (type-of container)
                                           :hash-fn (read-hash-fn container)
                                           :root new-root
                                           :equal-fn (read-equal-fn container)
                                           :max-depth (read-max-depth container)
                                           :size (1- (access-size container)))
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
                                                               :list-key #'car
                                                               :item-key #'car)
                                          (setf old (cdr old-value)
                                                rep replaced)
                                          (values (make-conflict-node next-list)
                                                  t))))))
        (values (make-instance (type-of container)
                               :equal-fn (read-equal-fn container)
                               :hash-fn (read-hash-fn container)
                               :root result
                               :max-depth (read-max-depth container))
                rep
                old)))))


(defmethod cl-ds:insert ((container functional-hamt-dictionary) location new-value)
  (functional-hamt-dictionary-insert container location new-value))


(defun mutable-hamt-dictionary-insert! (container location new-value)
  (let ((replaced nil)
        (old-value nil))
    (flet ((destructive-insert (node)
             (multiple-value-bind (next-list r v)
                 (insert-or-replace (access-conflict node)
                                    (list* location new-value)
                                    :test (read-equal-fn container)
                                    :list-key #'car
                                    :item-key #'car)
               (setf (access-conflict node) next-list
                     replaced r
                     old-value (cdr v))
               node)))
      (with-hash-tree-functions container
        (let* ((prev-node nil)
               (prev-index 0)
               (hash (hash-fn location))
               (root (access-root container))
               (result (block result (with-hash-tree-functions container
                                       (hash-do (node index c) (root hash)
                                         (symbol-macrolet ((just-node (return-from result
                                                                        (progn
                                                                          (hash-node-insert! node
                                                                                             index
                                                                                             (make-conflict-node (list (list* location new-value))))
                                                                          root))))
                                           (cond+ (node prev-node (typep node 'bottom-node))
                                             ((t t t) (return-from result
                                                        (progn
                                                          (hash-node-replace! prev-node
                                                                              prev-index
                                                                              (rebuild-rehashed-node container
                                                                                                     (1+ c)
                                                                                                     (read-max-depth container)
                                                                                                     (destructive-insert node)))
                                                          root)))
                                             ((t nil t)  (return-from result
                                                           (rebuild-rehashed-node container
                                                                                  (1+ c)
                                                                                  (read-max-depth container)
                                                                                  (destructive-insert node))))
                                             ((nil nil nil) (return-from result
                                                              (make-conflict-node (list (list* location new-value)))))
                                             ((nil t nil) (return-from result
                                                            (progn (hash-node-insert! prev-node
                                                                                      prev-index
                                                                                      (rebuild-rehashed-node container
                                                                                                             (1+ c)
                                                                                                             (read-max-depth container)
                                                                                                             (make-conflict-node (list (list* location new-value)))))
                                                                   root)))
                                             ((t t nil) nil)
                                             ((t nil nil) nil)))
                                         (setf prev-node node
                                               prev-index index))))))
          (setf (access-root container) result)
          (unless replaced
            (incf (access-size container)))
          (values container
                  replaced
                  old-value))))))


(defmethod (setf cl-ds:at) (new-value (container mutable-hamt-dictionary) location)
  (mutable-hamt-dictionary-insert! container location new-value))


(defun functional-hamt-dictionary-update (container location new-value)
  (let ((old nil)
        (up nil))
    (with-hash-tree-functions container
      (let ((result (modify-copy-hamt (access-root container)
                                      (hash-fn location)
                                      container
                                      (lambda (bottom)
                                        (multiple-value-bind (next-list replaced old-value)
                                            (insert-or-replace (and bottom (access-conflict bottom))
                                                               (list* location new-value)
                                                               :test (read-equal-fn container)
                                                               :list-key #'car
                                                               :item-key #'car)
                                          (setf old (cdr old-value)
                                                up replaced)
                                          (if replaced
                                              (values (make-conflict-node next-list)
                                                      t)
                                              (values bottom
                                                      nil)))))))
        (values (if up
                    (make-instance (type-of container)
                                   :equal-fn (read-equal-fn container)
                                   :hash-fn (read-hash-fn container)
                                   :root result
                                   :max-depth (read-max-depth container)
                                   :size (access-size container))
                    container)
                up
                old)))))


(defun functional-hamt-dictionary-add (container location new-value)
  (let ((add nil))
    (with-hash-tree-functions container
      (let ((result (modify-copy-hamt (access-root container)
                                      (hash-fn location)
                                      container
                                      (lambda (bottom)
                                        (let* ((list (and bottom (access-conflict bottom)))
                                               (item (find location list
                                                           :key #'car
                                                           :test (read-equal-fn container))))
                                          (setf add (not item))
                                          (if item
                                              (values bottom nil)
                                              (values (make-conflict-node (cons (list* location new-value)
                                                                                list))
                                                      t)))))))
        (values (if add
                    (make-instance (type-of container)
                                   :equal-fn (read-equal-fn container)
                                   :hash-fn (read-hash-fn container)
                                   :root result
                                   :max-depth (read-max-depth container)
                                   :size (access-size container))
                    container)
                add)))))


(defmethod cl-ds:update ((container functional-hamt-dictionary) location new-value)
  (functional-hamt-dictionary-update container location new-value))


(defmethod cl-ds:add ((container functional-hamt-dictionary) location new-value)
  (functional-hamt-dictionary-add container location new-value))


(defun mutable-hamt-dictionary-update! (container location new-value)
  (with-hash-tree-functions container
    (multiple-value-bind (r f)
        (let* ((hash (hash-fn location))
               (root (access-root container))
               (node (hash-do (node index) (root hash))))
          (if (typep node 'bottom-node)
              (try-find location
                        (access-conflict node)
                        :test (read-equal-fn container)
                        :key #'car)
              (values nil nil)))
      (let ((old-value (cdr r)))
        (when f
          (setf (cdr r) new-value))
        (values container f old-value)))))


(defmethod cl-ds:update! ((container mutable-hamt-dictionary) location new-value)
  (mutable-hamt-dictionary-update! container location new-value))


(defmethod cl-ds:size ((container hamt-dictionary))
  (access-size container))
