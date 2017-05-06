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
  "@b(Arguments and Values:)
   @begin(list)
   @item(hash-fn -- function that will be used to hash keys. Should return fixnum and follow all rules of hashing.)
   @item(equal-fn -- function that will be used to resolve hash conflicts.)
   @item(max-depth -- how many levels this hamt can have at most?)
   @end(list)

   @b(Description:)
   Constructs and returns new functional-hamt-dictionary object.

   @b(Notes:)
   In theory, HAMT can use infinite length of HASH, but this implementation uses 60 oldest bits at most."
  (assert (<= max-depth 10))
  (assert (> max-depth 0))
  (assure functional-hamt-dictionary (make-instance 'functional-hamt-dictionary
                                                    :hash-fn hash-fn
                                                    :root nil
                                                    :max-depth max-depth
                                                    :equal-fn equal-fn)))


(-> make-mutable-hamt-dictionary ((-> (t) fixnum)
                                  (-> (t t) boolean)
                                  &key (:max-depth (integer 1 11)))
    mutable-hamt-dictionary)
(defun make-mutable-hamt-dictionary (hash-fn equal-fn &key (max-depth 8))
  "@b(Arguments and Values:)
   @begin(list)
   @item(hash-fn -- function that will be used to hash keys. Should return fixnum and follow all rules of hashing.)
   @item(equal-fn -- function that will be used to resolve hash conflicts.)
   @item(max-depth -- how many levels this hamt can have at most?)
   @end(list)

   @b(Description:)
   Constructs and returns new mutable-hamt-dictionary object.

   @b(Notes:)
   In theory, HAMT can use infinite length of HASH, but this implementation uses 60 oldest bits at most."
  (assert (<= max-depth 10))
  (assert (> max-depth 0))
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
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  "Implementation of AT"
  (with-hash-tree-functions container
    (let* ((hash (hash-fn location))
           (root (access-root container)))
      (hash-do
          (node index)
          (root hash)
        :on-leaf (multiple-value-bind (r f) (try-find location
                                                      (access-conflict (the conflict-node node))
                                                      :test (read-equal-fn container)
                                                      :key #'car)
                   (values (cdr r) f))
        :on-nil (values nil nil)))))


(-> functional-hamt-dictionary-erase (functional-hamt-dictionary t)
    (values functional-hamt-dictionary boolean))
(defun functional-hamt-dictionary-erase (container location)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  "Implementation of ERASE"
  (let* ((old-value nil)
         (result
           (with-hash-tree-functions container
             (with-copy-on-write-hamt node container (hash-fn location)
               :on-leaf (multiple-value-bind (list removed value)
                            (try-remove location (access-conflict node)
                                        :test (read-equal-fn container)
                                        :key #'car)
                          (unless removed
                            (return-from functional-hamt-dictionary-erase (values container nil nil)))
                          (setf old-value (cdr value))
                          (and list (make-conflict-node list)))
               :on-nil (return-from functional-hamt-dictionary-erase (values container nil nil))))))
    (values (make-instance (type-of container)
                           :hash-fn (read-hash-fn container)
                           :root result
                           :equal-fn (read-equal-fn container)
                           :max-depth (read-max-depth container)
                           :size (1- (access-size container)))
            t
            old-value)))


(-> functional-hamt-dictionary-insert (functional-hamt-dictionary t t)
    (values functional-hamt-dictionary boolean t))
(defun functional-hamt-dictionary-insert (container location new-value)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  "Implementation of INSERT"
  (let* ((old nil)
         (rep nil)
         (result
           (with-hash-tree-functions container
               (with-copy-on-write-hamt node container (hash-fn location)
                 :on-leaf (multiple-value-bind (next-list replaced old-value)
                              (insert-or-replace (access-conflict (the conflict-node node))
                                                 (list* location new-value)
                                                 :test (read-equal-fn container)
                                                 :list-key #'car
                                                 :item-key #'car)
                            (setf old (cdr old-value)
                                  rep replaced)
                            (values (make-conflict-node next-list)))
                 :on-nil (make-conflict-node (list (list* location new-value)))))))
    (values (make-instance (type-of container)
                           :equal-fn (read-equal-fn container)
                           :hash-fn (read-hash-fn container)
                           :root result
                           :max-depth (read-max-depth container)
                           :size (if rep
                                     (access-size container)
                                     (1+ (access-size container))))
            rep
            old)))


(-> functional-hamt-dictionary-insert! (functional-hamt-dictionary t t)
    (values functional-hamt-dictionary boolean t))
(defun mutable-hamt-dictionary-insert! (container location new-value)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  "Implementation of (SETF AT)"
  (let ((replaced nil)
        (old-value nil))
    (flet ((destructive-insert (node)
             (multiple-value-bind (next-list r v)
                 (insert-or-replace (access-conflict (the conflict-node node))
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
               (root (access-root container))
               (result
                 (hash-do (node index c) ((access-root container) (hash-fn location))
                          :on-every (setf prev-node node prev-index index)
                          :on-nil (if prev-node
                                      (progn
                                        (assert (not (hash-node-contains-leaf prev-node prev-index)))
                                        (hash-node-insert! prev-node
                                                                prev-index
                                                                (rebuild-rehashed-node container
                                                                                       c
                                                                                       (read-max-depth container)
                                                                                       (make-conflict-node (list (list* location new-value)))))
                                             root)
                                      (make-conflict-node (list (list* location new-value))))
                          :on-leaf (if prev-node
                                       (progn
                                         (assert (hash-node-contains-leaf prev-node prev-index))
                                         (hash-node-replace! prev-node
                                                             prev-index
                                                             (rebuild-rehashed-node container
                                                                                    c
                                                                                    (read-max-depth container)
                                                                                    (destructive-insert node)))
                                              root)
                                       (rebuild-rehashed-node container
                                                              c
                                                              (read-max-depth container)
                                                              (destructive-insert node))))))
          (setf (access-root container) result)
          (unless replaced
            (incf (access-size container)))
          (values container
                  replaced
                  old-value))))))


(-> functional-hamt-dictionary-update (functional-hamt-dictionary t t)
    (values functional-hamt-dictionary boolean t))
(defun functional-hamt-dictionary-update (container location new-value)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  "Implementation of UPDATE"
  (let* ((old nil)
        (result
          (with-hash-tree-functions container
            (with-copy-on-write-hamt node container (hash-fn location)
              :on-leaf (multiple-value-bind (next-list replaced old-value)
                           (insert-or-replace (the list (access-conflict (the conflict-node node)))
                                              (list* location new-value)
                                              :test (read-equal-fn container)
                                              :list-key #'car
                                              :item-key #'car)
                         (unless replaced
                           (return-from functional-hamt-dictionary-update (values container nil nil)))
                         (setf old (cdr old-value))
                         (make-conflict-node next-list))
              :on-nil (return-from functional-hamt-dictionary-update (values container nil nil))))))
    (values (make-instance (type-of container)
                           :equal-fn (read-equal-fn container)
                           :hash-fn (read-hash-fn container)
                           :root result
                           :max-depth (read-max-depth container)
                           :size (access-size container))
            t
            old)))

(-> functional-hamt-dictionary-add (functional-hamt-dictionary t t)
    (values functional-hamt-dictionary boolean t))
(defun functional-hamt-dictionary-add (container location new-value)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  "Implementation of ADD"
  (let* ((existing-value nil)
         (result
           (with-hash-tree-functions container
             (with-copy-on-write-hamt node container (hash-fn location)
               :on-leaf (let* ((list (access-conflict node))
                               (item (find location (the list list)
                                           :key #'car
                                           :test (read-equal-fn container))))
                          (when item
                            (return-from functional-hamt-dictionary-add (values container nil (cdr item))))
                          (setf existing-value (cdr item))
                          (make-conflict-node (cons (list* location new-value)
                                                    list)))
               :on-nil (make-conflict-node (list (list* location new-value)))))))
    (values (make-instance (type-of container)
                           :equal-fn (read-equal-fn container)
                           :hash-fn (read-hash-fn container)
                           :root result
                           :max-depth (read-max-depth container)
                           :size (1+ (access-size container)))
            t
            existing-value)))


(-> mutable-hamt-dictionary-update! (functional-hamt-dictionary t t)
    (values functional-hamt-dictionary boolean t))
(defun mutable-hamt-dictionary-update! (container location new-value)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  "Implementation of UPDATE!"
  (with-hash-tree-functions container
    (hash-do
        (node index)
        ((access-root container) (hash-fn location))
        :on-leaf (if-let ((r (find location
                                (access-conflict (the conflict-node node))
                                :test (read-equal-fn container)
                                :key #'car)))
                   (let ((old (cdr r)))
                     (setf (cdr r) new-value)
                     (values container t old))
                   (values container nil nil))
        :on-nil (values container nil nil))))


(-> mutable-hamt-dictionary-add! (functional-hamt-dictionary t t)
    (values functional-hamt-dictionary boolean t))
(defun mutable-hamt-dictionary-add! (container location new-value)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  "Implementation of add!"
  (flet ((destructive-insert (node)
           (multiple-value-bind (next-list r v)
               (insert-or-replace (access-conflict (the conflict-node node))
                                  (list* location new-value)
                                  :test (read-equal-fn container)
                                  :list-key #'car
                                  :item-key #'car)
             (when r
               (return-from mutable-hamt-dictionary-add! (values container nil (cdr v))))
             (setf (access-conflict node) next-list)
             node)))
    (with-hash-tree-functions container
      (let* ((prev-node nil)
             (prev-index 0)
             (root (access-root container))
             (result
               (hash-do (node index c) ((access-root container) (hash-fn location))
                        :on-every (setf prev-node node prev-index index)
                        :on-nil (if prev-node
                                    (progn
                                      (assert (not (hash-node-contains-leaf prev-node prev-index)))
                                      (hash-node-insert! prev-node
                                                         prev-index
                                                         (rebuild-rehashed-node container
                                                                                c
                                                                                (read-max-depth container)
                                                                                (make-conflict-node (list (list* location new-value)))))
                                      root)
                                    (make-conflict-node (list (list* location new-value))))
                        :on-leaf (if prev-node
                                     (progn
                                       (assert (hash-node-contains-leaf prev-node prev-index))
                                       (hash-node-replace! prev-node
                                                           prev-index
                                                           (rebuild-rehashed-node container
                                                                                  c
                                                                                  (read-max-depth container)
                                                                                  (destructive-insert node)))
                                       root)
                                     (rebuild-rehashed-node container
                                                            c
                                                            (read-max-depth container)
                                                            (destructive-insert node))))))
        (setf (access-root container) result)
        (incf (access-size container))
        (values container
                t
                nil)))))


(-> hamt-dictionary-size (hamt-dictionary) positive-fixnum)
(defun hamt-dictionary-size (container)
  "Implementation of SIZE"
  (access-size container))


#|

Methods. Those will just call non generic functions.

|#


(defmethod cl-ds:update! ((container mutable-hamt-dictionary) location new-value)
  (mutable-hamt-dictionary-update! container location new-value))


(defmethod cl-ds:add! ((container mutable-hamt-dictionary) location new-value)
  (mutable-hamt-dictionary-add! container location new-value))


(defmethod cl-ds:size ((container hamt-dictionary))
  (hamt-dictionary-size container))


(defmethod cl-ds:at ((container hamt-dictionary) location)
  (hamt-dictionary-at container location))


(defmethod cl-ds:update ((container functional-hamt-dictionary) location new-value)
  (functional-hamt-dictionary-update container location new-value))


(defmethod cl-ds:add ((container functional-hamt-dictionary) location new-value)
  (functional-hamt-dictionary-add container location new-value))


(defmethod (setf cl-ds:at) (new-value (container mutable-hamt-dictionary) location)
  (mutable-hamt-dictionary-insert! container location new-value))


(defmethod cl-ds:insert ((container functional-hamt-dictionary) location new-value)
  (functional-hamt-dictionary-insert container location new-value))


(defmethod cl-ds:erase ((container functional-hamt-dictionary) location)
  (functional-hamt-dictionary-erase container location))

