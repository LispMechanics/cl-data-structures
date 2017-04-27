(in-package :cl-data-structures.dicts)


(deftype maybe-node ()
  `(or null hash-node bottom-node))


(deftype node-position ()
  `(values maybe-node fixnum))

#|

Macros

|#

(defmacro hash-do ((node index &optional (count (gensym)))
                   (root hash break-on-empty &optional (max-depth 10))
                   &body body)
  "Macro used for writing code going down into hash tree"
  (with-gensyms (!pos !block)
    (once-only (hash break-on-empty max-depth)
      `(the node-position
         (block ,!block
           (assert (<= ,max-depth 10))
           (do ((,!pos 6 (+ ,!pos 6))
                (,index (ldb (byte 6 0) ,hash)
                        (ldb (byte 6 ,!pos) ,hash))
                (,count 0 (1+ ,count))
                (,node ,root (and (hash-node-p ,node)
                                  (hash-node-contains ,node ,index)
                                  (hash-node-access ,node ,index))))
               ((= ,count ,max-depth)
                (values ,node
                        ,count))
             (declare (type fixnum ,hash ,!pos ,index ,count))
             (progn
               ,@body
               (when (or (and (null ,node) ,break-on-empty)
                         (typep ,node 'bottom-node))
                 (return-from ,!block
                   (values ,node
                           ,count))))))))))


(defmacro with-hash-tree-functions (container &body body)
  "Simple macro adding local functions (all forwards to the container closures) "
  `(fbind ((equal-fn (read-equal-fn ,container))
           (hash-fn (read-hash-fn ,container)))
     (declare (ignorable (function hash-fn)
                         (function equal-fn)))
     (flet ((remove-fn (item list)
              (funcall (read-remove-fn ,container)
                       item list
                       (read-equal-fn ,container)))
            (last-node-fn (item list)
              (funcall (read-last-node-fn ,container)
                       item list
                       (read-equal-fn ,container)))
            (insert-fn (item list)
              (funcall (read-insert-fn ,container)
                       item list
                       (read-equal-fn ,container))))
       (declare (ignorable (function remove-fn)
                           (function last-node-fn)
                           (function insert-fn)))
       ,@body)))


#|

Persitant hash table (like in Clojure)

|#


(deftype hash-node-index ()
  `(integer 0 63))


(deftype just-node ()
  `(or hash-node bottom-node))


(defstruct hash-node
  (mask 0 :type (unsigned-byte 64))
  (content #() :type simple-array))


(declaim (inline make-hash-node))


(defclass bottom-node () ()
  (:documentation "Base class of the last (conflict) node. Subclasses present to dispatch relevant logic."))


(defclass conflict-node (bottom-node)
  ((%conflict :initarg :conflict
              :accessor access-conflict
              :initform (list)
              :type list
              :documentation "List of elements with conflicting hash."))
  (:documentation "Conflict node simply holds list of elements that are conflicting."))


(defclass box-node (bottom-node)
  ((%content :initarg :content
             :reader read-content
             :documentation "Internal value of box"))
  (:documentation "Box node holds only one element inside."))


(defgeneric empty-nodep (bottom-node))


(defmethod empty-nodep ((node box-node))
  (slot-boundp node '%content))


(defmethod empty-nodep ((node conflict-node))
  (endp (access-conflict node)))


(define-constant +hash-level+ 6)


(-> hash-node-to-masked-index (hash-node (hash-node-index)) hash-node-index)
(defun hash-node-to-masked-index (hash-node index)
  (declare (optimize (speed 3) (debug 0) (safety 0) (compilation-speed 0) (space 0)))
  (~>> (hash-node-mask hash-node)
       (ldb (byte index 0))
       logcount))


(declaim (inline hash-node-to-masked-index))


(-> hash-node-contains (hash-node hash-node-index) boolean)
(defun hash-node-contains (hash-node index)
  (declare (optimize (speed 3) (debug 0) (safety 0) (compilation-speed 0) (space 0)))
  (~>> (hash-node-mask hash-node)
       (ldb (byte 1 index))
       zerop
       not))


(declaim (inline hash-node-contains))


(-> hash-node-access (hash-node hash-node-index) t)
(defun hash-node-access (hash-node index)
  (declare (optimize (speed 3) (debug 0) (safety 0) (compilation-speed 0) (space 0)))
  (handler-case
      (~>> (hash-node-to-masked-index hash-node index)
           (aref (hash-node-content hash-node)))))


(declaim (inline hash-node-access))


(-> hash-node-size (hash-node) (integer 0 64))
(defun hash-node-size (node)
  (logcount (hash-node-mask node)))


(-> hash-node-replace-in-the-copy (hash-node t hash-node-index) hash-node)
(defun hash-node-replace-in-the-copy (hash-node item index)
  (declare (optimize (speed 3) (debug 0) (safety 0) (compilation-speed 0) (space 0)))
  (let* ((content (copy-array (hash-node-content hash-node)))
         (mask (hash-node-mask hash-node)))
    (setf (aref content (logcount (ldb (byte index 0) mask)))
          item)
    (make-hash-node :mask mask
                    :content content)))


(declaim (inline hash-node-replace-in-the-copy))


(-> hash-node-insert-into-copy (hash-node t hash-node-index) hash-node)
(defun hash-node-insert-into-copy (hash-node item index)
  (let* ((current-array (hash-node-content hash-node))
         (new-array (make-array (1+ (array-dimension current-array 0))))
         (position (hash-node-to-masked-index hash-node index)))
    (assert (~> (array-dimension new-array 0)
                (<= 64)))
    ;;before new element
    (iterate

      (for i from 0 below position)
      (setf (aref new-array i)
            (aref current-array i)))

    ;;new element
    (setf (aref new-array position)
          item)

    ;;after new element
    (iterate
      (for i from position below (array-dimension current-array 0))
      (setf (aref new-array (1+ i))
            (aref current-array i)))

    ;;just make new hash-node
    (make-hash-node :mask (logior (hash-node-mask hash-node)
                                  (ash 1 index))
                    :content new-array)))


(defmacro descend-into-hash (root max-depth hash final-fn &optional break-on-absent)
  "Go into hamt, storing nodes into two stack allocated arrays. Pass those into supplied function. Macro so it is always inlined.
   @b(Arguments and values)
   @begin(list)
     @item(root -- node where we are starting descending)
     @item(max-depth -- how many nodes we are going to scan at most? @b(Must) be lower than 11).
     @item(break-on-absent -- control behavior when node is not present in the hamt. Either break and return, or go into filling indexes with positions where nodes should be until reaching maximal depth.)
   @end(list)"
  (once-only (max-depth)
    `(let ((path (make-array 10))
           (indexes (make-array ,max-depth :element-type 'fixnum))
           (depth 0))
       (declare (type fixnum depth)
                (dynamic-extent path indexes depth))
       (hash-do
           (node index i)
           (,root ,hash ,break-on-absent ,max-depth)
         (setf (aref path i) node
               (aref indexes i) index)
         (incf depth))
       (,final-fn path indexes depth)))) ;finally, pass data into function. Depth informs user about how many nodes was really scanned.


(defun non-empty-hash-table-p (table)
  (and (typep table 'hash-table)
       (not (zerop (hash-table-count table)))))


(deftype non-empty-hash-table ()
  `(satisfies non-empty-hash-table-p))


(defgeneric rehash (container conflict level)
  (:documentation "Attempts to divide conflct into smaller ones. Retudnerd hash table maps position of conflict to conflict itself and should contain at least one element"))


(defgeneric single-elementp (conflict)
  (:documentation "Checks if conflict node holds just a single element. Returns t if it does, returns nil if it does not."))


(-> rebuild-rehashed-node (fundamental-hash-tree-container fixnum fixnum bottom-node) just-node)
(-> build-rehashed-node (fundamental-hash-tree-container fixnum fixnum hash-table) just-node)
(defun build-rehashed-node (container depth max-depth content)
  (let ((array (make-array (hash-table-count content)))
        (mask 0))
    (iterate
     (for (index conflict) in-hashtable content)
     (setf mask (dpb 1 (byte 1 index) mask)))
    (iterate
     (for (index conflict) in-hashtable content)
     (setf (aref array (logcount (ldb (byte index 0) mask)))
           (if (or (<= depth max-depth) (single-elementp conflict))
               conflict
             (rebuild-rehashed-node container
                           depth
                           max-depth
                           conflict))))
    (make-hash-node :mask mask
                    :content array)))


(defun rebuild-rehashed-node (container depth max-depth conflict)
  (if (or (>= depth max-depth) (single-elementp conflict))
      conflict
      (let ((table (rehash container conflict (1- depth))))
        (build-rehashed-node container (1+ depth) max-depth table))))


(-> build-node (hash-node-index just-node) hash-node)
(defun build-node (index content)
  (make-hash-node :mask (ash 1 index)
                  :content (make-array 1 :initial-element content)))


(-> insert-into-hash (maybe-node fixnum t fundamental-hash-tree-container) hash-node)
(defun insert-into-hash (root hash item container)
  (declare (optimize (speed 3) (debug 0) (safety 0) (compilation-speed 0) (space 0)))
  ;; (declare (optimize debug))
  (with-hash-tree-functions container
    (flet ((cont (path indexes length) ;path and indexes have constant size BUT only part of it is used, that's why length is passed here
             (declare (type simple-array path indexes)
                      (type fixnum length))
             (iterate
               (for i from (- length 1) downto 0) ;reverse order (starting from deepest node)
               (for node = (aref path i))
               (for index = (aref indexes i))
               (for ac initially (let* ((last (aref path (1- length)))
                                        (bottom-node-or-nil (and (typep last 'bottom-node) last))
                                        (conflict (insert-fn item bottom-node-or-nil)))
                                   (if (or (null bottom-node-or-nil)
                                           (eql length (1- (read-max-depth container)))
                                           (single-elementp conflict))
                                       ;;if we didn't find element or element was found but depth was already maximal,
                                       ;;we will just return element, otherwise attempt to divide (rehash) conflicting node into few more
                                       conflict
                                       ;;rehash actually returns cl:hash-table, build-rehashed-node transforms it into another hash-node, depth is increased by 1 this way
                                       (rebuild-rehashed-node container
                                                              length
                                                              (read-max-depth container)
                                                              conflict)))
                    then (cond ((and (first-time-p) (null node)) ac) ;no node on path, just use our conflict node
                               ((null node) (build-node index ac)) ;no node, can happen in non shallow trees, in such case, let's just allocate another node.
                               ((typep node 'bottom-node) ac) ;corner case, added conflict or resolved conflict
                               ((hash-node-contains node index) (hash-node-replace-in-the-copy node ac index)) ;replace since it is already here
                               (t (hash-node-insert-into-copy node ac index)))) ;add since it was missing
               (finally (return ac)))))
      (declare (dynamic-extent (function cont))
               (inline cont))
      (descend-into-hash root
                         (read-max-depth container)
                         hash
                         cont
                         (read-shallow container)))))


(defun hash-node-insert! (node index content)
  (assert (zerop (ldb (byte 1 index) (hash-node-mask node))))
  (let* ((next-size (~> node
                        hash-node-content
                        (array-dimension 0)
                        1+))
        (next-mask (~>> node
                        hash-node-mask
                        (dpb 1 (byte 1 index))))
        (index (~>> next-mask
                    (ldb (byte index 0))
                    logcount)))
    (with-vectors ((n (make-array next-size)) (s (hash-node-content node)))
      (iterate
        (for i from 0 below next-size)
        (cond-compare (i index)
                      (setf (n i) (s i))
                      (setf (n i) content)
                      (setf (n i) (s (1- i)))))
      (setf (hash-node-mask node) next-mask
            (hash-node-content node) n)
      node)))


(defun hash-node-replace! (node index content)
  (assert (not (zerop (ldb (byte 1 index) (hash-node-mask node)))))
  (with-vectors ((a (hash-node-content node)))
    (setf (a (~>> node
                  hash-node-mask
                  (ldb (byte index 0))
                  logcount))
          content))
  node)


(-> insert-into-hash! (maybe-node fixnum t fundamental-hash-tree-container) hash-node)
(defun insert-into-hash! (root hash item container)
  (let ((prev-node nil)
        (prev-index 0))
    (with-hash-tree-functions container
      (hash-do (node index c) (root hash t)
        (symbol-macrolet ((just-node (unless (hash-node-contains node index)
                                       (return-from insert-into-hash!
                                         (progn
                                           (hash-node-insert! node
                                                              index
                                                              (insert-fn item nil))
                                           root)))))
          (cond+ (node prev-node (typep node 'bottom-node))
            ((t t t) (return-from insert-into-hash!
                       (progn
                         (hash-node-replace! prev-node
                                             prev-index
                                             (rebuild-rehashed-node container
                                                                    (1+ c)
                                                                    (read-max-depth container)
                                                                    (insert-fn item node)))
                         root)))
            ((t nil t) (return-from insert-into-hash!
                         (rebuild-rehashed-node container
                                                (1+ c)
                                                (read-max-depth container)
                                                (insert-fn item node))))
            ((nil nil nil) (return-from insert-into-hash!
                             (insert-fn item node)))
            ((t t nil) just-node)
            ((t nil nil) just-node)))
        (setf prev-node node
              prev-index index)))))


(-> hash-node-remove-from-the-copy (hash-node fixnum) maybe-node)
(defun hash-node-remove-from-the-copy (node index)
  "Returns copy of node, but without element under index. Not safe, does not check if element is actually present."
  (and (hash-node-contains node index)
       (let ((new-array (let ((size (hash-node-size node)))
                          (unless (eql 1 size)
                            (let ((result (make-array (1- size)))
                                  (position (1- (logcount (ldb (byte (1+ index) 0)
                                                               (hash-node-mask node)))))
                                  (input (hash-node-content node)))
                              (iterate
                                (for i from 0 below position)
                                (setf (aref result i) (aref input i)))
                              (iterate
                                (for i from position)
                                (for j from (1+ position) below size)
                                (setf (aref result i) (aref input j)))
                              result))))
             (new-mask (dpb 0 (byte 1 index) (hash-node-mask node))))
         (when new-array
           (make-hash-node :mask new-mask :content new-array)))))


(-> remove-from-hash (maybe-node fixnum t fundamental-hash-tree-container) (values maybe-node boolean))
(defun remove-from-hash (root hash item container)
  (declare (optimize (speed 3) (debug 0) (safety 0) (compilation-speed 0) (space 0))) ;very fast
  (with-hash-tree-functions container
    (flet ((cont (path indexes length)
             (declare (type simple-array path indexes)
                      (fixnum length))
             (if (not (zerop length))
                 (if-let ((last-node (aref path (1- length)))) ;if last-node = nil we didn't even found item in tree, so we will just return root
                   (if (typep last-node 'bottom-node) ;otherwise, item was not found
                       (let ((conf (remove-fn item last-node))) ;try removing
                         (if (eq conf last-node) ;nothing was removed
                             (values root nil) ;so just return root
                             (iterate
                               (for i from (- length 2) downto 0) ;without last node, because we already processed it
                               (for node = (aref path i))
                               (for index = (aref indexes i))
                               (for ac initially conf ;starting with new bottom node
                                    then (cond (ac (hash-node-replace-in-the-copy node ac index)) ;ac can be nil when we recursivly remove nodes (see line below)
                                               ((eql 1 (hash-node-size node)) ac) ;node does contain just one item but we remove that one item, so we remove whole node as well
                                               (t (hash-node-remove-from-the-copy node index)))) ;otherwise, just create copy of node, but without old item
                               (finally (return (values ac t))))))
                       (values root nil))
                   (values root nil))
                 (values root nil))))
      (declare (dynamic-extent (function cont))
               (inline cont))
      (multiple-value-bind (new-root removed)
          (descend-into-hash root
                             (read-max-depth container)
                             hash
                             cont
                             t)
        (values new-root removed)))))


(-> find-in-hash (hash-node fixnum t fundamental-hash-tree-container) (values t boolean))
(defun find-in-hash (root hash item container)
  "Obtain nodes in loop until node is missing or bottom-node was found
   @b(Arguments and values)
   @begin(list)
    @item(root -- root of scanned subtree)
    @item(hash -- fixnum containing hash of item)
    @item(container -- used for stored functions)"
  (declare (optimize (speed 3) (debug 0) (safety 0) (compilation-speed 0) (space 0)))
  (with-hash-tree-functions container
    (let ((node (hash-do (node index) (root hash t))))
      (cond
        ((typep node 'bottom-node) (values (last-node-fn item node) t))
        (t nil)))))


(-> try-remove (t list &key (:test (-> (t t) boolean)) (:key (-> (t) t))) list)
(defun try-remove (item list &key (test #'eql) (key #'identity))
  (iterate
    (for elt in list)
    (with removed = nil)
    (if (funcall test
                 (funcall key elt)
                 item)
        (setf removed t)
        (collect elt into result))
    (finally (return (values result removed)))))


(defclass fundamental-hash-tree-container ()
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


(defgeneric hash-find (container item)
  (:documentation "
 @b(Arguments and Values:)
 @begin(list)
  @item(container -- container in which we are seeking for any item)
  @item(item -- item that we are looking for)
 @end(list)
  @b(Description:)
  Try to find item in the container. Implementations of this generic function always return two values. First value returned is item found in the container (or nil if it could not be found).
  Second value is information if item was found in the container (t if yes, nil otherwise).
  @b(Side Effects:)
  None.
  @b(Notes:)
  Dictionary implementaions expect you to pass key as a item."))


(defgeneric hash-remove (container item)
  (:documentation "
 @b(Arguments and Values:)
 @begin(list)
  @item(container -- container from which we are removing item)
  @item(item -- item that will be removed from container)
 @end(list)

 @b(Description:)
 Removes from the container. Original container is not altered, new container is created, without item. If container does not contain item no error will be singaled.
 @b(Side Effects:)
 None.
 @b(Notes:)
 Dictionary implementations actually expect you to pass just a key, not whole pair as item.
"))


(defgeneric hash-insert (container item)
  (:documentation "
 @b(Arguments and Values:)
 @begin(list)
  @item(container -- container into which item is inserted)
  @item(item -- item that should be inserted into container)
 @end(list)
 @b(Description:)
 Inserts item into container. Original container is not altered. Type of item may be restriced by concrete implementation.
 @b(Side effects:)
 None.
 @b(Exceptional situations:)
 Some implementations (namely dictionary) place restrictions on item. Passing invalid item may signal error."))


(defgeneric hash-insert! (container item)
  (:documentation "
@b(Description:)
Destructive wariant of insert."))


(defclass functional-dictionary (fundamental-hash-tree-container)
  ())


(-> make-functional-dictionary ((-> (t) fixnum)
                                (-> (t t) boolean)
                                &key (:max-depth positive-fixnum) (:shallow boolean))
    functional-dictionary)
(defun make-functional-dictionary (hash-fn equal-fn &key (max-depth 8) (shallow t))
"
@b(Arguments and Values:)

@begin(list)
@item(hash-fn -- function that will be used to hash keys. Should return fixnum.)
@item(equal-fn -- function that will be used to resolve hash conflicts.)
@end(list)

@b(Description:)
Constructs and returns new functional-dictionary object.
"
  (make-instance 'functional-dictionary
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


(defmethod hash-find ((container functional-dictionary) item)
  (with-hash-tree-functions container
    (multiple-value-bind (r f)
        (find-in-hash (access-root container)
                      (hash-fn item)
                      item
                      container)
      (values (cdr r)
              f))))



(defmethod hash-insert-with-hash ((container fundamental-hash-tree-container) item hash)
  (declare (type fixnum hash))
  (insert-into-hash (access-root container)
                    hash
                    item
                    container))


(defmethod hash-remove ((container fundamental-hash-tree-container) item)
  (with-hash-tree-functions container
    (multiple-value-bind (new-root removed)
        (remove-from-hash (access-root container)
                          (hash-fn item)
                          item
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



(defmethod hash-insert ((container functional-dictionary) (item list))
  (with-hash-tree-functions container
    (let ((result (insert-into-hash (access-root container)
                                    (hash-fn (car item))
                                    item
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


(defmethod hash-insert! ((container functional-dictionary) (item list))
  (with-hash-tree-functions container
    (let ((result (insert-into-hash! (access-root container)
                                     (hash-fn (car item))
                                     item
                                     container)))
      (setf (access-root container) result)
      container)))


(-> map-hash-tree ((-> (bottom-node) t) hash-node) hash-node)
(defun map-hash-tree (fn root)
  (iterate
    (with stack = (make-array 32
                              :element-type 'maybe-node
                              :adjustable t
                              :fill-pointer 1
                              :initial-element root))
    (for current = (lastpop stack))
    (while current)
    (for (node . hash-path) = current)
    (etypecase node
      (bottom-node (funcall fn node))
      (hash-node (with-accessors ((mask hash-node-mask)
                                  (content hash-node-content)) node
                   (iterate
                     (for i from 0 below 64)
                     (with index = 0)
                     (unless (~> (ldb (byte 1 i) mask)
                                 zerop)
                       (vector-push-extend (aref content index)
                                           stack)
                       (incf index)))))
      (t (assert (null node)))))
  root)


(defgeneric hash-map (fn obj))


(defmethod hash-map (fn (obj functional-dictionary))
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


(-> contains-part-of-hash (fixnum fixnum (integer 0 64)) boolean)
(defun contains-part-of-hash (hash partial-hash depth)
  (~>> hash
      (logxor partial-hash)
      (ldb (byte depth 0))
      zerop))


(defmethod hash-of-bottom-node ((node conflict-node) container)
  (declare (type fundamental-hash-tree-container container))
  (with-hash-tree-functions container
    (~> node
        access-conflict
        caar
        hash-fn)))


(defmethod rehash ((container functional-dictionary) conflict level)
  (declare (type conflict-node conflict))
  (let ((result (make-hash-table))
        (byte (byte +hash-level+ (* +hash-level+ level))))
    (declare (dynamic-extent byte))
    (with-hash-tree-functions container
      (iterate
        (for key.value in (access-conflict conflict))
        (for (key . value) = key.value)
        (for hash = (hash-fn key))
        (for index = (ldb byte hash))
        (push key.value (access-conflict (ensure (gethash index result)
                                           (make-instance 'conflict-node))))))
    result))


(defmethod single-elementp ((conflict conflict-node))
  (endp (cdr (access-conflict conflict))))

