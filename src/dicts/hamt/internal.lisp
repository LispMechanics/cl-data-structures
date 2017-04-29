(in-package :cl-ds.dicts.hamt)

#|

Basic types

|#

(deftype maybe-node ()
  `(or null hash-node bottom-node))


(deftype node-position ()
  `(values maybe-node fixnum))


(deftype hash-node-index ()
  `(integer 0 63))


(deftype just-node ()
  `(or hash-node bottom-node))

#|

Macros

|#

(defmacro hash-do ((node index &optional (count (gensym)))
                   (root hash &optional (max-depth 10))
                   &body body)
  "Macro used for writing code going down into hash tree"
  (with-gensyms (!pos !block)
    (once-only (hash max-depth)
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
               (when (or (null ,node) 
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


(defmacro descend-into-hash (root max-depth hash final-fn)
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
           (,root ,hash ,max-depth)
         (setf (aref path i) node
               (aref indexes i) index)
         (incf depth))
       (,final-fn path indexes depth)))) ;finally, pass data into function. Depth informs user about how many nodes was really scanned.


#|

Tree structure of HAMT

|#


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


(-> make-conflict-node (list) conflict-node)
(defun make-conflict-node (content)
  (assure conflict-node (make-instance 'conflict-node :conflict content)))


(defclass box-node (bottom-node)
  ((%content :initarg :content
             :reader read-content
             :documentation "Internal value of box"))
  (:documentation "Box node holds only one element inside."))


(defgeneric empty-node-p (bottom-node))


(defgeneric contains-p (bottom-node item fn))


(defmethod contains-p ((node conflict-node) item fn)
  (find item (access-conflict node) :test fn))


(defmethod empty-node-p ((node box-node))
  (slot-boundp node '%content))


(defmethod empty-node-p ((node conflict-node))
  (endp (access-conflict node)))


(define-constant +hash-level+ 6)

#|

Interface class.

|#

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
               :documentation "Maximal depth of tree."))
  (:documentation "Base class of other containers. Acts as any container for bunch of closures (those vary depending on the concrete container) and root of the tree."))


(defclass hamt-dictionary (fundamental-hamt-container
                           cl-ds.dicts:dictionary)
  ())


#|

Functions with basic bit logic.

|#

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

#|

Copy nodes and stuff.

|#

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


(defun non-empty-hash-table-p (table)
  (and (typep table 'hash-table)
       (not (zerop (hash-table-count table)))))


(deftype non-empty-hash-table ()
  `(satisfies non-empty-hash-table-p))


(defgeneric rehash (container conflict level)
  (:documentation "Attempts to divide conflct into smaller ones. Retudnerd hash table maps position of conflict to conflict itself and should contain at least one element"))


(defgeneric single-elementp (conflict)
  (:documentation "Checks if conflict node holds just a single element. Returns t if it does, returns nil if it does not."))


(-> rebuild-rehashed-node (fundamental-hamt-container fixnum fixnum bottom-node) just-node)
(-> build-rehashed-node (fundamental-hamt-container fixnum fixnum hash-table) just-node)
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


(-> modify-copy-hamt (maybe-node fixnum fundamental-hamt-container (-> (bottom-node) (values maybe-node boolean)))
    (values hash-node boolean))
(defun modify-copy-hamt (root hash container bottom-fn)
  (declare (optimize (speed 3) (debug 0) (safety 0) (compilation-speed 0) (space 0)))
  ;; (declare (optimize debug))
  (fbind (bottom-fn)
    (flet ((cont (path indexes length) ;path and indexes have constant size BUT only part of it is used, that's why length is passed here
             (declare (type simple-array path indexes)
                      (type fixnum length))
             (iterate
               (for i from (- length 1) downto 0) ;reverse order (starting from deepest node)
               (for node = (aref path i))
               (for index = (aref indexes i))
               (for ac initially (let* ((last (aref path (1- length)))
                                        (bottom-node-or-nil (and (typep last 'bottom-node) last)))
                                   (multiple-value-bind (conflict changed) (bottom-fn bottom-node-or-nil)
                                     (unless changed
                                       (return-from modify-copy-hamt (values root nil)))
                                     (if (or (null bottom-node-or-nil)
                                             (null conflict)
                                             (eql length (1- (assure fixnum (read-max-depth container))))
                                             (single-elementp conflict))
                                         ;;if we didn't find element or element was found but depth was already maximal,
                                         ;;we will just return element, otherwise attempt to divide (rehash) conflicting node into few more
                                         conflict
                                         ;;rehash actually returns cl:hash-table, build-rehashed-node transforms it into another hash-node, depth is increased by 1 this way
                                         (rebuild-rehashed-node container
                                                                length
                                                                (read-max-depth container)
                                                                conflict))))
                    then (cond ((null node) ac) ;no node on path, just use our conflict node
                               ((typep node 'bottom-node) ac) ;corner case, added conflict or resolved conflict
                               (ac (if (hash-node-contains node index)
                                       (hash-node-replace-in-the-copy node ac index)
                                       (hash-node-insert-into-copy node ac index)))
                               (t (if (eql 1 (hash-node-size node))
                                      ac
                                      (hash-node-remove-from-the-copy node index)))))
               (finally (return (values ac t))))))
      (declare (dynamic-extent (function cont))
               (inline cont))
      (descend-into-hash root
                         (read-max-depth container)
                         hash
                         cont))))


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


(-> insert-into-hash! (maybe-node fixnum t fundamental-hamt-container) hash-node)
(defun insert-into-hash! (root hash item container)
  (let ((prev-node nil)
        (prev-index 0))
    (with-hash-tree-functions container
      (hash-do (node index c) (root hash)
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


(-> find-in-hash (hash-node fixnum t fundamental-hamt-container) (values t boolean))
(defun find-in-hash (root hash item container)
  "Obtain nodes in loop until node is missing or bottom-node was found
   @b(Arguments and values)
   @begin(list)
    @item(root -- root of scanned subtree)
    @item(hash -- fixnum containing hash of item)
    @item(container -- used for stored functions)"
  (declare (optimize (speed 3) (debug 0) (safety 0) (compilation-speed 0) (space 0)))
  (with-hash-tree-functions container
    (let ((node (hash-do (node index) (root hash))))
      (cond
        ((typep node 'bottom-node) (values (last-node-fn item node) t))
        (t nil)))))



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


(-> contains-part-of-hash (fixnum fixnum (integer 0 64)) boolean)
(defun contains-part-of-hash (hash partial-hash depth)
  (~>> hash
       (logxor partial-hash)
       (ldb (byte depth 0))
       zerop))

(defmethod hash-of-bottom-node ((node conflict-node) container)
  (declare (type fundamental-hamt-container container))
  (with-hash-tree-functions container
    (~> node
        access-conflict
        caar
        hash-fn)))


(defmethod rehash ((container hamt-dictionary) conflict level)
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
