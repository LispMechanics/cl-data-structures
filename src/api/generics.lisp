(in-package :cl-data-structures)


(defgeneric at (container location)
  (:documentation "Obtain element stored at LOCATION in the CONTAINER. This function will @b(return) one or two values, depending on the CONTAINER.
  In case of associative containers, second value informs if element was found (first value is nil if element was not found).
  In case of non-associtive containers (e.g. vectors), the function returns value under LOCATION if LOCATION is valid, otherwise condition of type TODO will be raised.

  @b(Arguments and values:)
  @begin(list)
  @item(container -- instance of subclass of fundamental-container)
  @item(location -- where are we looking at? Key in hashtable, index of vector, etc)
  @end(list)

  @b(Side effects:) None."))


(defgeneric (setf at) (new-value container location)
  (:documentation "@b(Mutable API:) Destructively insert/replace element in the CONTAINER at LOCATION with NEW-VALUE."))


(defgeneric add (container location new-value)
  (:documentation "@b(Functional API:) Non-destructively insert NEW-VALUE into CONTAINER at LOCATION. Will return two values: first one is the new container, second one is boolean informing if insert took place.

  @b(Side effects:) None"))


(defgeneric add! (container location new-value)
  (:documentation "@b(Mutable API:) Destructively insert NEW-VALUE into CONTAINER at LOCATION. Will return two values: first one is the modified CONTAINER, second one is boolean informing if insert took place "))


(defgeneric insert (container location new-value)
  (:documentation "@b(Functional API:) Non-destructively insert NEW-VALUE into CONTAINER at LOCATION. Will replace element value at LOCATION if it was already occupied. Will return up to three values: new container, boolean to inform user if element was already in the container and old value AT location (or nil if it was not present). Essentially purely functional (SETF (AT CONTAINER) NEW-VALUE).

 @b(Side effects:) None"))


(defgeneric erase (container location)
  (:documentation "@b(Functional API:) Non-destructively remove element at LOCATION from the CONTAINER.

  @b(Side effects:) None"))


(defgeneric erase! (container location)
  (:documentation "@b(Mutable API:) Destructively remove element at LOCATION from the CONTAINER."))


(defgeneric size (container)
  (:documentation "How many elements CONTAINER holds currently?

  @b(Side effects:) None"))


(defgeneric update (container location new-value)
  (:documentation "@b(Functional API:) If LOCATION is taken in the CONTAINER, update it. @b(Returns) three values:
   @begin(list)
   @item(first -- new CONTAINER with update LOCATION)
   @item(second -- t if update took place nil otherwise)
   @item(third -- previous value).
   @end(list)
   @b(Side effects:) None"))


(defgeneric update! (new-value container location)
  (:documentation "@b(Mutable API:) Destructive version of UPDATE."))


(defgeneric become-functional (container)
  (:method ((container functional)) container)
  (:documentation "@b(Returns) new CONTAINER presenting functional API of CONTAINER. Content of returned CONTAINER is identical to the input CONTAINER.

  @b(Note:) Not all containers implement this GF, but returned object should also implement become-mutable GF.

  @b(Side effects:) None, but some (or all) parts of internal representation are shared beetween both instances. Side effects from mutable CONTAINER may leak into functional CONTAINER."))


(defgeneric become-mutable (container)
  (:method ((container mutable)) container)
  (:documentation "@b(Returns) new CONTAINER presenting mutable API of CONTAINER. Content of returned CONTAINER is identical to the input CONTAINER.

  @b(Note:) Not all containers implement this GF, but returned object should also implement become-functional GF.

  @b(Side effects:) None, but some (or all) parts of internal representation are shared beetween both instances. Side effects from mutable CONTAINER may leak into functional CONTAINER."))


(defgeneric mutable-p (container)
  (:method ((container mutable)) t)
  (:method ((container fundamental-container)) nil)
  (:documentation "Will @b(return) T if CONTAINER exposes mutable API."))


(defgeneric functional-p (container)
  (:method ((container functional)) t)
  (:method ((container fundamental-container)) nil)
  (:documentation "Will @b(return) T if CONTAINER exposes functional API."))


(defgeneric empty (container)
  (:method ((container fundamental-container)) (zerop (size container)))
  (:documentation "@b(Returns) T if container is empty and @b(returns) NIL if there is something in it. All containers start as empty

  @b(Side effects:) None"))
