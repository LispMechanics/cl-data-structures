(in-package :cl-data-structures)


(defgeneric at (container location)
  (:documentation "Obtain element stored at location in the container. This function will return one or two values, depending on the container.
  In case of associative containers second value informs if element was found (first value is nil if element was not found).
  In case of non-associtive containers (vectors for instance) the function returns value under location if location is valid, or condition will be raised."))


(defgeneric (setf at) (new-value container location)
  (:documentation "Destructivly insert/replace element in the container at location."))


(defgeneric add (container location new-value)
  (:documentation "Non-destructive insert element into container at location. Will return two values: first one is the new container, second one is boolean informing if insert took place."))


(defgeneric add! (container location new-value)
  (:documentation "Destructivly insert element into container at location. Will return two values: first one is the container, second one is boolean informing if insert took place "))


(defgeneric insert (container location new-value)
  (:documentation "Non-destructivly insert element into container at location. Will replace element under location if location was already occupied. Will return next container and second boolean value informing user if element was already in the container. Essentially purely functional (setf (at container "))


(defgeneric erase (container location)
  (:documentation "Non-destructivly remove element from the container"))


(defgeneric erase! (container location)
  (:documentation "Destructivly remove element from the container."))


(defgeneric size (container)
  (:documentation "How many elements container holds currently?"))


(defgeneric update (container location new-value)
  (:documentation "If location is taken in the container, update it. Returns three values, first: container with update location, second: t if update took place; nil otherwise, and previous value."))


(defgeneric update! (container location new-value)
  (:documentation "Destructive version of update."))


(defgeneric become-functional (container)
  (:method ((container functional)) container))


(defgeneric become-mutable (container)
  (:method ((container mutable)) container))


(defgeneric mutable-p (container)
  (:method ((container mutable)) t)
  (:method ((container fundamental-container)) nil))


(defgeneric functional-p (container)
  (:method ((container functional)) t)
  (:method ((container fundamental-container)) nil))


(defgeneric empty (container)
  (:method ((container fundamental-container)) (zerop (size container))))
