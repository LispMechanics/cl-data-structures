(in-package :cl-data-structures)


(defgeneric at (container location)
  (:documentation "Obtain element stored at LOCATION in the CONTAINER. This function will return one or two values, depending on the CONTAINER.
  In case of associative containers, second value informs if element was found (first value is nil if element was not found).
  In case of non-associtive containers (e.g. vectors), the function returns value under LOCATION if LOCATION is valid, otherwise condition of type TBD will be raised."))


(defgeneric (setf at) (new-value container location)
  (:documentation "Destructively insert/replace element in the CONTAINER at LOCATION witn NEW-VALUE."))


(defgeneric add (container location new-value)
  (:documentation "Non-destructively insert NEW-VALUE into CONTAINER at LOCATION. Will return two values: first one is the new container, second one is boolean informing if insert took place."))


(defgeneric add! (container location new-value)
  (:documentation "Destructively insert NEW-VALUE into CONTAINER at LOCATION. Will return two values: first one is the modified CONTAINER, second one is boolean informing if insert took place "))


(defgeneric insert (container location new-value)
  (:documentation "Non-destructively insert NEW-VALUE into CONTAINER at LOCATION. Will replace element value at LOCATION if it was already occupied. Essentially purely functional (SETF (AT CONTAINER) NEW-VALUE)."))


(defgeneric erase (container location)
  (:documentation "Non-destructively remove element at LOCATION from the CONTAINER."))


(defgeneric erase! (container location)
  (:documentation "Destructively remove element at LOCATION from the CONTAINER."))


(defgeneric size (container)
  (:documentation "How many elements CONTAINER holds currently?"))


(defgeneric update (new-value container location)
  (:documentation "If LOCATION is taken in the CONTAINER, update it. Returns two values, first: new CONTAINER with updated LOCATION, second: t if update took place, nil otherwise."))


(defgeneric update! (new-value container location)
  (:documentation "Destructive version of UPDATE."))


(defgeneric become-functional (container)
  (:method ((container functional)) container))


(defgeneric become-mutable (container)
  (:method ((container mutable)) container))


(defgeneric mutable-p (container)
  (:method ((container mutable)) t)
  (:method ((container t)) nil))


(defgeneric functional-p (container)
  (:method ((container functional)) t)
  (:method ((container t)) nil))
