(in-package :cl-data-structures)


(defclass fundamental-container ()
  ()
  (:documentation "Root class."))


(defclass functional (fundamental-container)
  ()
  (:documentation "Object implements functional api."))


(defclass mutable (fundamental-container)
  ()
  (:documentation "Object implements mutable api."))
