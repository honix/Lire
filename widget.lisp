;;
;; Lire - widget is a UI bulding block
;;

(in-package :lire)

(defclass widget ()
  ((window :type 'widget)
   (parent :type 'widget)
   (childs :initform ())))

(defmethod add-child ((w widget) (child widget))
  (push child (slot-value w 'childs))
  (setf (slot-value child 'parent) w)
  (when (typep w 'glut:window)
    (setf (slot-value child 'window) w)))

(defmethod add-childs ((w widget) childs)
  (mapc (lambda (child) (add-child w child))
        childs))

;;;
;;  Events
;;;

(defmethod in-focus-p ((w widget) x y))

(defmethod reshape ((w widget)))

(defmethod draw ((w widget)))

(defmethod motion       ((w widget) x y dx dy))
(defmethod mouse        ((w widget) button state x y))
(defmethod mouse-whell  ((w widget) y))

(defmethod special-key    ((w widget) key))
(defmethod special-key-up ((w widget) key))
(defmethod keyboard       ((w widget) key))
(defmethod keyboard-up    ((w widget) key))
