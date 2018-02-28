;;
;; Lire - widget is a UI bulding block
;;

(in-package :lire)

(defclass widget ()
  ((window :type 'widget)
   (parent :type 'widget)
   (childs :initform ())

   (x      :initform 0)
   (y      :initform 0)
   (width  :initform 0)
   (height :initform 0)))

(defmethod add-child ((w widget) (child widget))
  (push child (slot-value w 'childs))
  (setf (slot-value child 'parent) w)
  (when (typep w 'glut:window)
    (setf (slot-value child 'window) w)))

(defmethod add-childs ((w widget) childs)
  (mapc (lambda (child) (add-child w child))
        (reverse childs)))

;;;
;;  Focus-pokus
;;;

(defmethod in-focus-p ((w widget) x y)
  "Returns t if mouse is over this widget"
  (with-slots ((mx x) (my y) width height) w
    (and (< mx x (+ mx width))
         (< my y (+ my height)))))

;;;
;;  Events
;;;

(defmethod reshape ((w widget)))

(defmethod draw ((w widget) active))

(defmethod motion       ((w widget) x y dx dy))
(defmethod mouse        ((w widget) button state x y))
(defmethod mouse-whell  ((w widget) y))

(defmethod special-key    ((w widget) key))
(defmethod special-key-up ((w widget) key))
(defmethod keyboard       ((w widget) key))
(defmethod keyboard-up    ((w widget) key))
