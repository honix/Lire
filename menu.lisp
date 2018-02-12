;;
;; Lire - menu
;;

(in-package :lire)

;; this settings will be pushed to settings file
(defparameter *menu-width* 100) ; px
(defparameter *menu-button-height* 30) ; px
(defparameter *menu-offset* 10) ; px

(defclass menu (widget)
  ((x :initform 0) (y :initform 0)
   (height :initform 0)
   
   (buttons :initform
            `(("save" ,#'save-lire)
              ("load" ,#'load-lire)))))

(defmethod reshape ((menu menu))
  (with-slots (window x y height buttons) menu
    (with-slots (width (w-height height)) window
      (setf height (* *menu-button-height* (length buttons))
            x      (- width    *menu-width* *menu-offset*)
            y      (- w-height height       *menu-offset*)))))

;;;
;;  Focus
;;;

(defmethod in-focus-p ((menu menu) x y)
  "Returns t if mouse is over this widget"
  (with-slots ((mx x) (my y) height) menu
    (and (< mx x (+ mx *menu-width*))
         (< my y (+ my height)))))

;;;
;;  Events
;;;

(defmethod mouse ((menu menu) button state x y)
  (with-slots ((my y) buttons) menu
    (when (and (eq button :left-button)
               (eq state :down))
      (let ((n (floor (/ (- y my) *menu-button-height*))))
        (funcall (second (nth n buttons)))))))

;;;
;;  Draw
;;;

(defmethod draw ((menu menu) active)
  (with-slots (window x y height buttons) menu
    (gl:with-pushed-matrix
      (gl:translate x y 0)
      (when active
        (gl:color 0 0 1 1)
        (gl:line-width 2)
        (aligned-quad-lines 0 0 0 *menu-width* height))
      (gl:color 1 1 1 0.1)
      (aligned-quad-shape 0 0 0 *menu-width* height)
      (let ((b-width/2  (/ *menu-width*         2))
            (b-height/2 (/ *menu-button-height* 2)))
        (loop
           for button in buttons
           and i from 0 do
             (let ((b-y (* i *menu-button-height*)))
               (gl:color 1 1 1 0.1)
               (aligned-quad-lines 0 b-y 0
                                   *menu-width* *menu-button-height*)
               (gl:color 1 1 1 0.3)
               (text (car button)
                     b-width/2 (+ b-y b-height/2)
                     (/ *menu-button-height* 3) 0)))))))
