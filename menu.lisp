;;
;; Lire - menu
;;

(in-package :lire)

(defclass menu (widget)
  ((x :initform 0) (y :initform 0)
   
   (buttons :initform
            `(("save" ,#'save-lire)
              ("load" ,#'load-lire)))))

(defmethod reshape ((menu menu))
  (with-slots (window x y (m-width width) (m-height height) buttons) menu
    (with-slots (width height) window
      (setf m-width  *menu-width*
            m-height (* *menu-button-height* (length buttons))
            x        (- width  m-width  *menu-offset*)
            y        (- height m-height *menu-offset*)))))

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
        (apply #'gl:color *normal-color*)
        (gl:line-width 1)
        (aligned-quad-lines 0 0 0 *menu-width* height))
      (apply #'gl:color *dimm-color*)
      (aligned-quad-shape 0 0 0 *menu-width* height)
      (let ((b-width/2  (/ *menu-width*         2))
            (b-height/2 (/ *menu-button-height* 2)))
        (loop
           for button in buttons
           and i from 0 do
             (let ((b-y (* i *menu-button-height*)))
               (aligned-quad-lines 0 b-y 0
                                   *menu-width* *menu-button-height*)
               (text (car button)
                     b-width/2 (+ b-y b-height/2)
                     (/ *menu-button-height* 3) 0)))))))
