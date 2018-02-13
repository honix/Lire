;;
;; Lire - window
;;

(in-package :lire)

(defclass lire-window (glut:window widget)
  ((mouse-x     :initform 0)
   (mouse-y     :initform 0)
   (shift       :initform nil)
   (mouse-left  :initform nil)
   (mouse-right :initform nil)

   (active-child :initform nil))
  (:default-initargs :width *initial-width* :height *initial-height*
                     :pos-x 100 :pos-y 100
                     :mode '(:double :rgb :stencil :multisample)
                     :tick-interval 16
                     :title "Lire"))

;;;
;;  Window initialization and input binding
;;;

(defmethod initialize-instance :after ((w lire-window) &rest rest)
  (declare (ignore rest))
  (with-slots (childs active-child) w
    (add-childs w (list (make-instance 'canvas)
                        (make-instance 'menu)))
    (setf active-child (first childs))))

(defmethod glut:display-window :before ((w lire-window))
  (sdl2-ttf:init)
  (clean-text-hash)
  (gl:enable :texture-2d :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha))

(defmethod glut:reshape ((w lire-window) width height)
  (setf (slot-value w 'width) width
        (slot-value w 'height) height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (let ((hwidth  (/ width 2))
        (hheight (/ height 2)))
    (gl:ortho (- hwidth)  (+ hwidth)
              (+ hheight) (- hheight) 0 1)
    (gl:translate (- hwidth) (- hheight) -1))
  (gl:matrix-mode :modelview)

  (with-slots (childs) w
    (loop for child in childs do
         (reshape child))))

(defmethod glut:close ((w lire-window))
  (sdl2-ttf:quit))


(defmethod glut:display ((w lire-window))
  (with-simple-restart (display-restart "Display")
    (with-slots (childs active-child) w
      (apply #'gl:clear-color *background-color*)
      (gl:clear :color-buffer :stencil-buffer-bit)
      
      (loop for child in childs do
           (draw child (eq child active-child)))
      
      (glut:swap-buffers))))
  
(defmethod glut:idle ((w lire-window))
  ;; Updates
  ;(glut:post-redisplay)
  )

(defmethod glut:tick ((w lire-window))
  (with-simple-restart (tick-restart "Tick")
    (glut:post-redisplay)))

(defmethod mouse-motion ((w lire-window) x y)
  (with-slots (mouse-x mouse-y active-child) w
    (let ((dx (- mouse-x x))
          (dy (- mouse-y y)))
      (setf mouse-x x mouse-y y)
      (motion active-child x y dx dy))))

(defmethod glut:motion ((w lire-window) x y)
  (with-simple-restart (motion-restart "Motion")
    (mouse-motion w x y)))

(defmethod glut:passive-motion ((w lire-window) x y)
  (with-simple-restart (passive-motion-restart "Passive-motion")
    (with-slots (childs active-child) w
      (mouse-motion w x y))))

(defmethod glut:mouse ((w lire-window) button state x y)
  (with-simple-restart (mouse-restart "Mouse")
    (with-slots (mouse-x mouse-y mouse-left mouse-right childs active-child) w
      (setf mouse-x x mouse-y y)
      (when (eq state :down)
        (setf active-child (find-if (lambda (w) (in-focus-p w x y))
                                    childs
                                    :from-end t)))
      (case button
        (:left-button
         (setf mouse-left (eq state :down)))
        (:middle-button
         nil)
        (:right-button
         (setf mouse-right (eq state :down)))
        (:wheel-up
         (mouse-whell active-child 1))
        (:wheel-down
         (mouse-whell active-child -1)))
      (mouse active-child button state x y))))

(defmethod glut:mouse-wheel ((w lire-window) button pressed x y)
  ;; This method works on windows. Linux sends all mouse events to GLUT:MOUSE
  (with-simple-restart (mouse-whell-restart "Mouse-whell")
    (with-slots (active-child) w
      (case pressed
        ((:up   :wheel-up)   (mouse-whell active-child 1))
        ((:down :wheel-down) (mouse-whell active-child -1))))))

(defmethod glut:special ((w lire-window) special-key x y)
  ;; Catches :KEY-F1 :KEY-LEFT-SHIFT :KEY-HOME :KEY-LEFT etc..
  (with-simple-restart (special-key-restart "Special-key")
    (with-slots (active-child shift) w
      (case special-key
        ((:key-left-shift :key-right-shift)
         (setf shift t)))
      (special-key active-child special-key))))

(defmethod glut:special-up ((w lire-window) special-key x y)
  (with-simple-restart (special-key-up-restart "Special-key-up")
    (with-slots (active-child shift) w
      (case special-key
        ((:key-left-shift :key-right-shift)
         (setf shift nil)))
      (special-key-up active-child special-key))))

(defmethod glut:keyboard ((w lire-window) key x y)
  ;; Catches alphanumeric keys + #\Return #\Backspace #\Tab and etc..
  (with-simple-restart (keyboard-restart "Keyboard")
    (with-slots (active-child) w
      (if (graphic-char-p key)
          (keyboard active-child key)
          (special-key active-child key)))))

(defmethod glut:keyboard-up ((w lire-window) key x y)
  (with-simple-restart (keyboard-up-restart "Keyboard-up")
    (with-slots (active-child) w
      (if (graphic-char-p key)
          (keyboard-up active-child key)
          (special-key-up active-child key)))))
