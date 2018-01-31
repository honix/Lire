;;
;; Lire - window
;;

(in-package :lire)

(defclass lire-window (glut:window)
  ((width       :initform 800)
   (height      :initform 600)
   (mouse-x     :initform 0)
   (mouse-y     :initform 0)
   (shift       :initform nil)
   (mouse-left  :initform nil)
   (mouse-right :initform nil)

   (canvas        :initform nil)
   (active-module :initform nil))
  (:default-initargs :width 800 :height 600
                     :pos-x 100 :pos-y 100
                     :mode '(:double :rgb :stencil :multisample)
                     :tick-interval 16
                     :title "Lire"))

;;;
;;  Window initialization and input binding
;;;

(defmethod initialize-instance :before ((w lire-window) &rest rest)
  (declare (ignore rest))
  (with-slots (canvas active-module) w
    (setf canvas (make-instance 'canvas :window w)
          active-module canvas)))

(defmethod glut:display-window :before ((w lire-window))
  (sdl2-ttf:init)
  (clean-text-hash)
  (gl:clear-color 0.1 0.1 0.1 0)
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
  (gl:matrix-mode :modelview))

(defmethod glut:close ((w lire-window))
  (sdl2-ttf:quit))


(defmethod glut:display ((w lire-window))
  (with-simple-restart (display-restart "Display")
    (gl:clear :color-buffer :stencil-buffer-bit)
    
    (process (slot-value w 'canvas))
    ;; (for module in modules do (draw module))
    
    (glut:swap-buffers)))
  
(defmethod glut:idle ((w lire-window))
  ;; Updates
  ;(glut:post-redisplay)
  )

(defmethod glut:tick ((w lire-window))
  (with-simple-restart (tick-restart "Tick")
    (glut:post-redisplay)))

(defmethod mouse-motion ((w lire-window) x y)
  (with-slots (mouse-x mouse-y active-module) w
    (let ((dx (- mouse-x x))
          (dy (- mouse-y y)))
      (setf mouse-x x mouse-y y)
      (motion active-module x y dx dy))))

(defmethod glut:motion ((w lire-window) x y)
  (with-simple-restart (motion-restart "Motion")
    (mouse-motion w x y)))

(defmethod glut:passive-motion ((w lire-window) x y)
  (with-simple-restart (passive-motion-restart "Passive-motion")
    (mouse-motion w x y)))

(defmethod glut:mouse ((w lire-window) button state x y)
  (with-simple-restart (mouse-restart "Mouse")
    (with-slots (mouse-x mouse-y mouse-left mouse-right active-module) w
      (setf mouse-x x mouse-y y)
      (case button
        (:left-button
         (setf mouse-left (eq state :down)))
        (:middle-button
         nil)
        (:right-button
         (setf mouse-right (eq state :down)))
        (:wheel-up
         (mouse-whell active-module 1))
        (:wheel-down
         (mouse-whell active-module -1)))
      (mouse active-module button state x y))))

(defmethod glut:mouse-wheel ((w lire-window) button pressed x y)
  ;; Really does nothing. GLUT:MOUSE catching wheel events.
  (with-simple-restart (mouse-whell-restart "Mouse-whell")
    (with-slots (active-module) w
      (mouse-whell active-module y))))

(defmethod glut:special ((w lire-window) special-key x y)
  ;; Catches :KEY-F1 :KEY-LEFT-SHIFT :KEY-HOME :KEY-LEFT etc..
  (with-simple-restart (special-key-restart "Special-key")
    (with-slots (active-module shift) w
      (case special-key
        ((:key-left-shift :key-right-shift)
         (setf shift t)))
      (special-key active-module special-key))))

(defmethod glut:special-up ((w lire-window) special-key x y)
  (with-simple-restart (special-key-up-restart "Special-key-up")
    (with-slots (active-module shift) w
      (case special-key
        ((:key-left-shift :key-right-shift)
         (setf shift nil))))))

(defmethod glut:keyboard ((w lire-window) key x y)
  ;; Catches alphanumeric keys + #\Return #\Backspace #\Tab and etc..
  (with-simple-restart (keyboard-restart "Keyboard")
    (with-slots (active-module) w
      (if (graphic-char-p key)                  ; (alphanumericp key)
          (keyboard active-module key)
          (special-key active-module key)))))

(defmethod glut:keyboard-up ((w lire-window) key x y)
  (with-simple-restart (keyboard-up-restart "Keyboard-up")))



(defun run-lire-old ()
  (with-slots (nodes
               connecting-nodes
               completions
               completions-select
               selected-nodes
               screen-width screen-height
               new-node-name
               shift key-move
               zoom
               mouse-x mouse-y
               camera-x camera-y
               position-x position-y
               mouse-left mouse-right)
      *window*
    (with-init (:everything)
      (with-window (lire-window :title "Lire"
                                :w screen-width :h screen-height
                                :flags '(:shown :resizable :opengl))
        (with-gl-context (gl-context lire-window)
          (with-event-loop (:method :poll)
            (:textinput   (:text text)
                          (ignore-errors 
                            (let ((char (code-char text)))
                              (setf new-node-name
                                    (concatenate 'string
                                                 new-node-name
                                                 (list char)))))
                          (if (> (length new-node-name) 1)
                              (setf completions
                                    (completion new-node-name))))
            (:keydown     (:keysym keysym)
                          (case (scancode-symbol
                                 (scancode-value keysym))
                            (:scancode-tab
                             (eval-tree selected-nodes))
                            ((:scancode-return :scancode-kp-enter)
                             (when (> completions-select -1)
                               (setf new-node-name
                                     (string-downcase
                                      (nth completions-select
                                           completions))))
                             (insert-new-node)
                             (setf completions ()
                                   completions-select -1))
                            (:scancode-backspace
                             (when (> (length new-node-name) 0)
                               (setf new-node-name
                                     (subseq
                                      new-node-name 0
                                      (- (length new-node-name) 1)))
                               (if (> (length new-node-name) 1)
                                   (setf completions
                                         (completion new-node-name))
                                   (setf completions ()))))
                            (:scancode-delete
                             (delete-nodes selected-nodes))
                            ((:scancode-lshift :scancode-rshift)
                             (setf shift t))
                            (:scancode-kp-7
                             (connect-selected))
                            (:scancode-kp-1
                             (mapc #'destroy-connections
                                   selected-nodes))
                            
                            (:scancode-lalt
                             (press-mouse-left))
                            (:scancode-lctrl
                             (press-mouse-right))
                            (:scancode-kp-5
                             (setf camera-x position-x
                                   camera-y position-y))
                            ((:scancode-kp-4 :scancode-left)
                             (setf key-move t)
                             (incf position-x -0.2))
                            ((:scancode-kp-6 :scancode-right)
                             (setf key-move t)
                             (incf position-x 0.2))
                            ((:scancode-kp-8 :scancode-up) 
                             (if (not (string= new-node-name ""))
                                 (decf completions-select)
                                 (progn
                                   (setf key-move t)
                                   (incf position-y 0.2))))
                            ((:scancode-kp-2 :scancode-down)
                             (if (not (string= new-node-name ""))
                                 (incf completions-select)
                                 (progn
                                   (setf key-move t)
                                   (incf position-y -0.2))))
                            
                            (:scancode-f11
                             (full-screen lire-window))))
            (:keyup       (:keysym keysym)
                          (case (scancode-symbol
                                 (scancode-value keysym))
                            ;; (:scancode-escape
                            ;;  (push-event :quit))
                            ((:scancode-lshift :scancode-rshift)
                             (setf shift nil))
                            (:scancode-lalt
                             (release-mouse-left))
                            (:scancode-lctrl
                             (setf mouse-right nil))))
            
            (:mousemotion (:x x :y y :xrel xrel :yrel yrel)
                          (setf key-move nil)
                          (let ((asp (/ screen-width screen-height))
                                (half-width  (/ screen-width  2))
                                (half-height (/ screen-height 2)))
                            (setf mouse-x
                                  (float
                                   (+ (* (/ (- x half-width)
                                            half-width zoom)
                                         asp)
                                      camera-x))
                                  mouse-y
                                  (float
                                   (+ (/ (- y half-height)
                                         half-height zoom -1)
                                      camera-y)))
                            (when mouse-right
                              (if connecting-nodes
                                  (progn
                                    (incf (node-x (car nodes))
                                          (/ xrel half-height zoom))
                                    (incf (node-y (car nodes))
                                          (/ yrel half-height zoom -1)))
                                  (progn
                                    (incf camera-x
                                          (/ xrel half-height zoom -1))
                                    (incf camera-y
                                          (/ yrel half-height zoom))
                                    (repose))))))
            (:mousebuttondown (:button button)
                              (case button
                                (1 (press-mouse-left))
                                (2 (setf position-x mouse-x
                                         position-y mouse-y)
                                   (snap-to-grid position-x)
                                   (snap-to-grid position-y))
                                (3 (press-mouse-right))))
            (:mousebuttonup   (:button button)
                              (case button
                                (1 (release-mouse-left))
                                (3 (release-mouse-right))))
            (:mousewheel      (:y y)
                              (setf zoom (max (min (+ zoom (* y 0.1))
                                                     2)
                                                0.1))
                              (repose))
            
            (:idle        ()
                          (idler lire-window))
            (:windowevent (:event event :data1 width :data2 height)
                          ;; (when (=
                          ;;        event
                          ;;        sdl2-ffi:+sdl-windowevent-size-changed+)
                          ;;   (setf screen-width width
                          ;;         screen-height height)
                          ;;   (resize-viewport width height)
                          ;;   (e))
                          )
            (:quit        ()
                          ;; (sdl2-ttf:quit)
                          (clean-text-hash)
                          (clean-texture-hash)
                          t)))))))
