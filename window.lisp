;;
;; Lire - window setup
;;

(in-package :lire)

(defstruct window
  (time        0.0)
  (delta       0.0)
  (second-time 0.0)
  (temp-fps      0)
  (fps           0)
  
  (screen-width  800)
  (screen-height 600)
  
  (completions       nil)
  (completions-select -1)
  
  (position-x      0)
  (position-y      0)
  (camera-x        0)
  (camera-y        0)
  (real-camera-x   0)
  (real-camera-y   0)
  (zoom          1.0)
  (real-zoom     0.0)

  (new-node-name    "")
  (nodes            ())
  (connecting-nodes ())
  (nodes-at-screen  ())
  (selected-nodes   ())

  (selector nil)
  (selector-x 0)
  (selector-y 0)

  (mouse-x       0)
  (mouse-y       0)
  (shift       nil)
  (key-move    nil)
  (mouse-left  nil)
  (mouse-right nil))

(defparameter *window* (make-window))

(defun completion (string)
  "Swank symbol matching"
  (map 'list
       (lambda (n) (write-to-string (swank::fuzzy-matching.symbol n)))
       (let ((comps (sort
                     (swank::fuzzy-find-matching-symbols
                      string *package*)
                     #'> :key #'swank::fuzzy-matching.score)))
         (subseq comps 0 (min 9 (length comps))))))

(defun set-mouse-position ()
  (with-slots (mouse-x mouse-y position-x position-y) *window*
    (setf mouse-x position-x
          mouse-y position-y)))

(defun timing (delta)
  (with-slots (time second-time fps temp-fps) *window*
    (incf time delta)
    (if (< second-time 1.0)
        (progn
          (incf second-time delta)
          (incf temp-fps))
        (progn
          (setf second-time 0.0
                fps temp-fps
                temp-fps 0)))))

(defun input-update ()
  (with-slots (key-move
               selector
               mouse-left mouse-right
               selected-nodes
               mouse-x mouse-y
               selector-x selector-y
               position-x position-y)
      *window*
    (when key-move
      (set-mouse-position))
    
    (when (and mouse-left (not selector))
      (dolist (node selected-nodes) 
        (incf (node-x node) (- mouse-x selector-x))
        (incf (node-y node) (- mouse-y selector-y)))
      (setf selector-x mouse-x
            selector-y mouse-y))

    (when (not (or mouse-right key-move))
      (snap-to-grid position-x)
      (snap-to-grid position-y))))

(defun draw-lire ()
  (with-slots (selector
               selected-nodes
               selector-x selector-y
               mouse-x mouse-y
               position-x position-y
               zoom real-zoom
               camera-x real-camera-x
               camera-y real-camera-y
               time nodes
               nodes-at-screen
               new-node-name
               completions-select
               completions
               fps)
      *window*

    (gl:enable :blend)
    (gl:clear :color-buffer-bit)
    
    (gl:load-identity)
    (gl:color 0.1 0.1 0.1)
    (quad-shape 0 0 0 10 1)

    (gl:scale (incf real-zoom (lerp zoom real-zoom 0.3))
              real-zoom 0)

    (let ((slower 0.5))
      (gl:translate (- (incf real-camera-x
                             (lerp camera-x
                                   real-camera-x slower)))
                    (- (incf real-camera-y
                             (lerp camera-y
                                   real-camera-y slower)))
                    0))

                                        ; cross
    (let ((flicker (+ (abs (* (sin time) 0.6)) 0.2)))
      (gl:line-width 1)
      (gl:color 0 flicker flicker)
      (simple-cross position-x position-y 0.1))

                                        ; nodes
    (gl:line-width (floor (max (* zoom 10) 1)))
    (mapc #'draw-wires nodes)
    
    (gl:line-width 1)
    (mapc #'draw-node nodes-at-screen)
    
    (when selected-nodes
      (draw-selection (car selected-nodes) t)
      (mapc #'draw-selection (cdr selected-nodes)))

                                        ; arguments tip
    (let ((node (find-if #'mouse-at-node-p nodes-at-screen)))
      (when node
        (draw-args-list node)))

                                        ; selector
    (when selector
      (let* ((x (min selector-x mouse-x))
             (y (min selector-y mouse-y))
             (w (- (max selector-x mouse-x) x))
             (h (- (max selector-y mouse-y) y)))
        (gl:color 0.5 0.7 1 0.3)
        (aligned-quad-shape x y 0 w h)
        (gl:color 0.5 0.7 1)
        (aligned-quad-lines x y 0 w h)))

                                        ; new node
    (when (not (string= new-node-name ""))
      (let ((node (create-node :name new-node-name
                               :x position-x
                               :y position-y)))
        (setf (node-color node) (list 0.2 0.2 0.2))
        (draw-node node)))
    
                                        ; completion list
    (let ((count -1)
          (y (- position-y 0.05)))
      (dolist (comp completions)
        (if (= completions-select (incf count))
            (gl:color 0 1 1 1.0)
            (gl:color 1 1 1 0.1))
        (text comp position-x (decf y 0.050) 0.025 0)))
    
                                        ; gui
    (gl:load-identity)
    (gl:color 1 1 1 0.5)
    (text (format nil "fps: ~A" fps) -0.5 -0.9 0.03 0)
    (text (princ-to-string *package*) 0.5 -0.9 0.03 0)
    (text "|" 0 -0.9 0.03 (* time 12))
    (text "|" 0 -0.9 0.03 (* time 42))))

(defun main-screen (delta)
  "Update and render"
  
  (timing delta)
  (input-update)
  (draw-lire))

(defun idler (window)
  "Every frame routines"
  (with-slots (delta)
      *window*
    (let ((old-time (get-internal-real-time)))
      (main-screen delta)
      
      (gl:flush)
      (gl-swap-window window)
      
      (setf delta
            (/ (- (get-internal-real-time)
                  old-time)
               internal-time-units-per-second)))))

(let ((last-click-time 0.0))
  (defun press-mouse-left ()
    (with-slots (nodes-at-screen
                 mouse-left
                 mouse-x mouse-y
                 selected-nodes
                 shift
                 selector
                 selector-x selector-y)
        *window*
      (if (< (- (get-internal-real-time) last-click-time) 300)
          (progn                          ; double-click
            (let ((select (find-if #'mouse-at-node-p nodes-at-screen)))
              (when select (eval-tree select))))
          (progn                          ; single-click
            (setf mouse-left t)
            (let ((select (find-if #'mouse-at-node-p nodes-at-screen)))
              (if select
                  (progn
                    (if (find select selected-nodes)
                                        ; make parent first
                        (setf selected-nodes
                              (cons select (remove select
                                                   selected-nodes)))
                        (if shift
                            (setf selected-nodes
                                  (cons select selected-nodes))
                            (setf selected-nodes (list select)))))
                  (setf selector t)))
            (setf selector-x mouse-x
                  selector-y mouse-y)))
      (setf last-click-time (get-internal-real-time)))))

(defun release-mouse-left ()
  (with-slots (nodes-at-screen
               mouse-left shift
               mouse-x mouse-y
               selected-nodes
               selector
               selector-x selector-y
               position-x position-y)
      *window*
    (setf mouse-left nil)
    (dolist (selected selected-nodes)
      (snap-node-to-grid selected)
      (with-slots (x y parents) selected
        (mapc #'sort-childs parents)))
    (when selector
      (setf selector nil)
      (let ((selected (remove-if-not
                       (lambda (node)
                         (node-in-rect node
                                       selector-x selector-y
                                       mouse-x mouse-y))
                       nodes-at-screen)))
        (if selected
            (setf selected-nodes (sort
                                    (if shift
                                        (remove-duplicates 
                                         (append selected
                                                 selected-nodes))
                                        selected)
                                    #'> :key #'node-y))
            (setf selected-nodes ()))))
    (setf position-x mouse-x
          position-y mouse-y)
    (snap-to-grid position-x)
    (snap-to-grid position-y)))

(defun press-mouse-right ()
  (with-slots (mouse-right
               mouse-x mouse-y
               nodes-at-screen
               connecting-nodes
               selected-nodes
               nodes)
      *window*
    (setf mouse-right t)
    (let ((select (find-if #'mouse-at-node-p nodes-at-screen)))
      (when select
        (setf selected-nodes (if (not (find select selected-nodes))
                                   (list select)
                                   selected-nodes))
        (setf connecting-nodes selected-nodes)
        (let ((dot (create-node :name "." :x mouse-x :y mouse-y)))
          (push dot nodes)
          (push dot selected-nodes)
          (repose))
        (connect-selected)))))

(defun release-mouse-right ()
  (with-slots (mouse-right
               nodes-at-screen
               connecting-nodes
               selected-nodes
               nodes)
      *window*
    (setf mouse-right nil)
    (let ((select (find-if #'mouse-at-node-p (rest nodes-at-screen))))
      (when select
        (when (not (find select connecting-nodes))
          (setf selected-nodes
                (cons select selected-nodes))
          (connect-selected))
                                        ;(delete-nodes (list (car nodes)))) ???
        (destroy-connections (car nodes))
        (setf nodes (remove (car nodes) nodes))
        (repose)))
    (when connecting-nodes
      (snap-node-to-grid (car nodes))
      (setf selected-nodes ())
      (setf connecting-nodes ()))))

;;
;; init and input setup
;;

(defun run-lire ()
  "Init all stuff and define events"
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
      (sdl2-ttf:init)
      (sdl2:gl-set-attr :multisamplebuffers 1) 
      (sdl2:gl-set-attr :multisamplesamples 2) 
      (with-window (lire-window :title "Lire"
                                :w screen-width
                                :h screen-height
                                :flags '(:shown :resizable :opengl))
        (with-gl-context (gl-context lire-window)
          (gl-make-current lire-window gl-context)
          (gl:enable :texture-2d)
          (gl:blend-func :src-alpha :one-minus-src-alpha)
          (resize-viewport screen-width screen-height)
          (gl:clear-color 0.5 0.5 0.5 1)

          (repose)

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
                          (when (=
                                 event
                                 sdl2-ffi:+sdl-windowevent-size-changed+)
                            (setf screen-width width
                                  screen-height height)
                            (resize-viewport width height)
                            (repose)))
            (:quit        ()
                          (sdl2-ttf:quit)
                          (clean-text-hash)
                          (clean-texture-hash)
                          t)))))))
