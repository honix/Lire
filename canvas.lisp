;;
;; Lire - canvas
;;

(in-package :lire)

(defparameter *grid-size* 50) ; px

(defclass canvas ()
  ((window :initarg :window)
   
   (completions        :initform ())
   (completions-select :initform  -1)
   
   (position-x    :initform 0) (position-y    :initform 0)
   (camera-x      :initform 0) (camera-y      :initform 0)
   (zoom          :initform 1.0)

   (new-node-name    :initform "")
   (nodes            :initform ())
   (connecting-nodes :initform ())
   (nodes-at-screen  :initform ())
   (selected-nodes   :initform ())

   (selector :initform nil)
   (selector-x :initform 0) (selector-y :initform 0)
   
   (key-move  :initform nil)
   (pointer-x :initform 0) (pointer-y :initform 0)))

;;;
;;  Symbol finder
;;;

(defun completion (string)
  (map 'list
       (lambda (n) (write-to-string (swank::fuzzy-matching.symbol n)))
       (let ((comps (sort
                     (swank::fuzzy-find-matching-symbols
                      string *package*)
                     #'> :key #'swank::fuzzy-matching.score)))
         (subseq comps 0 (min 9 (length comps))))))

;;;

(defmethod set-mouse-position ((canvas canvas))
  (with-slots (window position-x position-y) canvas
    (with-slots (mouse-x mouse-y) window
      (setf mouse-x position-x
            mouse-y position-y))))

;;;
;;  Every-frame procedures
;;;

(defmethod input-update ((canvas canvas))
  ;;
  ;; This function is somehow strange
  ;;
  (with-slots (window
               key-move
               selector
               selected-nodes
               selector-x selector-y
               pointer-x pointer-y
               position-x position-y)
      canvas
    (with-slots (mouse-left mouse-right) window
      (when key-move
        (set-mouse-position canvas))
      
      (when (and mouse-left (not selector))
        (dolist (node selected-nodes)
          (with-slots (x y) node
            (incf x (- pointer-x selector-x))
            (incf y (- pointer-y selector-y))))
        (setf selector-x pointer-x
              selector-y pointer-y))

      (when (not (or mouse-right key-move))
        (snap-to-grid position-x)
        (snap-to-grid position-y)))))

(defmethod draw ((canvas canvas))
  (with-slots (window
               selector
               selected-nodes
               selector-x selector-y
               position-x position-y
               pointer-x pointer-y
               zoom zoom-to
               camera-x camera-y 
               nodes nodes-at-screen new-node-name
               completions-select completions)
      canvas
   
    (gl:scale zoom zoom 0)
    (gl:translate (- camera-x) (- camera-y) 0)

                                        ; center
    (gl:line-width 2)
    (gl:color 1 1 1 0.1)
    (simple-cross 0 0 *grid-size*)
    (text-align "0,0" 10 20 *node-text-height* 0)
    
                                        ; pointer
    (let ((x pointer-x) (y pointer-y))
      (gl:line-width 2)
      (gl:color 1 1 0 0.1)
      (simple-cross (snap-to-grid x) (snap-to-grid y)
                    (/ *grid-size* 5)))
    
                                        ; position
    (let ((flicker (+ (abs (* (sin (get-time)) 0.6)) 0.2)))
      (gl:line-width 1)
      (gl:color 0 flicker flicker)
      (simple-cross position-x position-y (/ *grid-size* 2)))

                                        ; nodes
    (gl:line-width (floor (max (* zoom 10) 1)))
    (mapc #'draw-wires nodes)
    
    (gl:line-width 1)
    (mapc (lambda (node) (draw-node canvas node)) nodes-at-screen)
    
    (when selected-nodes
      (draw-selection (car selected-nodes) t)
      (mapc #'draw-selection (cdr selected-nodes)))

                                        ; arguments tip
    (let ((node (find-node-at-pointer canvas)))
      (when node
        (draw-args-list node)))

                                        ; selector
    (when selector
      (let* ((x (min selector-x pointer-x))
             (y (min selector-y pointer-y))
             (w (- (max selector-x pointer-x) x))
             (h (- (max selector-y pointer-y) y)))
        (gl:color 0.5 0.7 1 0.3)
        (aligned-quad-shape x y 0 w h)
        (gl:color 0.5 0.7 1)
        (aligned-quad-lines x y 0 w h)))

                                        ; new node
    (when (not (string= new-node-name ""))
      (let ((node (create-node :name new-node-name
                               :x position-x
                               :y position-y)))
        (setf (slot-value node 'color) (list 0.2 0.2 0.2))
        (draw-node canvas node)))
    
                                        ; completion list
    (let ((count -1)
          (y (+ position-y (* *node-height* 1.5))))
      (dolist (comp completions)
        (if (= completions-select (incf count))
            (gl:color 0 1 1 1.0)
            (gl:color 1 1 1 0.1))
        (text comp position-x (incf y (* *node-height* 1.1))
              *node-text-height* 0)))
    
                                        ; gui
    (gl:load-identity)
    (gl:color 1 1 1 0.5)
    (with-slots (height) window
      (text-align (princ-to-string *package*) 5 (- height 10) 10 0))))


(defmethod process ((canvas canvas))
  (input-update canvas)
  (draw canvas))

(defmethod set-zoom ((canvas canvas) amount)
  (with-slots (zoom camera-x camera-y pointer-x pointer-y) canvas
    (let ((prev-zoom zoom))
      (setf zoom (max (min (+ zoom amount) 2) 0.1))
      (let ((mul (- zoom prev-zoom)))
        (incf camera-x (* (- pointer-x camera-x) mul (/ zoom)))
        (incf camera-y (* (- pointer-y camera-y) mul (/ zoom))))))
  (repose canvas))

(defmethod set-pointer ((canvas canvas) x y)
  (with-slots (zoom
               camera-x camera-y
               pointer-x pointer-y)
      canvas
    (setf pointer-x (+ (/ x zoom) camera-x)
          pointer-y (+ (/ y zoom) camera-y))))

;;;
;;  Input procedures
;;;

(defmethod special-key ((canvas canvas) key)
  (with-slots (selected-nodes
               new-node-name
               key-move zoom zoom-to
               camera-x camera-y
               position-x position-y
               completions completions-select)
      canvas
    (print key)
    (case key
      (#\Tab
       (eval-tree selected-nodes))
      (#\Return
       (when (> completions-select -1)
         (setf new-node-name
               (string-downcase
                (nth completions-select
                     completions))))
       (insert-new-node canvas)
       (setf completions ()
             completions-select -1))
      (#\Backspace
       (when (> (length new-node-name) 0)
         (setf new-node-name
               (subseq
                new-node-name 0
                (- (length new-node-name) 1)))
         (if (> (length new-node-name) 1)
             (setf completions
                   (completion new-node-name))
             (setf completions ()))))
      (#\Rubout ; delete-key
       (delete-nodes canvas selected-nodes))
      ;; (:scancode-kp-7
      ;;  (connect-selected))
      ;; (:scancode-kp-1
      ;;  (mapc #'destroy-connections
      ;;        selected-nodes))
      
      ((:key-left-alt :key-right-alt)
       (press-pointer-primary canvas))
      ((:key-left-ctrl :key-right-ctrl)
       (press-pointer-alter canvas))
      (:key-home
       (setf camera-x 0
             camera-y 0
             zoom     1
             zoom-to  1)
       (repose canvas))
      (:key-left
       (setf key-move t)
       (incf position-x (- *grid-size*)))
      (:key-right
       (setf key-move t)
       (incf position-x *grid-size*))
      (:key-up 
       (if (not (string= new-node-name ""))
           (decf completions-select)
           (progn
             (setf key-move t)
             (incf position-y (- *grid-size*)))))
      (:key-down
       (if (not (string= new-node-name ""))
           (incf completions-select)
           (progn
             (setf key-move t)
             (incf position-y *grid-size*)))))))

(defmethod keyboard ((canvas canvas) key)
  (with-slots (new-node-name completions) canvas
    (ignore-errors 
      (setf new-node-name
            (concatenate 'string
                         new-node-name
                         (list key))))
    (if (> (length new-node-name) 1)
        (setf completions
              (completion new-node-name)))))

(defmethod motion ((canvas canvas) x y dx dy)
  (with-slots (window
               connecting-nodes nodes
               zoom camera-x camera-y
               key-move)
      canvas
    (set-pointer canvas x y)
    (setf key-move nil)
    (with-slots (mouse-right) window
      (when mouse-right
        (if connecting-nodes
            (progn
              ;; dublicates input-update function, this is better
              (incf (slot-value (car nodes) 'x)
                    (/ dx zoom -1))
              (incf (slot-value (car nodes) 'y)
                    (/ dy zoom -1)))
            (progn
              (incf camera-x
                    (/ dx zoom))
              (incf camera-y
                    (/ dy zoom))
              (repose canvas)))))))

(defmethod mouse ((canvas canvas) button state x y)
  (set-pointer canvas x y)
  (case button
    (:left-button (case state
                    (:down (press-pointer-primary canvas))
                    (:up   (release-pointer-primary canvas))))
    (:right-button (case state
                     (:down (press-pointer-alter canvas))
                     (:up   (release-pointer-alter canvas))))))

(defmethod mouse-whell ((canvas canvas) y)
  (set-zoom canvas (* y 0.1)))

(defparameter *double-click-limit* 0.3)

(let ((last-click-time 0.0))
  (defmethod press-pointer-primary ((canvas canvas))
    (with-slots (window
                 nodes-at-screen
                 selected-nodes
                 selector
                 selector-x selector-y
                 pointer-x pointer-y)
        canvas
      (with-slots (shift) window
        (let ((select (find-node-at-pointer canvas)))
          (if (< (- (get-time) last-click-time) *double-click-limit*)
              (progn                          ; double-click
                (when select (eval-tree select)))
              (progn                          ; single-click
                (if select
                    (progn
                      (if (find select selected-nodes)
                                        ; make parent first
                          (setf selected-nodes
                                (cons select (remove select
                                                     selected-nodes)))
                          (setf selected-nodes
                                (if shift
                                    (cons select selected-nodes)
                                    (list select)))))
                    (setf selector t))))))
      (setf selector-x pointer-x
            selector-y pointer-y)
      (setf last-click-time (get-time)))))

(defmethod release-pointer-primary ((canvas canvas))
  (with-slots (window
               nodes-at-screen
               selected-nodes
               selector
               selector-x selector-y
               position-x position-y
               pointer-x pointer-y)
      canvas
    (with-slots (shift mouse-left) window
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
                                         pointer-x pointer-y))
                         nodes-at-screen)))
          (if selected
              (setf selected-nodes (sort
                                    (if shift
                                        (remove-duplicates 
                                         (append selected
                                                 selected-nodes))
                                        selected)
                                    #'< :key (lambda (node) (slot-value node 'y))))
              (setf selected-nodes ()))))
      (setf position-x pointer-x
            position-y pointer-y))
    (snap-to-grid position-x)
    (snap-to-grid position-y)))

(defmethod press-pointer-alter ((canvas canvas))
  (with-slots (window
               nodes-at-screen
               connecting-nodes
               selected-nodes
               nodes
               pointer-x pointer-y)
      canvas
    (let ((select (find-node-at-pointer canvas)))
      (when select
        (setf selected-nodes (if (not (find select selected-nodes))
                                 (list select)
                                 selected-nodes))
        (setf connecting-nodes selected-nodes)
        (let ((dot (create-node :name "." :x pointer-x :y pointer-y)))
          (push dot nodes)
          (push dot selected-nodes)
          (repose canvas))
        (connect-selected canvas)))))

(defmethod release-pointer-alter ((canvas canvas))
  (with-slots (window
               nodes-at-screen
               connecting-nodes
               selected-nodes
               nodes)
      canvas
    (let ((select (find-if
                   (lambda (node) (pointer-at-node-p canvas node))
                   (rest nodes-at-screen))))
      (when select
        (when (not (find select connecting-nodes))
          (setf selected-nodes
                (cons select selected-nodes))
          (connect-selected canvas))
        ;; (delete-nodes (list (car nodes)))) ???
        (destroy-connections (car nodes))
        (setf nodes (remove (car nodes) nodes))
        (repose canvas)))
    (when connecting-nodes
      (snap-node-to-grid (car nodes))
      (setf selected-nodes ())
      (setf connecting-nodes ()))))

;;;
;;  Nodes work
;;;

(defmethod draw-node ((canvas canvas) (node node))
  (with-slots (zoom) canvas
    (with-slots (name x y width color message error parents) node
      (when (stringp name)
        (apply #'gl:color color)
        (quad-shape x y 0 width *node-height*))
      (gl:color 1 1 1)
      (when (> zoom 0.3)
        (text (if (stringp name) name
                  (case name (:list "(●)") (:dot "●") (t "?")))
              x y *node-text-height* 0)x
        (when message
          (if error
              (gl:color 1 1 0 0.5)
              (gl:color 0 1 1))
          (text message x (- y (* *node-height* 2)) *node-text-height* 0))))))

(defmethod node-at-screen ((canvas canvas) (node node))
  (with-slots (window
               zoom
               camera-x camera-y)
      canvas
    (with-slots (width height) window
      (node-in-rect node
                    camera-x
                    camera-y
                    (+ camera-x (/ width zoom))
                    (+ camera-y (/ height zoom))))))

(defmethod repose ((canvas canvas))
  (with-slots (nodes nodes-at-screen) canvas
    (setf nodes-at-screen
          (remove-if-not
           (lambda (node) (node-at-screen canvas node))
           nodes))))

(defmethod position-at-node-p ((canvas canvas) (node node))
  (with-slots (position-x position-y) canvas
    (point-at-node-p node position-x position-y)))

(defmethod pointer-at-node-p ((canvas canvas) (node node))
  (with-slots (pointer-x pointer-y) canvas
    (point-at-node-p node pointer-x pointer-y)))

(defmethod find-node-at-pointer ((canvas canvas))
  (with-slots (nodes-at-screen) canvas
    (find-if (lambda (node) (pointer-at-node-p canvas node))
             nodes-at-screen)))

(defmethod connect-selected ((canvas canvas))
  (with-slots (selected-nodes) canvas
    (let ((parent (car selected-nodes))
          (others (cdr selected-nodes)))
      (mapc (lambda (node) (flip-connection parent node))
            others))))

(defmethod delete-nodes ((canvas canvas) nodes-to-delete)
  (with-slots (nodes selected-nodes) canvas
    (mapc #'destroy-connections selected-nodes)
    (setf nodes (set-difference nodesnodes-to-delete)
          selected-nodes ())
    (repose canvas)))

(defmethod insert-new-node ((canvas canvas))
  (with-slots (nodes
               nodes-at-screen
               selected-nodes
               new-node-name
               position-x position-y)
      canvas
    (if (string= new-node-name "")
                                        ; hit 'enter' without symbol -> jump to last created node
        (let ((node (car nodes)))
          (when node
            (with-slots (x y) node
              (setf selected-nodes (list (car nodes)))
              (setf position-x x)
              (setf position-y (+ y *grid-size*)))))
                                        ; hit 'enter' with some symbol
        (let ((under (find-if (lambda (node) (position-at-node-p canvas node)) nodes-at-screen)))
          (if under
                                        ; new node overlaps old one -> replace it name!
              (progn
                (setf (slot-value under 'name)
                      (cond ((string= new-node-name " ") :list)
                            ((string= new-node-name ".") :dot)
                            (t new-node-name)))
                (node-update under))
                                        ; new node
              (let ((new-node (create-node :name new-node-name
                                           :x position-x
                                           :y position-y)))
                (push new-node nodes)

                (if selected-nodes
                    (progn
                      (flip-connection (car selected-nodes) new-node)
                      (incf position-x  *grid-size*))
                    (progn
                      (pushnew new-node selected-nodes)
                      (incf position-y *grid-size*)))))
          (setf new-node-name "")
          (repose canvas)))))
