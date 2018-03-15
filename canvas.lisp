;;
;; Lire - canvas
;;

(in-package :lire)

(defclass canvas (widget)
  ((position-x    :initform 0) (position-y    :initform 0)
   (camera-x      :initform 0) (camera-y      :initform 0)
   (zoom          :initform 1.0)

   (nodes            :initform ())
   (connecting-nodes :initform ())
   (nodes-at-screen  :initform ())
   (selected-nodes   :initform ())
   (new-node         :initform (make-instance 'new-node))

   (clipboard :initform "()")

   (selector   :initform nil)
   (selector-x :initform 0) (selector-y :initform 0)
   
   (key-move  :initform nil)
   (pointer-x :initform 0) (pointer-y :initform 0)))

;;;
;;  Nodes work
;;;

(defmethod node-at-screen ((canvas canvas) (node node))
  (with-slots (window zoom camera-x camera-y) canvas
    (with-slots (width height) window
      (node-in-rect node
                    camera-x camera-y
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
      (mapc (lambda (node) (flip-connection parent node)) others))))

(defmethod delete-selected-nodes ((canvas canvas))
  (with-slots (nodes selected-nodes) canvas
    (mapc #'destroy-connections selected-nodes)
    (setf nodes          (set-difference nodes selected-nodes)
          selected-nodes ())
    (repose canvas)))

(defmethod select-last-node ((canvas canvas))
  (with-slots (nodes selected-nodes position-x position-y) canvas
    (let ((node (car nodes)))
      (when node
        (with-slots (x y) node
          (setf selected-nodes (list (car nodes)))
          (setf position-x x)
          (setf position-y (+ y *grid-size*)))))))

(defmethod insert-new-node ((canvas canvas) &key (link t))
  (with-slots (nodes
               nodes-at-screen
               selected-nodes
               new-node
               position-x position-y)
      canvas
    (let ((under (find-if
                  (lambda (node) (position-at-node-p canvas node))
                  nodes))
          (inserted-node))
      (if under
          ;; new node overlaps old one -> replace it name!
          (progn
            (setf (slot-value under 'name) (slot-value new-node 'name))
            (clear new-node)
            (node-update under)
            (setf inserted-node under))
          ;; actually new node
          (let ((node (produce-node new-node)))
            (push node nodes)
            (when link
              (if selected-nodes
                  (progn
                    (flip-connection (car selected-nodes) node)
                    (incf position-x  *grid-size*))
                  (progn
                    (pushnew node selected-nodes)
                    (incf position-y *grid-size*))))
            (setf inserted-node node)))
      (repose canvas)
      inserted-node)))

(defmethod clear ((canvas canvas))
  (with-slots (nodes selected-nodes) canvas    
    (setf selected-nodes ()
          nodes          ())
    (repose canvas)))

(defmethod inject-nodes ((canvas canvas) nodes-and-poses &key clear)
  (with-slots (new-node selected-nodes position-x position-y) canvas
    (when clear (clear canvas))
    (let ((in-position-x position-x)
          (in-position-y position-y))
      (labels ((inject-tree (nodes poses &key parent)
                 (let* ((name (car nodes))
                        (pose (car poses))
                        (x    (+ (car pose) (if *fragment* in-position-x 0)))
                        (y    (+ (cdr pose) (if *fragment* in-position-y 0))))
                   ;; set positions to check overlapping
                   (setf position-x x
                         position-y y)
                   (set-new-node new-node name x y))
                 (let ((last-node (insert-new-node canvas :link nil)))
                   (with-slots (parents childs) last-node
                     (when parent
                       (push parent parents))
                     (setf childs (mapcar (lambda (nodes poses)
                                            (inject-tree nodes
                                                         poses
                                                         :parent last-node))
                                          (cdr nodes)
                                          (cdr poses))))
                   last-node)))
        (mapc (lambda (tree)
                (let ((nodes (cadr (member :nodes tree)))
                      (poses (cadr (member :poses tree))))
                  (when (and nodes poses)
                    (inject-tree nodes poses))))
              nodes-and-poses))
      (setf position-x in-position-x
            position-y in-position-y))))

;;;
;;
;;;

(defmethod set-zoom ((canvas canvas) amount)
  (with-slots (zoom camera-x camera-y pointer-x pointer-y) canvas
    (let ((prev-zoom zoom))
      (setf zoom (max (min (+ zoom (* amount zoom)) *zoom-max*) *zoom-min*))
      (let ((mul (- zoom prev-zoom)))
        (incf camera-x (* (- pointer-x camera-x) mul (/ zoom)))
        (incf camera-y (* (- pointer-y camera-y) mul (/ zoom))))))
  (repose canvas))

(defmethod set-pointer ((canvas canvas) x y)
  (with-slots (zoom camera-x camera-y pointer-x pointer-y)
      canvas
    (setf pointer-x (+ (/ x zoom) camera-x)
          pointer-y (+ (/ y zoom) camera-y))))

(defmethod set-pointer-position ((canvas canvas))
  (with-slots (pointer-x pointer-y position-x position-y) canvas
      (setf pointer-x position-x
            pointer-y position-y)))

;;;
;;  Virtual input
;;;

(let ((last-click-time 0.0))
  (defmethod press-pointer-primary ((canvas canvas))
    (with-slots (window selected-nodes selector
                        selector-x selector-y
                        pointer-x pointer-y)
        canvas
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
                              (if (slot-value window 'shift)
                                  (cons select selected-nodes)
                                  (list select)))))
                  (setf selector t)))))
      (setf selector-x pointer-x
            selector-y pointer-y)
      (setf last-click-time (get-time)))))

(defmethod release-pointer-primary ((canvas canvas))
  (with-slots (window nodes-at-screen selected-nodes
                      selector selector-x selector-y
                      position-x position-y
                      pointer-x pointer-y)
      canvas
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
            (setf selected-nodes
                  (sort
                   (if (slot-value window 'shift)
                       (remove-duplicates 
                        (append selected
                                selected-nodes))
                       selected)
                   #'< :key (lambda (node) (slot-value node 'y))))
            (setf selected-nodes ()))))
    (setf position-x pointer-x
          position-y pointer-y)
    (snap-to-grid position-x)
    (snap-to-grid position-y)))

(defmethod press-pointer-alter ((canvas canvas))
  (with-slots (connecting-nodes selected-nodes nodes pointer-x pointer-y)
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
  (with-slots (nodes-at-screen connecting-nodes selected-nodes nodes)
      canvas
    (let ((select (find-if
                   (lambda (node) (pointer-at-node-p canvas node))
                   (rest nodes-at-screen))))
      (when select
        (when (not (find select connecting-nodes))
          (setf selected-nodes
                (cons select selected-nodes))
          (connect-selected canvas))
        ;; remove dot node
        (destroy-connections (car nodes))
        (setf nodes (remove (car nodes) nodes))
        (repose canvas)))
    (when connecting-nodes
      (snap-node-to-grid (car nodes))
      (setf selected-nodes   ()
            connecting-nodes ()))))

;;;
;;  Input binds
;;;

(defmethod reshape ((canvas canvas))
  (with-slots (window (w-width width) (w-height height)) canvas
    (with-slots (width height) window
        (setf w-width  width
              w-height height)))
  (repose canvas))

(defmethod special-key ((canvas canvas) key)
  ;; (print key)
  (with-slots (selected-nodes
               new-node key-move zoom
               camera-x camera-y position-x position-y
               clipboard)
      canvas
    (case key
      (#\Tab
       (eval-tree selected-nodes))
      (#\Return
       (if (is-empty-p new-node)
           (select-last-node canvas)
           (progn
             (accept new-node)
             (insert-new-node canvas))))
      (#\Backspace
       (backspace new-node))
      (#\Rubout ; delete-key
       (delete-selected-nodes canvas))
      (#\     ; copy
       (let ((*fragment* t))
         (setf clipboard (write-lire selected-nodes))))
      (#\     ; paste
       (let ((*fragment* t))
         (inject-nodes canvas (read-from-string clipboard))))
      
      (#\
       (connect-selected canvas))
      (#\
       (mapc #'destroy-connections
             selected-nodes))
      
      ((:key-left-alt :key-right-alt)
       (press-pointer-primary canvas))
      ((:key-left-ctrl :key-right-ctrl)
       (press-pointer-alter canvas))
      (:key-home
       (setf camera-x 0 camera-y 0 zoom 1)
       (repose canvas))
      (:key-left
       (setf key-move t)
       (incf position-x (- *grid-size*)))
      (:key-right
       (setf key-move t)
       (incf position-x *grid-size*))
      (:key-up 
       (if (not (is-empty-p new-node))
           (select new-node -1)
           (progn
             (setf key-move t)
             (incf position-y (- *grid-size*)))))
      (:key-down
       (if (not (is-empty-p new-node))
           (select new-node 1)
           (progn
             (setf key-move t)
             (incf position-y *grid-size*)))))
    (case key
      ((:key-left :key-right :key-up :key-down)
       (set-pointer-position canvas)))))

(defmethod special-key-up ((canvas canvas) key)
  (case key
    ((:key-left-alt :key-right-alt)
     (release-pointer-primary canvas))
    ((:key-left-ctrl :key-right-ctrl)
     (release-pointer-alter canvas))))

(defmethod keyboard ((canvas canvas) key)
  ;; (print key)
  (with-slots (window new-node) canvas
    (with-slots (ctrl alt) window
      (cond
        (ctrl ())
        (alt  ())
        (t (keyboard new-node key))))))

(defmethod keyboard-up ((canvas canvas) key))

(defmethod motion ((canvas canvas) x y dx dy)
  (with-slots (window
               connecting-nodes nodes
               selected-nodes
               zoom camera-x camera-y
               selector selector-x selector-y
               pointer-x pointer-y
               key-move)
      canvas
    (set-pointer canvas x y)
    (setf key-move nil)
    (with-slots (mouse-left mouse-right) window
      (when (and mouse-left (not selector))
        (dolist (node selected-nodes)
          (with-slots (x y) node
            (incf x (- pointer-x selector-x))
            (incf y (- pointer-y selector-y))))
        (setf selector-x pointer-x
              selector-y pointer-y))
      (when mouse-right
        (if connecting-nodes
            (progn
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

;;;
;;  Focus
;;;

(defmethod in-focus-p ((canvas canvas) x y)
  "Returns t if mouse is over this widget"
  t)

;;;
;;  Draw
;;;

(defmethod draw ((canvas canvas) active)
  (with-slots (window
               selector
               selected-nodes
               selector-x selector-y
               position-x position-y
               pointer-x pointer-y
               zoom
               camera-x camera-y 
               nodes nodes-at-screen new-node)
      canvas
    
    (gl:scale zoom zoom 0)
    (gl:translate (- camera-x) (- camera-y) 0)
    
                                        ; center
    (gl:line-width 2)
    (apply #'gl:color *dimm-color*)
    (simple-cross 0 0 *grid-size*)
    (text-align "0,0" 10 20 *node-text-height* 0)
    
                                        ; position
    (gl:line-width 1)
    (apply #'gl:color *normal-color*)
    (simple-cross position-x position-y (/ *grid-size* 2))

                                        ; nodes
    (mapc (lambda (node)
            (draw-wires node
                        (floor (max (* zoom 10) 1))))
          nodes)

    (let ((show-name (> zoom 0.3)))
      (mapc (lambda (node) (draw-node node show-name)) nodes-at-screen))
    
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
        (apply #'gl:color *selector-color*)
        (aligned-quad-shape x y 0 w h)))

                                        ; new node
    (unless (is-empty-p new-node)
      (with-slots (x y) new-node
        (setf x position-x
              y position-y)
        ;; (setf (slot-value node 'color) '(0 0 0 0.75))
        (draw-node new-node t)))
    
                                        ; overlay
    (with-slots (width height) window
      (gl:load-identity)
      
      (when active
        (apply #'gl:color *normal-color*)
        (gl:line-width 2)
        (aligned-quad-lines 0 0 0 width height))
      
      (apply #'gl:color *dimm-color*)
      (text-align (princ-to-string *package*) 5 (- height 10) 10 0))))
