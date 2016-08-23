;;
;; Visual List Editor
;;

(ql:quickload '(:cl-opengl :sdl2-ttf :sdl2-image))

(defpackage :vle
  (:use :cl :cl-opengl :sdl2))

(in-package :vle)

(load (merge-pathnames "utils.lisp"))

(defstruct node
  name
  x y
  width
  color
  message
  error
  parents
  childs)

(defun node-update (node)
  (setf (node-width node)
	(+ (* (length (node-name node)) *node-width-char*)
	   *node-width-bumps*)
	(node-color node)
	(if (ignore-errors (symbol-function
			    (read-from-string (node-name node))))
	    (hsv-to-rgb (mod (sxhash (node-name node)) 360)
			0.75 0.55)
	    (hsv-to-rgb 0 0 0.3)))
  node)

(defun update-tree (node)
  (with-slots (childs) node
    (node-update node)
    (when childs
	(mapc #'update-tree childs))))

(defun create-node (&key name x y)
  (let ((node (make-node :name name :x x :y y)))
    (node-update node)))
	     
;; main
(defparameter *time* 0.0)
(defparameter *delta* 0.0)
(defparameter *screen-width* 800)
(defparameter *screen-height* 600)
;; travel
(defparameter *position-x* 0)
(defparameter *position-y* 0)
(defparameter *camera-x* 0)
(defparameter *camera-y* 0)
(defparameter *real-camera-x* 0)
(defparameter *real-camera-y* 0)
(defparameter *zoom* 1.0)
(defparameter *real-zoom* 1.0)
;; *nodes* visual
(defparameter *node-height* 0.06)
(defparameter *node-width-char* 0.021) ;?
(defparameter *node-width-bumps* 0.05)
;; *nodes*
(defparameter *new-node-name* "")
(defparameter *nodes* ())
(defparameter *nodes-at-screen* ())
(defparameter *selected-nodes* ())
;; selector
(defparameter *selector* nil)
(defparameter *selector-x* 0)
(defparameter *selector-y* 0)
;; inputs
(defparameter *mouse-x* 0)
(defparameter *mouse-y* 0)
(defparameter *key-move* nil)
(defparameter *mouse-left* nil)
(defparameter *mouse-right* nil)

;;
;; think only at screen 
;;

(defun node-in-rect (node x1 y1 x2 y2)
  (let ((x1 (min x1 x2))
	(y1 (min y1 y2))
	(x2 (max x1 x2))
	(y2 (max y1 y2)))
    (with-slots (x y) node
      (and
       (< x1 x x2)
       (< y1 y y2)))))

(defun node-at-screen (node)
  (let ((asp (/ *screen-width* *screen-height*)))
    (node-in-rect node
		  (- *camera-x* (* (/ *zoom*) asp) 1)
		  (- *camera-y* (/ *zoom*) 1)
		  (+ *camera-x* (* (/ *zoom*) asp) 1)
		  (+ *camera-y* (/ *zoom*) 1))))

(defun repose ()
  (setf *nodes-at-screen*
	(remove-if-not #'node-at-screen *nodes*)))

;;
;; node draw functions
;;

(defun draw-wires (node)
  (with-slots (x y color childs) node
    (let* ((pulse-in  (/ (mod *time* 6) 6))
	   (pulse-out (min 1 (* pulse-in 1.3))))
      (dolist (child childs)
	(apply #'gl:color color)
	(simple-line x y (node-x child) (node-y child))
	(gl:color 0 1 1)
	(simple-line
	 (+ (node-x child) (* (- x (node-x child)) pulse-in))
	 (+ (node-y child) (* (- y (node-y child)) pulse-in))
	 (+ (node-x child) (* (- x (node-x child)) pulse-out))
	 (+ (node-y child) (* (- y (node-y child)) pulse-out)))))))

(defun draw-node (node)
  (with-slots (name x y width color
		    message error parents) node
    (gl:push-matrix)
    (gl:translate x y 0)
    #|(if (null parents)
    (gl:color 0.5 0.0 0.5)
    (gl:color 0.2 0.2 0.2))|#
    (apply #'gl:color color)
    (quad-shape 0 0 0 width *node-height*)
    (gl:color 1 1 1)
    (when (> *zoom* 0.3)
      (text name 0 0 0.04 0)
      (when message
	(if error
	    (gl:color 1 1 0 0.5)	
	    (gl:color 0 1 1))
	(text message 0 0.1 0.04 0)))
    (gl:pop-matrix)))

(defun draw-selection (node &optional first)
  (with-slots (x y width) node
    (gl:push-matrix)
    (gl:translate x y 0)
    (if first
	(gl:color 1 1 0)
	(gl:color 0 1 1))
    (quad-lines 0 0 0
		(+ width         0.02)
		(+ *node-height* 0.02))
    (gl:pop-matrix)))

;;
;; nodes utils
;;

(defun mouse-at-node-p (node)
  (with-slots (x y width) node
    (let ((w width)
	  (h *node-height*))
      (and (< (- x w) *mouse-x* (+ x w))
	   (< (- y h) *mouse-y* (+ y h))))))

(defun make-connection (parent child)
  (when (not (eq parent child))
    (pushnew child
	     (node-childs parent))
    (pushnew parent
	     (node-parents child))
    (when (node-message child)
      (setf (node-message child) nil))
    (dolist (head (find-heads child))
      (when (node-message head)
	(setf (node-message head) "?")))))

(defun connect-selected ()
  (let ((parent (car *selected-nodes*))
	(others (cdr *selected-nodes*)))
    (mapc (lambda (node) (make-connection parent node))
	  others)))

(defun destroy-connections (node)
  (dolist (child (node-childs node))
    (setf (node-parents child)
	  (remove node (node-parents child))))
  (dolist (parent (node-parents node))
    (setf (node-childs parent)
	  (remove node (node-childs parent))))
  (setf (node-childs  node) ())
  (setf (node-parents node) ()))

(defun insert-new-node ()
  (when (not (string= *new-node-name* ""))
    (let ((new-node (create-node :name *new-node-name*
				 :x *position-x*
				 :y *position-y*)))
      (push new-node *nodes*)
      (setf *new-node-name* "")
      (if *selected-nodes*
	  (progn
	    (make-connection (car *selected-nodes*) new-node)
	    (incf *position-x* 0.2))
	  (progn
	    (pushnew new-node *selected-nodes*)
	    (incf *position-y* -0.2)))
      (repose))))

;;
;; evaluation
;;

(defun find-heads (node)
  "Find all tree-heads linked to node"
  (labels ((find-head-in (node)
	     (if (node-parents node)
		 (mapcar #'find-head-in (node-parents node))
		 node)))
    (remove-duplicates (flatten (find-head-in node)))))

(defun compose-code (node)
  "Make lisp form"
  (with-slots (name parents childs) node
    (if (string= name " ")               ; (child1 child2 ...)
	`(,@(mapcar #'compose-code
		    (setf childs
			  (sort childs #'< :key #'node-x))))
	(let ((symbol (read-from-string name)))
	  (cond
	    (childs                      ; (symbol child1 child2 ...)
	     `(,symbol
	       ,@(mapcar #'compose-code
			 (setf childs
			       (sort childs #'< :key #'node-x)))))
	    ((and (null parents)         ; (function-symbol)
		  (ignore-errors (symbol-function symbol)))
	     (list symbol))
	    (t                           ; symbol
	     symbol))))))

(defun eval-tree (node)
  (mapc (lambda (node)
	  (with-slots (message error) node
	    (let ((cant
		   (nth-value 1 (ignore-errors
				  (let ((result
					 (write-to-string
					  (eval (compose-code node)))))
				    (setf error nil)
				    (setf message result))))))
	      (when cant
		(setf error t)
		(setf message (substitute #\Space #\Linefeed
					  (princ-to-string cant)))))))
	(find-heads node))
  (mapc #'update-tree (find-heads node)))

;(eval (list (read-from-string "+") 
; 	     (read-from-string "2")
;	     (read-from-string "5")))

;;
;; main-screen routine
;;

(defmacro snap-to-grid (what)
  `(decf ,what
	 (- (mod (+ ,what 0.1) 0.2) 0.1)))

(defun set-mouse-position ()
  (setf *mouse-x* *position-x*
	*mouse-y* *position-y*))

(defun main-screen (delta)
  "Update and render"
  ; update
  (incf *time* delta)

  (when *key-move*
    (set-mouse-position))
  
  (when (and *mouse-left* (not *selector*))
    (dolist (node *selected-nodes*) 
      (incf (node-x node) (- *mouse-x* *selector-x*))
      (incf (node-y node) (- *mouse-y* *selector-y*)))
    (setf *selector-x* *mouse-x*
	  *selector-y* *mouse-y*))
  
  ; draw
  (gl:clear :color-buffer-bit)
  (gl:scale (incf *real-zoom* (lerp *zoom* *real-zoom* 0.3))
	    *real-zoom* 0)

  (when (not (or *mouse-right* *key-move*))
    (snap-to-grid *position-x*)
    (snap-to-grid *position-y*))

  (let ((slower 0.5))
    (gl:translate (- (incf *real-camera-x*
			   (lerp *camera-x*
				 *real-camera-x* slower)))
		  (- (incf *real-camera-y*
			   (lerp *camera-y*
				 *real-camera-y* slower)))
		  0))

  ; cross
  (gl:color 0.2 0.2 0.2)
  (simple-cross *position-x* *position-y* 0.1)

  ; *nodes*
  (mapc #'draw-wires *nodes*)
  (mapc #'draw-node *nodes-at-screen*)
  (when *selected-nodes* 
    (draw-selection (car *selected-nodes*) t)
    (mapc #'draw-selection (cdr *selected-nodes*)))

  ; selector
  (when *selector*
    (let* ((x (min *selector-x* *mouse-x*))
	   (y (min *selector-y* *mouse-y*))
	   (w (- (max *selector-x* *mouse-x*) x))
	   (h (- (max *selector-y* *mouse-y*) y)))
      (gl:color 0.5 0.7 1 0.3)
      (aligned-quad-shape x y 0 w h)
      (gl:color 0.5 0.7 1)
      (aligned-quad-lines x y 0 w h)))
  
  ; gui
  (gl:load-identity)
  (gl:color 1 1 1)
  (text (if (string= *new-node-name* "")
	    "enter symbol"
	    *new-node-name*)
	0 -0.8 0.03 0)
  (text "|" 0 -0.9 0.03 (* *time* 12))
  (text "|" 0 -0.9 0.03 (* *time* 42)))

(defun press-mouse-left ()
  (setf *mouse-left* t)
  (let ((select (find-if #'mouse-at-node-p *nodes-at-screen*)))
    (if select
	(progn
	  (if (find select *selected-nodes*)
	      (setf *selected-nodes*
		    (cons select (remove select *selected-nodes*)))
	      (setf *selected-nodes* (list select)))
	  (snap-to-grid *mouse-x*)
	  (snap-to-grid *mouse-y*))
	(setf *selector* t)))
  (setf *selector-x* *mouse-x*
	*selector-y* *mouse-y*))

(defun release-mouse-left ()
  (setf *mouse-left* nil)
  (when *selector*
    (setf *selector* nil)
    (let ((selected (remove-if-not
		     (lambda (node)
		       (node-in-rect node
				     *selector-x* *selector-y*
				     *mouse-x* *mouse-y*))
		     *nodes-at-screen*)))
      (setf *selected-nodes* (if selected selected ())))))

(defun press-mouse-right ()
  (setf *mouse-right* t)
  (let ((select (find-if #'mouse-at-node-p *nodes-at-screen*)))
    (when select
        (setf *selected-nodes*
	      (cons select (remove select *selected-nodes*))))))

;;
;; init and input setup
;;

(defun main()
  "Init all stuff and define events"
  (with-init (:everything)
    (sdl2-ttf:init)
    (sdl2:gl-set-attr :multisamplebuffers 1) 
    (sdl2:gl-set-attr :multisamplesamples 2) 
    (with-window (window :title "VLE"
			 :w *screen-width*
			 :h *screen-height*
			 :flags '(:shown :resizable :opengl))
      (with-gl-context (gl-context window)
	(gl-make-current window gl-context)	
	(gl:enable :texture-2d)
	(gl:enable :blend)
	(gl:blend-func :src-alpha :one-minus-src-alpha)
	(resize-viewport *screen-width* *screen-height*)
	(gl:clear-color 0.1 0.1 0.1 1)

	(repose)

	(with-event-loop (:method :poll)
	  (:textinput   (:text text)
			(ignore-errors 
			  (let ((char (code-char text)))
			    (setf *new-node-name*
				  (concatenate 'string
					       *new-node-name*
					       (list char))))))
	  (:keydown     (:keysym keysym)
			(case (scancode-symbol
			       (scancode-value keysym))
			  (:scancode-tab
			   (mapc #'eval-tree
				 *selected-nodes*))
			  (:scancode-return
			   (insert-new-node))
			  (:scancode-backspace
			   (when (not (string= *new-node-name* ""))
			     (setf *new-node-name*
				   (subseq *new-node-name* 0
					   (- (length *new-node-name*) 1)))))
			  (:scancode-delete
			   (mapc #'destroy-connections
				 *selected-nodes*)
			   (setf *nodes*
				 (set-difference *nodes*
						 *selected-nodes*)
				 *selected-nodes* ())
			   (repose))
			  (:scancode-kp-7
			   (connect-selected))
			  (:scancode-kp-1
			   (mapc #'destroy-connections
				 *selected-nodes*))
			  
			  (:scancode-lalt
			   (press-mouse-left))
			  (:scancode-lctrl
			   (press-mouse-right))
			  (:scancode-kp-4
			   (setf *key-move* t)
			   (incf *position-x* -0.2))
			  (:scancode-kp-6
			   (setf *key-move* t)
			   (incf *position-x* 0.2))
			  (:scancode-kp-8
			   (setf *key-move* t)
			   (incf *position-y* 0.2))
			  (:scancode-kp-2
			   (setf *key-move* t)
			   (incf *position-y* -0.2))
			  
			  (:scancode-f11
			   (full-screen window))))
	  (:keyup       (:keysym keysym)
			(case (scancode-symbol
			       (scancode-value keysym))
			  (:scancode-escape
			   (push-event :quit))
			  (:scancode-lalt
			   (release-mouse-left))
			  (:scancode-lctrl
			   (setf *mouse-right* nil))))
	  
	  (:mousemotion (:x x :y y :xrel xrel :yrel yrel)
			(setf *key-move* nil)
			(let ((asp (/ *screen-width* *screen-height*))
			      (half-width  (/ *screen-width*  2))
			      (half-height (/ *screen-height* 2)))
			  (setf *mouse-x*
				(float
				 (+ (* (/ (- x half-width)
					  half-width *zoom*)
				       asp)
				    *camera-x*))
				*mouse-y*
				(float
				 (+ (/ (- y half-height)
				       half-height *zoom* -1)
				    *camera-y*)))
			  (when (and *mouse-left* (not *selector*))
			    (snap-to-grid *mouse-x*)
			    (snap-to-grid *mouse-y*))
			  (when *mouse-right*
			    (incf *camera-x*
				  (/ xrel half-height *zoom* -1))
			    (incf *camera-y*
				  (/ yrel half-height *zoom*))
			    (repose))))
	  (:mousebuttondown (:button button)
			    (case button
			      (1 (press-mouse-left))
			      (2 (setf *position-x* *mouse-x*
				       *position-y* *mouse-y*)
				 (snap-to-grid *position-x*)
				 (snap-to-grid *position-y*))
			      (3 (press-mouse-right))))
	  (:mousebuttonup   (:button button)
			    (case button
			      (1 (release-mouse-left))
			      (3 (setf *mouse-right* nil))))
	  (:mousewheel      (:y y)
			    (setf *zoom* (max (min (+ *zoom* (* y 0.1))
						   2)
					      0.1))
			    (repose))
			     
	  (:idle        ()
			(idler window))
	  (:windowevent (:event event :data1 width :data2 height)
			(when (=
			       event
			       sdl2-ffi:+sdl-windowevent-size-changed+)
			  (setf *screen-width* width
				*screen-height* height)
			  (resize-viewport width height)
			  (repose)))
	  (:quit        ()
			(sdl2-ttf:quit)
			(clean-text-hash)
			(clean-texture-hash)
			t))))))

(main)

    
