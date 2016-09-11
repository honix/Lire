;;
;; Visual List Editor
;;

(ql:quickload '(:swank :cl-opengl :sdl2 :sdl2-ttf :sdl2-image))
(load (merge-pathnames (pathname "contrib/swank-fuzzy.lisp")
		       swank-loader:*source-directory*))

(defpackage :vle
  (:use :cl :sdl2))

(in-package :vle)

;;
;; communication
;;

(defun completion (string)
  (map 'list
       (lambda (n) (write-to-string (swank::fuzzy-matching.symbol n)))
       (let ((comps (sort
		     (swank::fuzzy-find-matching-symbols
		      string *package*)
		     #'> :key #'swank::fuzzy-matching.score)))
	 (subseq 
	  comps
	  0 (min 9 (length comps))))))

(defun e-eval (form)
  (print form)
  (eval form))

(defun function-symbol-p (symbol)
  (e-eval
   `(not (null (ignore-errors (symbol-function ',symbol))))))

;;
;; parameters
;;

;; main
(defparameter *time* 0.0)
(defparameter *delta* 0.0)
(defparameter *screen-width* 800)
(defparameter *screen-height* 600)
(defparameter *show-editor* t)
(defparameter *completions* nil)
(defparameter *completions-select* -1)
;; travel
(defparameter *position-x* 0)
(defparameter *position-y* 0)
(defparameter *camera-x* 0)
(defparameter *camera-y* 0)
(defparameter *real-camera-x* 0)
(defparameter *real-camera-y* 0)
(defparameter *zoom* 1.0)
(defparameter *real-zoom* 0.0)
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
;; modules
;;

(load "utils.lisp")

;;
;; basic node
;;

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
	(if (stringp (node-name node))
	    (+ (* (length (node-name node)) *node-width-char*)
	       *node-width-bumps*)
	    *node-height*)
	(node-color node)
	(if (ignore-errors (function-symbol-p
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
  (let ((node (make-node :name (cond ((string= name " ") :list)
				     ((string= name ".") :dot)
				     (t name))
			 :x x :y y)))
    (node-update node)))

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

(defmacro snap-to-grid (what)
  `(decf ,what
	 (- (mod (+ ,what 0.1) 0.2) 0.1)))

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
	(gl:color 1 1 1 0.5)
	(simple-line
	 (+ (node-x child) (* (- x (node-x child)) pulse-in))
	 (+ (node-y child) (* (- y (node-y child)) pulse-in))
	 (+ (node-x child) (* (- x (node-x child)) pulse-out))
	 (+ (node-y child) (* (- y (node-y child)) pulse-out)))))))

(defun draw-node (node)
  (with-slots (name x y width color
		    message error parents) node
    (when (stringp name)
      (apply #'gl:color color)
      (quad-shape x y 0 width *node-height*))
    (gl:color 1 1 1)
    (when (> *zoom* 0.3)
      (text (if (stringp name) name
		(case name (:list "(●)") (:dot "●") (t "?")))
	    x y 0.04 0)
      (when message
	(if error
	    (gl:color 1 1 0 0.5)	
	    (gl:color 0 1 1))
	(text message x (+ y 0.1) 0.04 0)))))

(defun draw-selection (node &optional first)
  (with-slots (x y width) node
    (let ((alpha (if *mouse-left* 0.5 1)))
      (if first
	  (gl:color 1 1 0 alpha)
	  (gl:color 0 1 1 alpha)))
    (let ((x x) (y y))
      (quad-lines (snap-to-grid x)
		  (snap-to-grid y) 0
		  (+ width         0.02)
		  (+ *node-height* 0.02)))))

(defun draw-args-list (node)
  (with-slots (name x y childs) node
    (gl:color 1 1 1 0.5)
    (let ((args (e-eval
		 `(or
		   #+sbcl(ignore-errors
			   (sb-impl::%fun-lambda-list
			    (macro-function (read-from-string ,name))))
		   #+sbcl(ignore-errors
			   (sb-impl::%fun-lambda-list
			    (symbol-function (read-from-string ,name))))
		   " "))))
      (text (princ-to-string args) x (- y 0.15) 0.04 0)
      (let ((count 0))
	(dolist (child childs) 
	  (text ;(princ-to-string (nth count args))
	   (princ-to-string count)
	   (node-x child) (+ (node-y child) 0.10) 0.03 0)
	  (incf count))))))


;;
;; nodes utils
;;

(defun point-at-node-p (node point-x point-y)
  (with-slots (x y width) node
    (let ((w width)
	  (h *node-height*))
      (and (< (- x w) point-x (+ x w))
	   (< (- y h) point-y (+ y h))))))

(defun position-at-node-p (node)
  (point-at-node-p node *position-x* *position-y*))

(defun mouse-at-node-p (node)
  (point-at-node-p node *mouse-x* *mouse-y*))

(defun sort-childs (node)
  "Sort childs for args mapping (func 0 1 2 3).
Horizontal (from left) is main axis, but if args-nodes 
horizontaly equal, sort it by vertical (from upper)"
  (with-slots (childs) node
    (setf childs
	  (sort childs (lambda (a b)
			 (if (< (- (node-x a) 0.01) 
			 	(node-x b) 
			 	(+ (node-x a) 0.01))
			     (> (node-y a) (node-y b))
			     (< (node-x a) (node-x b))))))))

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
	(setf (node-message head) "?")))
    (sort-childs parent)))

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

(defun delete-nodes (nodes-to-delete)
  (mapc #'destroy-connections
	*selected-nodes*)
  (setf *nodes*
	(set-difference *nodes*
		        nodes-to-delete)
	*selected-nodes* ())
  (repose))

(defun insert-new-node ()
  (if (string= *new-node-name* "")
      ; hit 'enter' with out symbol -> jump to last created node
      (let ((node (car *nodes*)))
	(when node
	  (setf *selected-nodes* (list (car *nodes*)))
	  (setf *position-y* (- (node-y node) 0.2))
	  (setf *position-x* (node-x node))))
      ; hit 'enter' with symbol
      (let ((under (find-if #'position-at-node-p *nodes-at-screen*)))
	(if under
	    ; new node overlaps olds -> replace it name!
	    (progn
	      (setf (node-name under) (string-upcase *new-node-name*))
	      (node-update under))
	    ; new node
	    (let ((new-node (create-node :name (string-upcase
						*new-node-name*)
					 :x *position-x*
					 :y *position-y*)))
	      (push new-node *nodes*)

	      (if *selected-nodes*
		  (progn
		    (make-connection (car *selected-nodes*) new-node)
		    (incf *position-x* 0.2))
		  (progn
		    (pushnew new-node *selected-nodes*)
		    (incf *position-y* -0.2)))))
	(setf *new-node-name* "")
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
    (if (stringp name)
	(let ((symbol (e-eval `(read-from-string ,name))))
	  (cond
	    (childs                      ; (symbol child1 child2 ...)
	     `(,symbol ,@(mapcar #'compose-code
				 (sort-childs node))))
	    ((and (null parents)         ; (function-symbol)
		  (function-symbol-p symbol))
	     (list symbol))
	    (t                           ; symbol
	     symbol)))
	(case name
	  (:list                         ; (child1 child2 ...)
	   `(,@(mapcar #'compose-code
		       (sort-childs node))))
	  (:dot                          ; child1
	   (compose-code (car childs)))))))


(defun eval-node (node)
  "Node must be head of tree"
  (with-slots (message error) node
    (setf message "...")
    (let* ((eva (multiple-value-list (e-eval
				      `(ignore-errors
					 ,(compose-code node)))))
	   (res (first  eva))
	   (err (second eva)))
      (if err
	  (progn
	    (setf error t)
	    (setf message (substitute #\Space #\Linefeed
				      (write-to-string err))))
	  (progn
	    (setf error nil)
	    (setf message (write-to-string res)))))))

(defun eval-node-threaded (node)
  (sb-thread:make-thread #'eval-node
			 :arguments (list node)))

(defun eval-tree (node)
  "Take some node from tree, find heads and evaluate"
  (let ((heads (if (listp node)
		   (remove-duplicates
		    (flatten (mapcar #'find-heads node)))
		   (find-heads node))))
    (mapc #'eval-node-threaded heads)
    (mapc #'update-tree heads)))

					;(eval (list (read-from-string "+") 
					; 	     (read-from-string "2")
					;	     (read-from-string "5")))

;;
;; main-screen routine
;;

(defun set-mouse-position ()
  (setf *mouse-x* *position-x*
	*mouse-y* *position-y*))

(defun draw ()) ; place-holder

(defun main-screen (delta)
  "Update and render"
					; update
  (incf *time* delta)

					; draw back render
  (gl:clear :color-buffer-bit)
  (ignore-errors (draw))

  (when (not *show-editor*)
    (return-from main-screen))
  
					; update vle
  (when *key-move*
    (set-mouse-position))
  
  (when (and *mouse-left* (not *selector*))
    (dolist (node *selected-nodes*) 
      (incf (node-x node) (- *mouse-x* *selector-x*))
      (incf (node-y node) (- *mouse-y* *selector-y*)))
    (setf *selector-x* *mouse-x*
	  *selector-y* *mouse-y*))

  (when (not (or *mouse-right* *key-move*))
    (snap-to-grid *position-x*)
    (snap-to-grid *position-y*))
  
					; draw vle
  (gl:load-identity)
  (gl:color 0 0 0 0.777)    ; transparent
  (quad-shape 0 0 0 10 1)   ; black window
  (gl:scale (incf *real-zoom* (lerp *zoom* *real-zoom* 0.3))
	    *real-zoom* 0)

  (let ((slower 0.5))
    (gl:translate (- (incf *real-camera-x*
			   (lerp *camera-x*
				 *real-camera-x* slower)))
		  (- (incf *real-camera-y*
			   (lerp *camera-y*
				 *real-camera-y* slower)))
		  0))

					; cross
  (let ((flicker (+ (abs (* (sin *time*) 0.6)) 0.2)))
    (gl:line-width 1)
    (gl:color 0 flicker flicker)
    (simple-cross *position-x* *position-y* 0.1))

					; *nodes*
  (gl:line-width (floor (max (* *zoom* 10) 1))) ;?
  (mapc #'draw-wires *nodes*)
  (gl:line-width 1)
  (mapc #'draw-node *nodes-at-screen*)
  (when *selected-nodes* 
    (draw-selection (car *selected-nodes*) t)
    (mapc #'draw-selection (cdr *selected-nodes*)))

					; arguments tip
  (let ((node (find-if #'mouse-at-node-p *nodes-at-screen*)))
    (when node
      (draw-args-list node)))

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

					; new node name
  (gl:color 1 1 1)
  (if (not (string= *new-node-name* ""))
      (text *new-node-name*
	    *position-x* *position-y* 0.035 0))
					; completion list
  (let ((count -1)
	(y (- *position-y* 0.05)))
    (dolist (comp *completions*)
      (if (= *completions-select* (incf count))
	  (gl:color 0 1 1 1.0)
	  (gl:color 1 1 1 0.1))
      (text comp *position-x* (decf y 0.050) 0.025 0)))
  
					; gui
  (gl:load-identity)
  (gl:color 1 1 1)
  (text "|" 0 -0.9 0.03 (* *time* 12))
  (text "|" 0 -0.9 0.03 (* *time* 42)))

(let ((last-click-time 0.0))
  (defun press-mouse-left ()
    (if (< (- (get-internal-real-time) last-click-time) 300)
	(progn                          ; double-click
	  (let ((select (find-if #'mouse-at-node-p *nodes-at-screen*)))
	    (when select (eval-tree select))))
	(progn                          ; single-click
	  (setf *mouse-left* t)
	  (let ((select (find-if #'mouse-at-node-p *nodes-at-screen*)))
	    (if select
		(progn
		  (if (find select *selected-nodes*)
		      (setf *selected-nodes*
			    (cons select (remove select
						 *selected-nodes*)))
		      (setf *selected-nodes* (list select))))
		(setf *selector* t)))
	  (setf *selector-x* *mouse-x*
		*selector-y* *mouse-y*)))
    (setf last-click-time (get-internal-real-time))))

(defun release-mouse-left ()
  (setf *mouse-left* nil)
  (dolist (selected *selected-nodes*)
    (with-slots (x y parents) selected
      (snap-to-grid x)
      (snap-to-grid y)
      (mapc #'sort-childs parents)))
  (when *selector*
    (setf *selector* nil)
    (let ((selected (remove-if-not
		     (lambda (node)
		       (node-in-rect node
				     *selector-x* *selector-y*
				     *mouse-x* *mouse-y*))
		     *nodes-at-screen*)))
      (if selected
	  (setf *selected-nodes* (sort selected #'> :key #'node-y))
	  (setf *selected-nodes* ()))))
  (setf *position-x* *mouse-x*
	*position-y* *mouse-y*)
  (snap-to-grid *position-x*)
  (snap-to-grid *position-y*))

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
    (with-window (vle-window :title "VLE"
			     :w *screen-width*
			     :h *screen-height*
			     :flags '(:shown :resizable :opengl))
      (with-gl-context (gl-context vle-window)
	(gl-make-current vle-window gl-context)
	(gl:enable :texture-2d)
	(gl:enable :blend)
	(gl:blend-func :src-alpha :one-minus-src-alpha)
	(resize-viewport *screen-width* *screen-height*)
	(gl:clear-color 0.5 0.5 0.5 1)

	(repose)

	(with-event-loop (:method :poll)
	  (:textinput   (:text text)
			(ignore-errors 
			  (let ((char (code-char text)))
			    (setf *new-node-name*
				  (concatenate 'string
					       *new-node-name*
					       (list char)))))
			(if (> (length *new-node-name*) 1)
			    (setf *completions*
				  (completion *new-node-name*))))
	  (:keydown     (:keysym keysym)
			(case (scancode-symbol
			       (scancode-value keysym))
			  (:scancode-tab
			   (eval-tree *selected-nodes*))
			  (:scancode-return
			   (when (> *completions-select* -1)
			     (setf *new-node-name*
				   (nth *completions-select*
					*completions*)))
			   (insert-new-node)
			   (setf *completions* ()
				 *completions-select* -1))
			  (:scancode-backspace
			   (when (> (length *new-node-name*) 0)
			     (setf *new-node-name*
				   (subseq
				    *new-node-name* 0
				    (- (length *new-node-name*) 1)))
			     (if (> (length *new-node-name*) 1)
				 (setf *completions*
				       (completion *new-node-name*))
				 (setf *completions* ()))))
			  (:scancode-delete
			   (delete-nodes *selected-nodes*))
			  (:scancode-kp-7
			   (connect-selected))
			  (:scancode-kp-1
			   (mapc #'destroy-connections
				 *selected-nodes*))
			  
			  (:scancode-lalt
			   (press-mouse-left))
			  (:scancode-lctrl
			   (press-mouse-right))
			  (:scancode-kp-3
			   (setf *show-editor* (not *show-editor*)))
			  (:scancode-kp-5
			   (setf *camera-x* *position-x*
				 *camera-y* *position-y*))
			  ((:scancode-kp-4 :scancode-left)
			   (setf *key-move* t)
			   (incf *position-x* -0.2))
			  ((:scancode-kp-6 :scancode-right)
			   (setf *key-move* t)
			   (incf *position-x* 0.2))
			  ((:scancode-kp-8 :scancode-up) 
			   (if (not (string= *new-node-name* ""))
			       (decf *completions-select*)
			       (progn
				 (setf *key-move* t)
				 (incf *position-y* 0.2))))
			  ((:scancode-kp-2 :scancode-down)
			   (if (not (string= *new-node-name* ""))
			       (incf *completions-select*)
			       (progn
				 (setf *key-move* t)
				 (incf *position-y* -0.2))))
			  
			  (:scancode-f11
			   (full-screen vle-window))))
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
					;(when (and *mouse-left* (not *selector*))
					;  (snap-to-grid *mouse-x*)
					;  (snap-to-grid *mouse-y*))
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
			(idler vle-window))
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


(sb-thread:make-thread #'main)
