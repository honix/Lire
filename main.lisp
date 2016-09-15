;;
;; Visual List Editor - main
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
	 (subseq comps 0 (min 9 (length comps))))))

(defun e-eval (form)
  (print form)
  (eval form))

(defun function-symbol-p (symbol)
  (e-eval
   `(not (null (ignore-errors (symbol-function ',symbol))))))

;;
;; parameters
;;

;; timing
(defparameter *time* 0.0)
(defparameter *delta* 0.0)
(defparameter *second-time* 0.0)
(defparameter *temp-fps* 0)
(defparameter *fps* 0)
;; screen
(defparameter *screen-width* 800)
(defparameter *screen-height* 600)
(defparameter *show-editor* t)
;; completions
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
;; macros
;;

(defmacro snap-to-grid (what)
  `(decf ,what
	 (- (mod (+ ,what 0.1) 0.2) 0.1)))

;;
;; modules
;;

(load "utils.lisp")
(load "node.lisp")
(load "evaluation.lisp")

;;
;; main-screen routine
;;

(defun set-mouse-position ()
  (setf *mouse-x* *position-x*
	*mouse-y* *position-y*))

(defun draw ()) ; place-holder

(defun main-screen (delta)
  "Update and render"
					; timing and fps count
  (incf *time* delta)
  (if (< *second-time* 1.0)
      (progn
	(incf *second-time* delta)
	(incf *temp-fps*))
      (progn
	(setf *second-time* 0.0
	      *fps*         *temp-fps*
	      *temp-fps*    0)))

					; draw back render
  (gl:clear :color-buffer-bit)
  (gl:disable :blend)
  (ignore-errors (draw))
  (gl:enable :blend)

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
  (gl:line-width (floor (max (* *zoom* 10) 1)))
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

					; new node
  (when (not (string= *new-node-name* ""))
    (let ((node (create-node :name *new-node-name*
			     :x *position-x*
			     :y *position-y*)))
      (setf (node-color node) (list 0.2 0.2 0.2))
      (draw-node node)))
  
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
  (gl:color 1 1 1 0.5)
  (text (format nil "fps: ~A" *fps*) -0.5 -0.9 0.03 0)
  (text (princ-to-string *package*) 0.5 -0.9 0.03 0)
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
				   (string-downcase
				    (nth *completions-select*
					 *completions*))))
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
