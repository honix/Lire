;;
;; Visual List Editor - node
;;

(in-package :vle)

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
	    (gl:color 0 1 1 0.5))
	(text message x (+ y 0.1) 0.03 0)))))

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

(let ((last-node nil)
      (last-value " "))
  (defun args-list (node)
    "Returns string with function or macro arguments"
    (if (equal node last-node)
	last-value
	(setf last-node node
	      last-value 
	      (let* ((name (node-name node))
		     (args (e-eval
			    `(or
			      #+sbcl(ignore-errors
				      (sb-impl::%fun-lambda-list
				       (macro-function
					(read-from-string ,name))))
			      #+sbcl(ignore-errors
				      (sb-impl::%fun-lambda-list
				       (symbol-function
					(read-from-string ,name))))
			      " "))))
		(princ-to-string args))))))
		      
(defun draw-args-list (node)
  (with-slots (x y childs) node
    (gl:color 1 1 1 0.5)
    (text (args-list node) x (- y 0.15) 0.04 0)
    ; childs numbering
    (let ((count 0))
      (dolist (child childs) 
	(text ;(princ-to-string (nth count args))
	 (princ-to-string count)
	 (node-x child) (+ (node-y child) 0.10) 0.03 0)
	(incf count)))))

;;
;; nodes utils
;;

(defun snap-node-to-grid (node)
  (snap-to-grid (node-x node))
  (snap-to-grid (node-y node)))

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

(defun flip-connection (parent child)
  (if (find child (node-childs parent))
      (setf (node-childs parent) (remove child (node-childs parent))
	    (node-parents child) (remove parent (node-parents child)))
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
	(sort-childs parent))))

(defun connect-selected ()
  (let ((parent (car *selected-nodes*))
	(others (cdr *selected-nodes*)))
    (mapc (lambda (node) (flip-connection parent node))
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
	      (setf (node-name under)
		    (cond ((string= *new-node-name* " ") :list)
			  ((string= *new-node-name* ".") :dot)
			  (t *new-node-name*)))
	      (node-update under))
	    ; new node
	    (let ((new-node (create-node :name *new-node-name*
					 :x *position-x*
					 :y *position-y*)))
	      (push new-node *nodes*)

	      (if *selected-nodes*
		  (progn
		    (flip-connection (car *selected-nodes*) new-node)
		    (incf *position-x* 0.2))
		  (progn
		    (pushnew new-node *selected-nodes*)
		    (incf *position-y* -0.2)))))
	(setf *new-node-name* "")
	(repose))))
