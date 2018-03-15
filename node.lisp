;;
;; Lire - node
;;

(in-package :lire)

(defclass node ()
  ((name           :initarg :name :initform "")
   (x :type fixnum :initarg :x    :initform 0)
   (y :type fixnum :initarg :y    :initform 0)
   
   (width   :initform 0)
   (color   :initform '(0 0 0))
   
   (message :initform nil)
   (error   :initform nil)
   
   (parents :initform ())
   (childs  :initform ())))

(defmethod node-update ((node node))
  (with-slots (width name color) node
    (setf name
          (cond ((string= name " ") :list)
                ((string= name ".") :dot)
                (t name))
          width
          (if (stringp name)
              (+ (* (length name) *node-width-char*)
                 *node-width-bumps*)
              *node-height*)
          color
          (if (ignore-errors (function-symbol-p
                              (read-from-string name)))
              (hsv-to-rgb (mod (sxhash name) 360)
                          0.75 0.5)
              (hsv-to-rgb 0 0 0.3))))
  node)

(defmethod update-tree ((node node))
  (with-slots (childs) node
    (node-update node)
    (when childs
      (mapc #'update-tree childs))))

(defun create-node (&key name x y)
  (let ((node (make-instance 'node
                             :name name
                             :x x :y y)))
    (node-update node)))

(defmethod node-in-rect ((node node) x1 y1 x2 y2)
  (let ((x1 (min x1 x2))
        (y1 (min y1 y2))
        (x2 (max x1 x2))
        (y2 (max y1 y2)))
    (with-slots (x y) node
      (and
       (< x1 x x2)
       (< y1 y y2)))))


;;;
;;  Node drawing functions
;;;

(defmethod draw-wires ((node node) wire-width)
  (with-slots (x y color childs) node
    (dolist (child childs)
      (with-slots ((cx x) (cy y)) child
        (apply #'gl:color *dimm-color*)
        (gl:line-width wire-width)
        (simple-line x y cx cy)
        (apply #'gl:color color)
        (let* ((dx (- x cx))
               (dy (- y cy))
               (gap       0.5))
          ;; (gl:line-width (/ wire-width 3))
          (simple-line
           x y
           (+ cx (* dx gap))
           (+ cy (* dy gap))))))))

(defmethod draw-selection ((node node) &optional first)
  (with-slots (x y width) node
    (if first
        (apply #'gl:color *warn-color*)
        (apply #'gl:color *normal-color*))
    (let ((x x) (y y))
      (gl:line-width 1)
      (quad-lines (snap-to-grid x)
                  (snap-to-grid y) 0
                  (+ width         3)
                  (+ *node-height* 3)))))

(let ((last-node nil)
      (last-value " "))
  (defmethod args-list ((node node))
    (if (equal node last-node)
        last-value
        (setf last-node node
              last-value 
              (let* ((name (slot-value node 'name))
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

(defmethod draw-args-list ((node node))
  (with-slots (x y name childs) node
    (apply #'gl:color *dimm-color*)
    (if (stringp name)
        (text (args-list node) x (+ y (* *node-height* 2))
              (* *node-text-height* 0.7) 0))
                                        ; childs numbering
    (let ((count 0))
      (dolist (child childs)
        (with-slots ((cx x) (cy y)) child
          (text (princ-to-string count)
                cx (- cy (* *node-height* 2))
                (* *node-text-height* 0.7) 0)
          (incf count))))))

;;;
;;  Node utils
;;;

(defmethod snap-node-to-grid ((node node))
  (with-slots (x y) node
    (snap-to-grid x)
    (snap-to-grid y)))

(defmethod point-at-node-p ((node node) point-x point-y)
  (with-slots (x y width) node
    (let ((w width)
          (h *node-height*))
      (and (< (- x w) point-x (+ x w))
           (< (- y h) point-y (+ y h))))))


(defmethod sort-childs ((node node))
  "Sort childs for args mapping (func 0 1 2 3).
Horizontal (from left) is main axis, but if args-nodes 
horizontaly equal, sort it by vertical (from upper)"
  (with-slots (childs) node
    (setf childs
          (sort childs
                (lambda (a b)
                  (with-slots ((ax x) (ay y)) a
                    (with-slots ((bx x) (by y)) b
                      (if (< (- ax 0.01) bx (+ ax 0.01))
                          (< ay by)
                          (< ax bx)))))))))

(defparameter *fragment* nil)

(defmethod find-heads ((node node))
  "Find all tree-heads linked to node"
  (labels ((find-head-in (node &optional (depth 0))
             (when (> depth 1024)
               (error "Can't find some heads: looping structures are not allowed"))
             (with-slots (parents) node
               (let ((l-parents (if *fragment*
                                    (intersection
                                     parents
                                     (slot-value (canvas) 'selected-nodes))
                                    parents)))
                 (if l-parents
                     (mapcar (lambda (p) (find-head-in p (1+ depth))) l-parents)
                     node)))))
    (remove-duplicates (flatten (find-head-in node)))))

(defmethod send-message-to-heads ((node node) message)
  (dolist (head (find-heads node))
    (with-slots (message) head
      (when message (setf message "?")))))

(defmethod flip-connection ((parent node) (child node))
  (with-slots ((parent-childs childs)) parent
    (with-slots ((child-parents parents)) child
      (if (find child parent-childs)
          (progn
            (setf parent-childs (delete child parent-childs)
                  child-parents (delete parent child-parents)))
          (when (not (eq parent child))
            (pushnew child parent-childs)
            (pushnew parent child-parents)
            (with-slots (message) child
              (when message (setf message nil)))
            (handler-case
                (send-message-to-heads child "?")
              (condition (c) ; infinity structure catch
                (format t "~a~%" c)
                (pop parent-childs)
                (pop child-parents)))
            (sort-childs parent))))))

(defmethod destroy-connections ((node node))
  (with-slots (childs parents) node
    (dolist (child childs)
      (with-slots (parents) child
        (setf parents (delete node parents))))
    (dolist (parent parents)
      (with-slots (childs) parent
        (setf childs (delete node childs))))
    (setf childs  ()
          parents ())))


(defmethod draw-node ((node node) show-name)
  (with-slots (name x y width color message error parents) node
    (when (stringp name)
      (apply #'gl:color color)
      (quad-shape x y 0 width *node-height*))
    (apply #'gl:color (if (stringp name) '(1 1 1 1) *alt-color*))
    (when show-name
      (text (case name (:list "(●)") (:dot "●") (t name))
            x y *node-text-height* 0)
      (when message
        (if error
            (apply #'gl:color *warn-color*)
            (apply #'gl:color *normal-color*))
        (text message x (- y (* *node-height* 2)) *node-text-height* 0)))))
