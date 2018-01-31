;;
;; Lire - low-level drawing
;;

(in-package :lire)

(defmacro define-textured (fname &body body)
  `(defun ,fname (texture x y rotation scale-x scale-y)
     (gl:bind-texture :texture-2d texture)
     (gl:push-matrix)
     (gl:translate x y 0)
     (gl:rotate rotation 0 0 1)
     (gl:scale scale-x scale-y 1)
     ,@body
     (gl:end)
     (gl:pop-matrix)))

(defmacro define-shape (fname &body body)
  `(defun ,fname (x y rotation scale-x scale-y)
     (gl:bind-texture :texture-2d 0)
     (gl:push-matrix)
     (gl:translate x y 0)
     (gl:rotate rotation 0 0 1)
     (gl:scale scale-x scale-y 1)
     ,@body
     (gl:end)
     (gl:pop-matrix)))

(define-textured quad-textured
  (gl:begin :triangle-fan)
  (gl:tex-coord 0 0) (gl:vertex -1 -1)
  (gl:tex-coord 1 0) (gl:vertex  1 -1)
  (gl:tex-coord 1 1) (gl:vertex  1  1)
  (gl:tex-coord 0 1) (gl:vertex -1  1))

(define-shape quad-shape
  (gl:begin :triangle-fan)
  (gl:vertex -1 -1)
  (gl:vertex  1 -1)
  (gl:vertex  1  1)
  (gl:vertex -1  1))

(define-shape aligned-quad-shape
  (gl:begin :triangle-fan)
  (gl:vertex  0  0)
  (gl:vertex  1  0)
  (gl:vertex  1  1)
  (gl:vertex  0  1))

(define-shape quad-lines
  (gl:begin :line-loop)
  (gl:vertex -1 -1)
  (gl:vertex  1 -1)
  (gl:vertex  1  1)
  (gl:vertex -1  1))

(define-shape aligned-quad-lines
  (gl:begin :line-loop)
  (gl:vertex  0  0)
  (gl:vertex  1  0)
  (gl:vertex  1  1)
  (gl:vertex 0 1))

(defun simple-line (x1 y1 x2 y2)
  (gl:bind-texture :texture-2d 0)
  (gl:begin :lines)
  (gl:vertex x1 y1)
  (gl:vertex x2 y2)
  (gl:end))

(defun simple-cross (x y size)
  (simple-line (- x size) y
               (+ x size) y)
  (simple-line x (- y size)
               x (+ y size)))
