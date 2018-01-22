;;
;; Lire - utils
;;

(in-package :lire)

(defun path (file-name)
  (merge-pathnames (concatenate 'string "media/" file-name)))

;;;
;;; evaly
;;;

(defun e-eval (form &optional echo)
  (when echo (print form))
  (ignore-errors (eval form)))

(defun function-symbol-p (symbol)
  (e-eval
   `(not (null (ignore-errors (symbol-function ',symbol))))))

;;;
;;; math
;;;

(defun lerp (from to amount)
  (* (- from to)
     amount))

(defmacro snap-to-grid (what)
  `(decf ,what
         (- (mod (+ ,what 0.1) 0.2) 0.1)))

;;;
;;; color
;;;

(defun hsv-to-rgb (h s v)
  (let* ((c (* v s))
         (x (* c (- 1 (abs (- (mod (/ h 60) 2) 1)))))
         (m (- v c))
         (rgb-n (cond ((<= 0   h  60) (list c x 0))
                      ((<= 60  h 120) (list x c 0))
                      ((<= 120 h 180) (list 0 c x))
                      ((<= 180 h 240) (list 0 x c))
                      ((<= 240 h 300) (list x 0 c))
                      ((<= 300 h 360) (list c 0 x)))))
    (mapcar (lambda (x) (+ x m)) rgb-n)))

;;;
;;; lists
;;;

(defun flatten (l &key (test #'atom))
  (cond ((null l) nil)
        ((funcall test l) (list l))
        (t (loop for a in l nconc (flatten a :test test)))))

;;;
;;; textures hashing and loading
;;;

(defstruct text-texture
  id width heigth)

(defparameter *text-hash* (make-hash-table :test #'equal))

(defun clean-text-hash ()
  (labels ((delete-texture (key value)
             (declare (ignore key))
             (gl:delete-textures (list (text-texture-id value)))))
    (maphash #'delete-texture *text-hash*)
    (clrhash *text-hash*)))

(defun make-text (string)
  (or (gethash string *text-hash*)
      (let* ((font         (sdl2-ttf:open-font
                            (path "saxmono.ttf") 30))
             (texture      (car (gl:gen-textures 1)))
             (surface      (sdl2-ttf:render-utf8-blended
                            font string 255 255 255 0))
             (surface-w    (surface-width surface))
             (surface-h    (surface-height surface))
             (surface-data (surface-pixels surface)))
        (gl:bind-texture  :texture-2d texture)
        (gl:tex-parameter :texture-2d :texture-min-filter :linear)
        (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
        (gl:tex-image-2d  :texture-2d 0 :rgba surface-w surface-h 0
                          :rgba :unsigned-byte surface-data)
        (free-surface surface)
        (sdl2-ttf:close-font font)
        (setf (gethash string *text-hash*)
              (make-text-texture :id texture
                                 :width (/ surface-w 30)
                                 :heigth (/ surface-h 30 -1))))))


(defparameter *texture-hash* (make-hash-table :test #'equal))

(defun clean-texture-hash ()
  (labels ((delete-texture (key value)
             (declare (ignore key))
             (gl:delete-textures (list value))))
    (maphash #'delete-texture *texture-hash*)
    (clrhash *texture-hash*)))

(defun load-texture (file-name &optional (format :rgba)
                                 (filter :linear))
  (or (gethash file-name *texture-hash*)
      (let* ((texture      (car (gl:gen-textures 1)))
             (surface      (sdl2-image:load-image (path file-name)))
             (surface-w    (surface-width surface))
             (surface-h    (surface-height surface))
             (surface-data (surface-pixels surface)))
        (gl:bind-texture  :texture-2d texture)
        (gl:tex-parameter :texture-2d :texture-min-filter filter)
        (gl:tex-parameter :texture-2d :texture-mag-filter filter)
        (gl:tex-image-2d  :texture-2d 0 :rgba surface-w surface-h 0
                          format ; :bgr - bmp  :rgba - png
                          :unsigned-byte surface-data)
        (free-surface surface)
        (setf (gethash file-name *texture-hash*) texture))))

;;;
;;; drawing
;;;

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
  (gl:vertex  0  1))

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

(defun text (string x y size rotation)
  (let ((text-texture (make-text string)))
    (with-slots (id width heigth) text-texture
      (quad-textured id x y
                     rotation (* width size) (* heigth size)))))

(defun text-align (string x y size rotation)
  (let ((text-texture (make-text string)))
    (with-slots (id width heigth) text-texture
      (quad-textured id (+ x (* width size)) y
                     rotation (* width size) (* heigth size)))))


;;;
;;; window-utils
;;;

(defun resize-viewport (width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (let ((asp (/ width height)))
    (gl:ortho (- asp) asp -1 1 0 1))
  (gl:translate 0 0 -1)
  (gl:matrix-mode :modelview))

(let ((fullscreen nil))
  (defun full-screen (window)
    (multiple-value-bind (some width height)
        (get-current-display-mode 0)
      (declare (ignore some))
      (set-window-size window width height))
    (set-window-fullscreen
     window
     (setf fullscreen
           (not fullscreen)))))
