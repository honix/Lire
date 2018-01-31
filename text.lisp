;;
;; Lire - text drawing
;;

(in-package :lire)

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
      (let* ((font         (sdl2-ttf:open-font "media/saxmono.ttf" 30))
             (texture      (car (gl:gen-textures 1)))
             (surface      (sdl2-ttf:render-utf8-blended
                            font string 255 255 255 0))
             (surface-w    (sdl2:surface-width surface))
             (surface-h    (sdl2:surface-height surface))
             (surface-data (sdl2:surface-pixels surface)))
        (gl:bind-texture  :texture-2d texture)
        (gl:tex-parameter :texture-2d :texture-min-filter :linear)
        (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
        (gl:tex-image-2d  :texture-2d 0 :rgba surface-w surface-h 0
                          :rgba :unsigned-byte surface-data)
        (sdl2:free-surface surface)
        (sdl2-ttf:close-font font)
        (setf (gethash string *text-hash*)
              (make-text-texture :id texture
                                 :width (/ surface-w 30)
                                 :heigth (/ surface-h 30 -1))))))

(defun text (string x y size rotation)
  (let ((text-texture (make-text string)))
    (with-slots (id width heigth) text-texture
      (quad-textured id x y
                     rotation (* width size) (* heigth size -1)))))

(defun text-align (string x y size rotation)
  (let ((text-texture (make-text string)))
    (with-slots (id width heigth) text-texture
      (quad-textured id (+ x (* width size)) y
                     rotation (* width size) (* heigth size -1)))))
