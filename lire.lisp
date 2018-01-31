;;
;; Lire - launch
;;

(setf sb-impl::*default-external-format* :utf-8) ; can be userful with Windows


(ql:quickload '(:swank :bordeaux-threads
                :cl-opengl :cl-glut
                :sdl2-ttf))

(load (merge-pathnames (pathname "contrib/swank-fuzzy.lisp")
                       swank-loader:*source-directory*))


(defpackage :lire
  (:use :cl))

(in-package :lire)

(dolist (file '("utils.lisp"
                "shapes.lisp"
                "text.lisp"
                "node.lisp"
                "evaluation.lisp"
                "canvas.lisp"
                "window.lisp"))
  (load file))

(defparameter *lire* (make-instance 'lire-window))

(bordeaux-threads:make-thread
 (lambda () (glut:display-window *lire*)))
