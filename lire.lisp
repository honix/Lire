;;
;; Lire - launch
;;

(setf sb-impl::*default-external-format* :utf-8) ; can be userful with Windows


(ql:quickload '(:swank :bordeaux-threads
                :cl-opengl :sdl2
                :sdl2-ttf :sdl2-image))

(load (merge-pathnames (pathname "contrib/swank-fuzzy.lisp")
                       swank-loader:*source-directory*))


(defpackage :lire
  (:use :cl :sdl2))

(in-package :lire)

(defparameter *window* nil)

(dolist (module '("utils.lisp"
                  "node.lisp"
                  "evaluation.lisp"
                  "window.lisp"))
  (load module))

(bordeaux-threads:make-thread #'run-lire)
