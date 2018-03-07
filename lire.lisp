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
  (:shadowing-import-from :cl-glut width height)
  (:use :cl))

(in-package :lire)


;;; Bad things here

(defmacro canvas ()
  '(first (slot-value *lire* 'childs)))

(defmacro nodes ()
  '(slot-value (canvas) 'nodes))

;;; end


(dolist (file '("settings"
                "utils"
                "shapes"
                "text"
                "node"
                "new-node"
                "evaluation"
                "writer"
                "file-dialog"
                "widget"
                "canvas"
                "menu"
                "window"))
  (load file))

(defparameter *lire* (make-instance 'lire-window))

(defparameter *lire-ui-thread*
  (bordeaux-threads:make-thread
   (lambda () (glut:display-window *lire*))
   :name "Lire-UI-thread"))
