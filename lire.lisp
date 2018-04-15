;;
;; Lire - launch
;;

(setf sb-impl::*default-external-format* :utf-8) ; can be userful with Windows


(ql:quickload '(:swank :bordeaux-threads
                :cl-opengl :cl-glut
                :sdl2-ttf
                :cl-anonfun))

(load (merge-pathnames (pathname "contrib/swank-fuzzy.lisp")
                       swank-loader:*source-directory*))


(defpackage :lire-user
  (:use :cl :anonfun))

(defpackage :lire
  (:shadowing-import-from :cl-glut width height)
  (:use :cl))

(in-package :lire)

(defparameter *lire* nil)
(defparameter *lire-ui-thread* nil)

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
                "window"                
                "menu"))
  (load file))

(defun start-lire ()
  (in-package :lire-user)
  (glut:display-window *lire*))

(defun run-lire (&key (threaded t))
  (format t "Running lire..~%")
  (setf *lire* (make-instance 'lire-window)
        *lire-ui-thread* (if threaded
                             (bordeaux-threads:make-thread
                              #'start-lire
                              :name "Lire-UI-thread")
                             (and (start-lire)
                                  (sb-thread:main-thread)))))

(run-lire)
; (save-lisp)

