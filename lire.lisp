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

(defparameter *lire* nil)
(defparameter *lire-ui-thread* nil)

(defun run-lire (&key (threaded t))
  (format t "Running lire..~%")
  (setf *lire*
        (make-instance 'lire-window)
        *lire-ui-thread*
        (if threaded
            (bordeaux-threads:make-thread
                (lambda () (glut:display-window *lire*))
                :name "Lire-UI-thread")
            (sb-thread:main-thread))) ; hmm-umm
  (unless threaded (glut:display-window *lire*)))

(run-lire)
; (save-lisp)

