;;
;; Lisp server launching for distant repl
;; (mostly copied from lime example by eudoxia0)
;;

(setf sb-impl::*default-external-format* :utf-8)

(ql:quickload :swank)

(defparameter *port* 15001)

#| migrated to client side
(defun read-symbols (form)
  (cond ((atom form) (read-from-string form))
	((listp form) (mapcar #'read-symbols form))))

(defun read-symbols-and-eval (form)
  (eval (read-symbols form)))
|# 

(setf swank:*configure-emacs-indentation* nil)
(let ((swank::*loopback-interface* (uiop:hostname)))
  (swank:create-server :port *port* :dont-close t))

(loop (sleep 60)) ; a way to stay alive?
