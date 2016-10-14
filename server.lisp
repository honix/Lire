;;
;; Visual List Editor - Lisp server launching for distant repl
;;

(setf sb-impl::*default-external-format* :utf-8)

(ql:quickload :swank)

(defparameter *port* 1501)

(setf swank:*configure-emacs-indentation* nil)
(let ((swank::*loopback-interface* (uiop:hostname)))
  (swank:create-server :port *port* :dont-close t))

(loop (sleep 60)) ; a way to stay alive?
