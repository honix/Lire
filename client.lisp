;;
;; Client connection setup
;;

(in-package :VLE)

(ql:quickload '(:lime :bordeaux-threads))

(defparameter *port* 15001)

;; Wake server
(format t "Run server side!~%")
(bordeaux-threads:make-thread
 (lambda ()
   (uiop:run-program "sbcl --load server.lisp")))
;; closing process then?

;; Connect
(defparameter *swank-connection* nil)
(defparameter *lisp-dialog* (format nil "LISP-DIALOG~%"))

(defun log-message (string)
  (setf *lisp-dialog* (concatenate 'string *lisp-dialog* string)))

(defun log-demon ()
  (bordeaux-threads:make-thread
   (lambda ()
     (loop
	(sleep 0.3)
	(let ((s ""))
	  (dolist (e (ignore-errors (lime:pull-all-events
				     *swank-connection*)))
	    (setf s (concatenate 'string s 
				 (typecase e
				   (lime:write-string-event
				    (lime:event-string e))
				   (lime:debugger-event
				    (format nil "!debugger say smthng~%"))
				   (t
				    (format nil "!unknow event~%"))))))
	  (log-message s))))))

(defun connect-to-server (&optional silent)
  (format t "Connection")
  (setf *swank-connection*
	(lime:make-connection (uiop:hostname) *port*))
  (let ((counter 0))
    (loop
       (when (ignore-errors (lime:connect *swank-connection*))
	 (unless silent
	   (log-message (format nil "Done!~%")))
	 (log-demon)
	 (return))
       (when (> counter 2)
	 (unless silent
	   (log-message (format nil "Time-out~%")))
	 (return))
       (sleep 0.01)
       (incf counter 0.01)
       (unless silent
	 (log-message (format nil "."))))))

;; Evaluating
(defun read-symbols (form)
  "From ('defun' 'func' ('x' 'y')) to (defun func (x y))"
  (cond ((atom form) (if (stringp form) (read-from-string form) form))
	((listp form) (mapcar #'read-symbols form))))

(defun send-eval (form)
  (connect-to-server :silent)
  (lime:evaluate *swank-connection*
		 (prin1-to-string (read-symbols form))))

(connect-to-server)

#|
  (let ((counter (or counter 0)))
    (loop
       (when (and (not uroboros) (> counter 1))
	 ; reconnect
	 (connect-to-server)
	 (return (send-eval form)))
       (sleep 0.03)
       (incf counter 0.03)
       (let ((answer (car (lime:pull-all-events *swank-connection*))))
	 (typecase answer
	   (lime:write-string-event
	    (let ((as-string (slot-value answer 'string)))
	      (logger "IN" as-string)
	      (return (values as-string counter))))
	   (lime:debugger-event
	    (return :error)))))))
|#
