;;
;; Visual List Editor - Client connection setup
;;

(in-package :VLE)

(ql:quickload '(:lime :bordeaux-threads))

(defparameter *port* 1501)

;; Wake server
(format t "Run server side!~%")
(bordeaux-threads:make-thread
 (lambda ()
   (uiop:run-program "sbcl --load server.lisp")))

;; Connect
(defparameter *swank-connection* nil)
(defparameter *lisp-dialog* (format nil "LISP-DIALOG~%"))

(defun log-message (string)
  (setf *lisp-dialog* (concatenate 'string *lisp-dialog* string)))

(defun connect-to-server (&optional silent)
  (unless silent
    (log-message (format nil "Connection")))
  (setf *swank-connection*
	(lime:make-connection (uiop:hostname) *port*))
  (let ((counter 0))
    (loop
       (when (ignore-errors (lime:connect *swank-connection*))
	 (unless silent
	   (log-message (format nil "Done!~%")))
	 (return))
       (when (> counter 2)
	 (unless silent
	   (log-message (format nil "Time-out~%")))
	 (return))
       (sleep 0.5)
       (incf counter 0.5)
       (unless silent
	 (log-message (format nil "."))))))

(connect-to-server)

;; Evaluating
(defun send-eval (form)
  (connect-to-server :silent) ; re-enable connection
  (lime:evaluate *swank-connection*
		 (princ-to-string form)))

(defparameter *log-thread*
  (bordeaux-threads:make-thread
   (lambda ()
     (loop
	(sleep 0.3)
	(let ((s ""))
	  (dolist (e (ignore-errors (lime:pull-all-events
				     *swank-connection*)))
	    (setf s (concatenate
		     'string s 
		     (typecase e
		       (lime:write-string-event
			(lime:event-string e))
		       ;(lime:debugger-event
			;(format nil "!debugger-event~%"))
		       ;(lime:switch-package-event
			;(format nil "!package-event~%"))
		       (t
			(format nil "!~A~%" e))))))
	  (log-message s))))))
