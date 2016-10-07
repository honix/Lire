;;
;; Client connection setup
;;

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

(defun connect-to-server ()
  (format t "Connection")
  (setf *swank-connection*
	(lime:make-connection (uiop:hostname) *port*))
  (let ((counter 0))
    (loop
       (when (> counter 2)
	 (format t "Time-out~%")
	 (return))
       (when (ignore-errors (lime:connect *swank-connection*))
	 (format t "Done!~%")
	 (return))
       (sleep 0.01)
       (incf counter 0.01)
       (format t "."))))

(connect-to-server)

(defparameter *last-log* "...")

(defun logger (direction string)
  (setf *last-log* (format nil "@~A <> ~A~&" direction string))
  (with-open-file (s "send-eval-log.log"
		     :direction :output
		     :if-does-not-exist :create
		     :if-exists :append)
    (format s "@~A <> ~A~&" direction string)))

;; Utils
(defun read-symbols (form)
  "From ('defun' 'func' ('x' 'y')) to (defun func (x y))"
  (cond ((atom form) (if (stringp form) (read-from-string form) form))
	((listp form) (mapcar #'read-symbols form))))

(defun send-eval (form &optional uroboros counter)
  (when (> (lime:connection-debug-level *swank-connection*) 0)
    (logger "debug!"
	    (lime:connection-debug-level *swank-connection*))
    (lime:abort-debugger *swank-connection*)
    (return-from send-eval))
  (lime:abort-debugger *swank-connection*)
  (lime:pull-all-events *swank-connection*) ; make empty
  (let ((message (prin1-to-string (read-symbols form))))
    ; log out every message
    (logger "OUT" message)
    (lime:evaluate *swank-connection* message))
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
