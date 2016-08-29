(ql:quickload :swank-client)

;; crazy stuff. in future there will be no packages
(defpackage :vle
  (:use :cl))
(in-package :vle)

(defvar *swank-client-port* 10000)

(defun start-swank-server-for-swank-client (port)
  "Starts a Swank server thread, listening on PORT of the host's network
interface, to handle Swank Client connection requests."
  (let ((swank::*loopback-interface* (sb-unix:unix-gethostname)))
    (swank:create-server :port port :dont-close t)))

(defun swank-thread ()
  "Returns a thread that's acting as a Swank server."
  (dolist (thread (sb-thread:list-all-threads))
    (when (com.google.base:prefixp "Swank" (sb-thread:thread-name thread))
      (return thread))))

(defun wait-for-swank-thread ()
  "Wait for the Swank server thread to exit."
  (let ((swank-thread (swank-thread)))
    (when swank-thread
      (sb-thread:join-thread swank-thread))))

(defun main ()
  (setf swank:*configure-emacs-indentation* nil
        swank::*enable-event-history* nil
        swank:*log-events* t)
  (ignore-errors
    (start-swank-server-for-swank-client *swank-client-port*))
  (wait-for-swank-thread))

(main)
