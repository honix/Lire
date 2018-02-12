;;
;; Lire - functions to invoke UI for loading and saving
;;

(in-package :lire)

(defun file-dialog (operation)
  (handler-bind
      ((condition (lambda (e)
                    (case (slot-value e 'uiop/run-program::code)
                      (1 (invoke-restart 'continue)) ; unexpected exit
                      (t (error e)))))) ; something serious
    #+linux
    (let ((out (make-string-output-stream))
          (command
           (case operation
             (:load "zenity --file-selection")
             (:save "zenity --file-selection --save --confirm-overwrite")
             (t (error "Use :load or :save key")))))
      (uiop:run-program command :output out)
      (get-output-stream-string out))
    #+windows
    (print "Windows dialog not implemented yet")))

(defun file-dialog-threaded (operation)
  (bordeaux-threads:make-thread (lambda () (file-dialog operation))))

;; be aware if user has not install zenity or smth

(defun save-lire ()
  (file-dialog-threaded :save))

(defun load-lire ()
  (file-dialog-threaded :load))
