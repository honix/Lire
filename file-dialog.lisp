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
             (:load
              "zenity --file-selection --title=Load")
             (:save
              "zenity --file-selection --title=Save --save --confirm-overwrite")
             (t (error "Use :load or :save key")))))
      (uiop:run-program command :output out)
      (string-trim '(#\NewLine) (get-output-stream-string out)))
    #+windows
    (print "Windows dialog not implemented yet")))

;; be aware if user has not install zenity or smth

(defun save-lire ()
  (let ((path (file-dialog :save)))
    (write-lire-to-file path)))

(defun load-lire ()
  (let ((path (file-dialog :load)))
    (load-lire-from-file path)))
