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
    (let ((out (make-string-output-stream))
          (command
           #+linux
            (case operation
              (:load "./filedialog/linux/opendialog")
              (:save "./filedialog/linux/savedialog"))
            #+windows
            (case operation
              (:load ".\\filedialog\\windows\\opendialog.exe")
              (:save ".\\filedialog\\windows\\savedialog.exe"))))
      (uiop:run-program command :output out)
      (string-trim '(#\Return #\Linefeed) (get-output-stream-string out)))))


(defun save-lire ()
  (let ((path (file-dialog :save)))
    (write-lire-to-file path)))

(defun load-lire ()
  (let ((path (file-dialog :load)))
    (load-lire-from-file path)))
