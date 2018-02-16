;;
;; Lire - evaluation
;;

(in-package :lire)

(defmethod compose-code ((node node))
  "Make lisp form"
  (with-slots (name parents childs) node
    (if (stringp name)
                                        ; ~symbols
        (let ((symbol (e-eval `(read-from-string ,name))))
          (if (function-symbol-p symbol)
                                        ; -> (symbol child1 child2 ...)
              `(,symbol ,@(mapcar #'compose-code (sort-childs node)))
                                        ; -> symbol
              symbol))
                                        ; ~specials
        (case name
          (:list                        ; -> (child1 child2 ...)
           (mapcar #'compose-code (sort-childs node)))
          (:dot                         ; -> child1 child2 ...
           (if (cdr childs)
                                        ; multiple link
               `(values ,@(mapcar #'compose-code (sort-childs node)))
                                        ; short link
               (compose-code (car childs))))))))

(defmethod eval-node ((node node))
  (with-slots (message error name) node
    (setf message "...")
    (let* ((code   (compose-code node))
           (eval   (multiple-value-list
                    (e-eval code :echo)))
           (result  (first eval))
           (e-error (second eval)))
      (if (typep e-error 'error)
          (progn
            (setf error t)
            (setf message (write-to-string e-error :length 16)))
          (progn
            (setf error nil)
            (setf message
                  (write-to-string result :length 16)))))))

(defmethod eval-node-threaded ((node node))
  (bordeaux-threads:make-thread (lambda () (eval-node node))))

(defun heads (nodes)
  (if (listp nodes)
      (remove-duplicates
       (flatten (mapcar #'find-heads nodes)))
      (find-heads nodes)))

(defun eval-tree (nodes)
  (let ((heads (heads nodes)))
    (mapc #'eval-node-threaded heads)
    (mapc #'update-tree heads)))

;(eval (list (read-from-string "+") 
;            (read-from-string "2")
;            (read-from-string "5")))
