;;
;; Lire - evaluation
;;

(in-package :lire)

(defmethod compose-code ((node node))
  "Make lisp form"
  (with-slots (name parents childs) node
    (if (stringp name)
                                        ; symbols
        (let ((symbol (e-eval `(read-from-string ,name))))
          (cond
            (childs                      ; (symbol child1 child2 ...)
             `(,symbol ,@(mapcar #'compose-code (sort-childs node))))
            ((and (null parents)         ; (function-symbol)
                  (function-symbol-p symbol))
             `(,symbol))
            (t                           ; symbol
             symbol)))
                                        ; specials
        (case name
          (:list                         ; (child1 child2 ...)
           (mapcar #'compose-code
                   (sort-childs node)))
          (:dot                          ; child1 child2 ...
           (if (null (cdr childs))
               (compose-code (car childs)) ; short link
               `(values                    ; multiple link
                 ,@(mapcar #'compose-code
                           (sort-childs node)))))))))

(defmethod eval-node ((node node))
  (with-slots (message error name) node
    (setf message "...")
    (let* ((cod (compose-code node))
           (eva (multiple-value-list
                 (e-eval cod :echo)))
           (res (first  eva))
           (err (second eva)))
      (if (typep err 'error)
          (progn
            (setf error t)
            (setf message (write-to-string err :length 16)))
          (progn
            (setf error nil)
            (setf message
                  (write-to-string (if (eq name :dot)
                                       cod
                                       res)
                                   :length 16)))))))

(defmethod eval-node-threaded ((node node))
  (bordeaux-threads:make-thread (lambda () (eval-node node))))

(defun eval-tree (nodes)
  (let ((heads (if (listp nodes)
                   (remove-duplicates
                    (flatten (mapcar #'find-heads nodes)))
                   (find-heads nodes))))
    (mapc #'eval-node-threaded heads)
    (mapc #'update-tree heads)))

;(eval (list (read-from-string "+") 
;            (read-from-string "2")
;            (read-from-string "5")))
