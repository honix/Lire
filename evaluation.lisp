;;
;; Lire - evaluation
;;

(in-package :lire)

(defmethod compose-code ((node node))
  "Make lisp form"
  (let ((args))
    (labels
        ((compose (node)
             (with-slots (name parents childs) node
               (if (stringp name)
                                        ; ~symbols
                   (let ((symbol (e-eval `(read-from-string ,name))))
                     (if childs
                                        ; -> (symbol child1 child2 ...)
                         `(,symbol ,@(mapcar #'compose (sort-childs node)))
                                        ; -> symbol
                         symbol))
                                        ; ~specials
                   (case name
                     (:list                        ; -> (child1 child2 ...)
                      (mapcar #'compose (sort-childs node)))
                     (:dot                         ; -> child1 child2 ...
                      (if childs
                          (if (cdr childs)
                                        ; multiple link
                              `(values ,@(mapcar #'compose (sort-childs node)))
                                        ; short link
                              (compose (car childs)))
                          (let ((arg (read-from-string (format nil "ARG-~a-~a"
                                                               (/ (slot-value node 'x) *grid-size*)
                                                               (/ (slot-value node 'y) *grid-size*)))))
                            (pushnew arg args)
                            arg))))))))
      ;; actually, args will be sorted by x position
      (list (compose node) (reverse args)))))

(defmethod eval-node ((node node))
  (with-slots (message error name x y) node
    (setf message "...")
    (let* ((comp   (compose-code node))
           (code   (first comp))
           (args   (second comp)))
      (let* ((eval   (multiple-value-list
                      (e-eval (if args
                                  `(defun ,(read-from-string
                                            (format nil "FUN-~a-~a"
                                                    (/ x *grid-size*)
                                                    (/ y *grid-size*)))
                                       ,args ,code)
                                  code)
                              :echo)))
             (result  (first eval))
             (e-error (second eval)))
        (if (typep e-error 'error)
            (progn
              (setf error t)
              (setf message (write-to-string e-error :length 16)))
            (progn
              (setf error nil)
              (setf message
                    (write-to-string result :length 16))))))))

(defmethod eval-node-threaded ((node node))
  (bordeaux-threads:make-thread
   (lambda ()
     (eval-node node)
     (bt:interrupt-thread *lire-ui-thread* #'glut:post-redisplay))))

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
