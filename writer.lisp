;;
;; Lire - write lire code from list of nodes
;;

(in-package :lire)

(defmethod compose-list ((head node))
  (with-slots (name childs) head
    (if childs
        (cons name (mapcar #'compose-list childs))
        name)))

(defmethod compose-poses ((head node))
  (with-slots (x y childs) head
    (let ((pos (cons x y)))
      (if childs
          (cons pos (mapcar #'compose-poses childs))
          pos))))

(defmethod list-and-poses ((head node))
  (list
   :code  (compose-list  head)
   :poses (compose-poses head)))

(defun write-lire (nodes)
  (let ((heads (heads nodes)))
    (write-to-string
     (mapcar #'list-and-poses heads)
     :pretty t)))

(defmacro nodes ()
  "Ugly pump"
  '(slot-value (first (slot-value *lire* 'childs)) 'nodes))

(defun write-lire-to-file (path)
  (format t "~%Writing ~s" path)
  (handler-case
      (with-open-file (out path
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
        (princ (write-lire (nodes)) out))
    (condition (e) (format t "~%Error while writing: ~a" e))))

(defun load-lire-from-file (path)
  (format t "~%Loading ~s" path)
  (handler-case
      (with-open-file (in path)
        (let ((code-and-poses (read in)))
          (print code-and-poses)))
    (condition (e) (format t "~%Error while loading: ~a" e))))
