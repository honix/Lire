;;
;; Lire - preview of the newely created node
;;

(in-package :lire)

(defclass new-node (node)
  ((completions        :initform ())
   (completions-select :initform  -1)))


(defmethod is-empty-p ((new-node new-node))
  (with-slots (name) new-node
    (if (typep name 'string)
        (= (length name) 0)
        (null name))))

;;;
;;  Symbol finder
;;;

(defun completion (string)
  (map 'list
       (lambda (n) (write-to-string (swank::fuzzy-matching.symbol n)))
       (let ((comps (sort
                     (swank::fuzzy-find-matching-symbols string *package*)
                     #'> :key #'swank::fuzzy-matching.score)))
         (subseq comps 0 (min (length comps) 9)))))

(defmethod update-completions ((new-node new-node))
  (with-slots (completions completions-select name) new-node
    (if (and (not (is-empty-p new-node)) (typep name 'string))
        (setf completions (completion name))
        (setf completions ()
              completions-select -1))))

;;;
;;  Inputs
;;;

(defmethod select ((new-node new-node) direction)
  (with-slots (completions-select) new-node
    (incf completions-select direction)))

(defmethod accept ((new-node new-node))
  (with-slots (name completions completions-select) new-node
    (when (> completions-select -1)
      (setf name (string-downcase
                  (nth completions-select
                       completions)))
      (node-update new-node))))

(defmethod keyboard ((new-node new-node) key)
  (with-slots (name) new-node
    (when (typep name 'string)
      (setf name (concatenate 'string name (list key))))
    (node-update new-node)
    (update-completions new-node)))

(defmethod backspace ((new-node new-node))
  (with-slots (name) new-node
    (if (typep name 'string)
        (when (> (length name) 0)
          (setf name (subseq name 0 (- (length name) 1))))
        (setf name ""))
    (node-update new-node)
    (update-completions new-node)))

;;;
;;
;;;

(defmethod clear ((new-node new-node))
  (with-slots (name) new-node
    (setf name "")
    (node-update new-node)
    (update-completions new-node)))

(defmethod produce-node ((new-node new-node))
  (with-slots (name x y) new-node
    (let ((node (create-node :name name :x x :y y)))
      (clear new-node)
      node)))

;;;
;;  Draw ;)
;;;

(defmethod draw-node ((new-node new-node) show-name)
  (call-next-method new-node show-name)
  (with-slots (x y width completions completions-select) new-node
    ;; border
    (apply #'gl:color *normal-color*)
    (let ((x x) (y y))
      (gl:line-width 2)
      (quad-lines (snap-to-grid x)
                  (snap-to-grid y) 0
                  width
                  *node-height*))
    ;; completion list
    (let ((count -1)
          (y (+ y (* *node-height* 1.5))))
      (dolist (comp completions)
        (if (= completions-select (incf count))
            (apply #'gl:color *normal-color*)
            (apply #'gl:color *dimm-color*))
        (text comp x (incf y (* *node-height* 1.1))
              *node-text-height* 0)))))
