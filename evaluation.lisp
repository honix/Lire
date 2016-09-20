;;
;; Visual List Editor - evaluation
;;

(in-package :vle)

(defun find-heads (node)
  "Find all tree-heads linked to node"
  (labels ((find-head-in (node)
	     (if (node-parents node)
		 (mapcar #'find-head-in (node-parents node))
		 node)))
    (remove-duplicates (flatten (find-head-in node)))))

(defun compose-code (node)
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

(defun eval-node (node)
  "Node must be head of tree"
  (with-slots (message error) node
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
		  (write-to-string (if (eq (node-name node) :dot)
				       cod
				       res)
				   :length 16)))))))

(defun eval-node-threaded (node)
  (sb-thread:make-thread #'eval-node
			 :arguments (list node)))

(defun eval-tree (node)
  "Take some node from tree, find heads and evaluate"
  (let ((heads (if (listp node)
		   (remove-duplicates
		    (flatten (mapcar #'find-heads node)))
		   (find-heads node))))
    (mapc #'eval-node-threaded heads)
    (mapc #'update-tree heads)))

;(eval (list (read-from-string "+") 
; 	     (read-from-string "2")
;	     (read-from-string "5")))
