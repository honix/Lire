;;
;; Visual List Editor - evaluation
;;

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
	(let ((symbol (e-eval `(read-from-string ,name))))
	  (cond
	    (childs                      ; (symbol child1 child2 ...)
	     `(,symbol ,@(mapcar #'compose-code
				 (sort-childs node))))
	    ((and (null parents)         ; (function-symbol)
		  (function-symbol-p symbol))
	     (list symbol))
	    (t                           ; symbol
	     symbol)))
	(case name
	  (:list                         ; (child1 child2 ...)
	   `(,@(mapcar #'compose-code
		       (sort-childs node))))
	  (:dot                          ; child1
	   (compose-code (car childs)))))))


(defun eval-node (node)
  "Node must be head of tree"
  (with-slots (message error) node
    (setf message "...")
    (let* ((eva (multiple-value-list (e-eval
				      `(ignore-errors
					 ,(compose-code node)))))
	   (res (first  eva))
	   (err (second eva)))
      (if err
	  (progn
	    (setf error t)
	    (setf message (substitute #\Space #\Linefeed
				      (write-to-string err))))
	  (progn
	    (setf error nil)
	    (setf message (write-to-string res)))))))

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
