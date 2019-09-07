(defun find-x-in-y (x y)
  "Return the index of the first occurrence of X in Y."
  (loop
     for i in y
     for j from 0 to (length y)
     do (if (equal i x) (return j))))

(defun ensure-list (x)
  "If X is not a list or is nil, return it in a list."
  (if (listp x)
      (or x (list x))
      (list x)))

;; (apply 'append '((a b c) nil (x y z) nil))
(defun nappend (&rest lists)
  "Merge multiple lists."
  (loop for list in lists append
       (ensure-list list)))


(defun evaluate-boolean-expression (expression)

  ;; Convert the expression to a list of operators and Boolean objects
  (if (stringp expression)
      (setf expression (read-from-string expression)))

  ;; Evaluate inner parentheses first
  (dotimes (i (length expression))
    (if (listp (nth i expression))
  	(setf (nth i expression)
  	      (evaluate-boolean-expression (nth i expression)))))

  ;; Evaluate not statements until there are no more
  (loop
     while (find-x-in-y 'not expression)
     do
       (setf i (find-x-in-y 'not expression))
       (setf expression (nappend (subseq expression 0 i)
       				 (not (nth (+ i 1) expression))
       				 (nthcdr (+ i 2) expression))))

  ;; Evaluate and statements until there are no more
  (loop
     while (find-x-in-y 'and expression)
     do
       (setf i (find-x-in-y 'and expression))
       (setf expression (nappend (subseq expression 0 (1- i))
  				 (and (nth (1- i) expression)
  				      (nth (1+ i) expression))
  				 (nthcdr (+ i 2) expression))))

  ;; Evaluate or statements until there are no more
  (loop
     while (find-x-in-y 'or expression)
     do
       (setf i (find-x-in-y 'or expression))
       (setf expression (nappend (subseq expression 0 (1- i))
  				 (or (nth (1- i) expression)
  				     (nth (1+ i) expression))
  				 (nthcdr (+ i 2) expression))))

  ;; Need to take the second element since there will be a NIL at either end
  (second expression))


(setf string "(t or nil)")
(evaluate-boolean-expression string)
