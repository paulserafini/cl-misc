(defun massoc (key list)
  "Return the elements of an association list which share a common key."
  (remove-if-not (lambda (i) (equal (car i) key)) list))

(defun whitespace-p (string)
  "Evaluate whether a string is whitespace."
  (or (equal string #\Space)
      (equal string #\Tab)))

(defun collect-letters (string)
  "Extract non-whitespace characters into an association list, the keys
   of which are a whitespace-delimited enumeration."
  (loop
     for char across string
     with i = 0
     if (whitespace-p char) do (incf i)
     else collect (list i char)))

(defun split-by-whitespace (string)
  "Convert a whitespace delimited string to a list of strings."

  (setf letters (collect-letters string))

  (setf indices (remove-duplicates (mapcar #'first letters)))

  (loop for i in indices collect
       (coerce (mapcar #'cadr (massoc i letters)) 'string)))

(string-to-list "1990/1/1                   $200.0 = $500.0")
