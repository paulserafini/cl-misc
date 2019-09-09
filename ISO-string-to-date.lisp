(defvar *digits*
  "A list of digits 0 through 1."
  (loop
     with zero = (char-code #\0)
     for i below 10
     collect (code-char (+ zero i))))

(defun all-digits (string)
  "Verify that every character in a string is a digit."
  (loop
     for i across string
     unless (member i *digits*) do (return nil)
     finally (return T)))

(defun collect-chars (string seperator)
  "Extract non-whitespace characters into an association list, the keys
   of which are a whitespace-delimited enumeration."
  (loop
     for char across string
     with i = 0
     if (equal seperator char) do (incf i)
     else collect (cons i char)))

(defun split-string (string seperator)
  "Convert a whitespace delimited string to a list of strings."
  (let* ((chars (collect-chars string seperator))
         (indices (remove-duplicates (mapcar #'first chars))))
    (loop for i in indices collect
         (coerce (mapcar #'cdr (massoc i chars)) 'string))))

(defun is-x-between-y-and-z (x y z)
  "Evaluate whether x is between y and z (inclusive)."
  (and (numberp x)
       (numberp y)
       (numberp z)
       (<= y x)
       (>= z x)))

(defun is-x-divisible-by-y (x y)
  "Evaluate whether x is evenly divisible by y."
  (and (numberp x)
       (numberp y)
       (= (rem x y) 0)))

(defun leapyearp (year)
  "Determine whether a year is a leapyear."
  (or (and (is-x-divisible-by-y year 4)
	   (not (is-x-divisible-by-y year 100)))
      (is-x-divisible-by-y year 400)))

(defun days-per-month (year)
  "Return a list containing the number of days in each month
   for a specified year."
  (if (leapyearp year)
      '(31 29 31 30 31 30 31 31 30 31 30 31)
      '(31 28 31 30 31 30 31 31 30 31 30 31)))

(defun valid-date (date)
  "Check whether a date object specified a valid date."

  (let ((year (getf date :year))
	(month (getf date :month))
	(day (getf date :day)))

    (and (integerp year)
	 (integerp month)
	 (is-x-between-y-and-z month 1 12)
	 (integerp day)
	 (is-x-between-y-and-z day 1 (nth (1- month) (days-per-month year)))
	 date)))

(defun ISO-string-to-date (string)
  "Convert an ISO formatted date from a string to a
   list with a 'year', 'month', and 'day' element."

  (let* ((contents (split-string string #\-))
	 (length (length contents))
	 (year (first contents))
	 (month (second contents))
	 (day (third contents)))

    (if (and (= length 3)
	     (all-digits year)
	     (all-digits month)
	     (all-digits day))
	(valid-date (list :year (parse-integer year)
			  :month (parse-integer month)
			  :day (parse-integer day))))))

(datep "1990-01-21")
