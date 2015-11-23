(defun read-string-from-file (filename)
  (with-open-file (in filename
		      :direction :input
		      :if-exists :supersede)
    (with-standard-io-syntax
      (read-line in))))

(defun write-data (filename data)
  (with-open-file (out filename
		       :direction :output
		       :if-exists :supersede)
    (with-standard-io-syntax
      (print data out))))

(defun write-string-to-file (filename string)
  (with-open-file (out filename
		       :direction :output
		       :if-exists :supersede)
    (with-standard-io-syntax
      (format out "~A" string))))

(defun mapgetf (files key)
  (mapcar (lambda (f)
	    (getf f key)) files))


(defun to-list (item)
  (coerce item 'list))

(defun to-string (item)
  (coerce item 'string))

(defun strcat (first &rest others)
  (apply #'concatenate 'string first others))
