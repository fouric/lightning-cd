(asdf:load-system :trivial-shell)
(asdf:load-system :split-sequence)

(load "filter.lisp")

(defun extract-filename (string)
  (to-string (butlast (to-list (apply #'concatenate 'string
				      (mapcar (lambda (str)
						(strcat str " ")) (nthcdr 8
									  (remove "" (split-sequence:split-sequence #\space string) :test #'string=))))))))

(defun generate-print-name (filename type)
  (strcat filename (case type
		     (:file "")
		     (:directory "/"))))

(defun ls (path)
  "takes a string as a path and returns a list of plists, with each plist representing a file and containing a filename and file type"
  (let* ((raw-output (split-sequence:split-sequence #\newline (trivial-shell:shell-command (format nil "ls -lA \"~A\"" path))))
	 (raw-files (subseq raw-output 1 (- (length raw-output) 2))))
    (mapcar (lambda (raw)
	      (let ((name (extract-filename raw))
		    (type (case (schar raw 0)
			    (#\- :file)
			    (#\d :directory)
			    (#\p :fifo)
			    (#\c :char-device)
			    (#\l :link)
			    (#\b :block-device))))
		(list :type type
		      :name name
		      :clean-name (filename-clean name)
		      :print-name (generate-print-name name type)))) raw-files)))

(defun cd (path cwd)
  "modifies a special variable that corresponds to the current path"
  (to-string (butlast (to-list (trivial-shell:shell-command (strcat "cd \"" cwd "\";cd \"" path "\";pwd"))))))
