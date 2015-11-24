(asdf:load-system :trivial-shell)
(asdf:load-system :split-sequence)

(load "filter.lisp")
(load "utilities.lisp")

(defun extract-filename (string)
  (to-string (butlast (to-list (apply #'concatenate 'string
				      (mapcar (lambda (str)
						(strcat str " ")) (nthcdr 8
									  (remove "" (split-sequence:split-sequence #\space string) :test #'string=))))))))

(defun generate-print-name (filename type)
  (strcat filename (case type
		     (:file "")
		     (:directory "/"))))

(defun ls-line-to-file (line)
  (let ((name (extract-filename line))
	(type (case (schar line 0)
		(#\- :file)
		(#\d :directory)
		(#\p :fifo)
		(#\c :char-device)
		(#\l :link)
		(#\b :block-device))))
    (list :type type
	  :name name
	  :clean-name (filename-clean name)
	  :print-name (generate-print-name name type))))

(defun ls (path)
  "takes a string as a path and returns a list of plists, with each plist representing a file and containing a filename and file type"
  (let* ((raw-output (split-sequence:split-sequence #\newline (trivial-shell:shell-command (format nil "ls -lA \"~A\"" path)))))
    (mapcar #'ls-line-to-file (subseq raw-output 1 (1- (length raw-output))))))

(defun cd (path cwd)
  "modifies a special variable that corresponds to the current path"
  (to-string (butlast (to-list (trivial-shell:shell-command (strcat "cd \"" cwd "\";cd \"" path "\";pwd"))))))
