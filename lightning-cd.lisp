(asdf:load-system :split-sequence)
(asdf:load-system :trivial-shell)
(asdf:load-system :cl-termbox)

(declaim (optimize (debug 2)))
(declaim (optimize (speed 0)))

(defun to-list (item)
  (coerce item 'list))

(defun to-string (item)
  (coerce item 'string))

(defun strcat (first &rest others)
  (apply #'concatenate 'string first others))

(defun extract-filename (string)
  (to-string (butlast (to-list (apply #'concatenate 'string
				      (mapcar (lambda (str)
						(strcat str " ")) (nthcdr 8
									  (remove "" (split-sequence:split-sequence #\space string) :test #'string=))))))))

(defparameter *show-deselected-files* nil)
(defparameter *default-mode* :search)
(defparameter *editor* "nvim")
(defparameter *lightning-path-file* "/home/grant/Ramdisk/.lightningpath")
(defparameter *current-directory* nil)

(defun get-char-range ()
  "get a string of the characters that are valid to enter in search mode"
  (let ((chars (list #\.)))
    (dotimes (i 255)
      (if (or (and (>= i (char-code #\a)) (<= i (char-code #\z)))
	      (and (>= i (char-code #\0)) (<= i (char-code #\9))))
	  (nconc chars (list (code-char i)))))
    chars))

(defun filename-clean (filename)
  "convert a raw filename to a simplified one that can be searched for"
  (let ((acceptable-chars (get-char-range)))
    (to-string (remove-if-not (lambda (item)
				(member item acceptable-chars))
			      (to-list (string-downcase filename))))))

(defun ls (&optional (path *current-directory*))
  "takes a string as a path and returns a list of lists, with each sublist representing a file and containing a filename and if the file is a directory"
  (let* ((raw-files (butlast (rest (butlast (split-sequence:split-sequence #\newline (trivial-shell:shell-command (format nil "ls -lA \"~A\"" path)))))))
	 (files (mapcar (lambda (raw)
			  (let ((name (extract-filename raw)))
			    (list :type (case (first (to-list (subseq raw 0 1)))
					  (#\- :file)
					  (#\d :directory)
					  (#\p :fifo)
					  (#\c :char-device)
					  (#\l :link)
					  (#\b :block-device))
				  :name name
				  :clean-name (filename-clean name)))) raw-files)))
    files))

(defun cd (path)
  "modifies a special variable that corresponds to the current path"
  (setf *current-directory* (to-string (butlast (to-list (trivial-shell:shell-command (strcat "cd \"" *current-directory* "\";cd \"" path "\";pwd")))))))

(cd ".")

(defun pwd ()
  *current-directory*)

(defun write-text (x y text-string &optional (fg-bg (cons termbox:+default+ termbox:+default+)))
  "execute a series of change-cell's in a sequential manner such as to write a line of text"
  (let ((text (to-list text-string)))
    (dotimes (i (length text))
      (termbox:change-cell (+ x i) y (char-code (nth i (to-list text))) (car fg-bg) (cdr fg-bg)))))

(defun select-files-in-search-buffer (all-files search-buffer)
  "return a list of selected files by comparing simplified filenames with the search buffer"
  (remove-if-not (lambda (file)
		   (string= search-buffer (subseq (filename-clean file) 0 (min (length search-buffer) (length file)))))
		 (mapcar (lambda (file-object)
			   (getf file-object :clean-name)) all-files)))

(defun get-file-colors (mode selected-files selected-index this-file file-list)
  "return a cons cell containing the foreground and background colors for the given file"
  (cond
    ((and (eq mode :search) (member (getf this-file :clean-name) selected-files :test #'string=) *show-deselected-files*)
     (cons termbox:+black+ termbox:+white+))
    ((and (eq mode :normal) (equal this-file (nth selected-index file-list)))
     (cons termbox:+black+ termbox:+white+))
    (t
     (cons 0 0))))

(defun show-this-file-p (this-file selected-files)
  (or *show-deselected-files* (member (getf this-file :clean-name) selected-files :test #'string=) (null selected-files)))

(defun write-path (filename path)
  (with-open-file (out filename
		       :direction :output
		       :if-exists :supersede)
    (with-standard-io-syntax
      (print path out))))

(defun draw-file-list (ystart yend mode selected-files selected-index file-list)
  "draw the list of selected file-list onto the screen"
  (let ((x 0)
	(y ystart)
	(width (apply #'max (mapcar (lambda (f)
				      (length (coerce f 'list)))
				    (or selected-files (mapcar (lambda (x) (getf x :name)) file-list))))))
    (dolist (f file-list)
      (if (= y yend)
	  (setf y ystart
		x (+ x width)))
      (when (show-this-file-p f selected-files)
	
	(write-text x y (if (eq (getf f :type) :directory)
			    (strcat (getf f :name) "/")
			    (getf f :name))
		    (get-file-colors mode selected-files selected-index (nth selected-index file-list) file-list))
	(incf y)))))

(defun switch-mode (prev-mode selected selected-files files)
  "switch the mode to either search or normal and do associated setup for each mode"
  (let ((new-mode nil))
    (if (eq prev-mode :search)
	(setf selected (if (plusp (length selected-files))
			   (position (nth 0 selected-files) files)
			   0)
	      new-mode :normal)
	(setf new-mode :search))
    (list new-mode selected)))

(defun open-file-with-command (path command)
  "write the current path, close Lightning, and execute the command"
  (termbox:shutdown)
  (write-path (to-string *lightning-path-file*) path)
  (trivial-shell:shell-command command)
  (exit))

(defun action (file path)
  "do something with a filename that the user selected"
  (case (getf file :type)
    (:file
     (open-file-with-command path (concatenate 'string *editor* " \"" *current-directory* (getf file :name) "\"")))
    (:directory
     (cd (getf file :name)))))

(defun lightning ()
  (let ((mode *default-mode*)
	(selected-files ())
	(search-buffer ())
	(selected-index 0)
	(all-files ())
	(char-range (get-char-range)))
    (termbox:init)
    (loop
	; if files is nil, then we've nuked the buffer because of a cd or something; time to regenerate!
       (or all-files (setf all-files (sort (ls *current-directory*) (lambda (x y)
							    (string< (getf x :name) (getf y :name))))))

	; if we're in search mode, only show files that match the buffer
       (and (eq mode :search) search-buffer
	    (setf selected-files (select-files-in-search-buffer all-files search-buffer)))

       (termbox:clear)

	; draw the current mode, current directory, file list, and search buffer, respectively
       (write-text 0 0 (strcat (string mode) ": " *current-directory*))
       (draw-file-list 1 (1- (termbox:height)) mode selected-files selected-index all-files)
       (if (eq mode :search)
	   (write-text 0 (1- (termbox:height)) search-buffer))
       (termbox:present)

	; get and process input
	; if we're in search mode with only one selected file, then open it
       (if (and (eq mode :search) (= (length selected-files) 1) (plusp (length search-buffer)))
	   (progn
	     (action (nth (position (nth 0 selected-files) all-files :test (lambda (x y)
									     (string= x (getf y :clean-name)))) all-files) *current-directory*)
	     (setf mode *default-mode*
		   all-files ()
		   selected-files ()
		   selected-index 0
		   search-buffer ()))
	   (let ((event (termbox:poll-event)))
	     (let* ((event-data (termbox:event-data event))
		    (letter (code-char (getf event-data :ch)))
		    (keycode (getf event-data :key)))
	       (termbox:free-event event)
	       (if (eq (getf event-data :type) termbox:+event-key+)
		   (cond
		     ((eq keycode termbox:+key-space+)
		      (let ((result (switch-mode mode selected-index selected-files all-files)))
			(setf mode (nth 0 result)
			      selected-index (nth 1 result)
			      selected-files ()
			      search-buffer ())))
		     ((not (equal letter #\Null))
		      (cond
			((eq letter #\,)
			 (cd "..")
			 (setf search-buffer ()
			       all-files ()
			       selected-index 0))
			((eq letter #\;)
			 (open-file-with-command *current-directory* "true"))
					; switch modes
			
					; quit to the current directory
					; normal-mode-specific commands
			((eq mode :normal)
			 (cond
					; move up one item
			   ((eq letter #\k)
			    (setf selected-index (mod (1- selected-index) (length all-files))))
					; move down one item
			   ((eq letter #\j)
			    (setf selected-index (mod (1+ selected-index) (length all-files))))
					; open the current item
			   ((eq letter #\')
			    (action (nth selected-index all-files) *current-directory*)
			    (setf mode *default-mode*
				  selected-files ()
				  selected-index 0
				  search-buffer ()))
			   ((eq letter #\v)
			    (open-file-with-command *current-directory* (strcat *editor* " " (nth selected-index all-files))))))
			((eq mode :search)
			 (cond
			   (letter
			    (cond
			      ((member letter char-range)
			       (setf search-buffer (strcat search-buffer (to-string (list letter)))))
			      ((eq letter #\-)
			       (if (plusp (length (to-list search-buffer)))
				   (setf search-buffer (to-string (butlast (to-list search-buffer))))))
			      ((eq letter #\')
			       (action (nth 0 selected-files) *current-directory*))))))))))))))))

(lightning)
