(asdf:load-system :split-sequence)
(asdf:load-system :trivial-shell)
(asdf:load-system :cl-termbox)

(load "utilities.lisp")

(declaim (optimize (debug 2)))
(declaim (optimize (speed 0)))

(defun to-list (item)
  (coerce item 'list))

(defun to-string (item)
  (coerce item 'string))

(defun strcat (first &rest others)
  (apply #'concatenate 'string first others))


(defparameter *default-mode* :search)
(defparameter *editor* "nvim")
(defparameter *lightning-initial-path-file* "/home/grant/Ramdisk/.lightninginitialpath")
(defparameter *lightning-path-file* "/home/grant/Ramdisk/.lightningpath")
(defparameter *lightning-command-file* "/home/grant/Ramdisk/.lightningcommand")
(defparameter *current-directory* nil)

(defun extract-filename (string)
  (to-string (butlast (to-list (apply #'concatenate 'string
				      (mapcar (lambda (str)
						(strcat str " ")) (nthcdr 8
									  (remove "" (split-sequence:split-sequence #\space string) :test #'string=))))))))

(defun is-acceptable-char (char)
  (let ((code (char-code char)))
    (or (eq char #\.)
	(<= (char-code #\a) code (char-code #\z))
	(<= (char-code #\0) code (char-code #\9)))))

(defun get-char-range ()
  "get a string of the characters that are valid to enter in search mode"
  (let ((chars nil))
    (dotimes (i 255)
      (if (is-acceptable-char (code-char i))
	  (push (code-char i) chars)))
    chars))

(defun filename-clean (filename)
  "convert a raw filename to a simplified one that can be searched for"
  (let ((acceptable-chars (get-char-range)))
    (to-string (remove-if-not (lambda (item)
				(member item acceptable-chars))
			      (to-list (string-downcase filename))))))

(defun generate-print-name (filename type)
  (strcat filename (case type
		     (:file "")
		     (:directory "/"))))

(defun ls (&optional (path *current-directory*))
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

(defun cd (path)
  "modifies a special variable that corresponds to the current path"
  (setf *current-directory* (to-string (butlast (to-list (trivial-shell:shell-command (strcat "cd \"" *current-directory* "\";cd \"" path "\";pwd")))))))

(defun pwd ()
  *current-directory*)

(defun write-text (x y text-string &optional (fg-bg (cons termbox:+default+ termbox:+default+)))
  "execute a series of change-cell's in a sequential manner such as to write a line of text"
  (dotimes (i (length text-string))
    (termbox:change-cell (+ x i) y (char-code (schar text-string i)) (car fg-bg) (cdr fg-bg))))

(defun select-files-in-search-buffer (all-files search-buffer)
  "return a list of selected files by comparing simplified filenames with the search buffer"
  (let ((result nil))
    (dolist (f all-files)
      (if (string= search-buffer (subseq (getf f :clean-name) 0 (min (length search-buffer) (length (getf f :clean-name)))))
	  (push f result)))
    (reverse result)))

(defun get-file-colors (mode selected-index this-file file-list)
  "return a cons cell containing the foreground and background colors for the given file"
  (let ((fg termbox:+default+)
	(bg termbox:+default+))
    (if (and (eq mode :normal) (eq this-file (nth selected-index file-list)))
	(setf fg termbox:+black+
	      bg termbox:+white+))
    (if (eq (getf this-file :type) :directory)
	(setf fg termbox:+blue+))
    (cons fg bg)))

(defun show-this-file-p (this-file selected-files)
  (or (null selected-files) (member this-file selected-files)))

(defun draw-file-list (ystart yend mode selected-files selected-index file-list)
  "draw the list of selected file-list onto the screen"
  (let ((x 0)
	(y ystart)
	(width (1+ (apply #'max (mapcar #'length (mapgetf (or selected-files file-list) :print-name))))))
    (dolist (f file-list)
      (if (= y yend)
	  (setf y ystart
		x (+ x width)))
      (when (show-this-file-p f selected-files)
	(write-text x y (getf f :print-name) (get-file-colors mode selected-index f file-list))
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
  (write-string-to-file *lightning-path-file* path)
  (write-string-to-file *lightning-command-file* command)
  (exit))

(defun action (file path)
  "do something with a filename that the user selected"
  (case (getf file :type)
    (:file
     (open-file-with-command path (strcat *editor* " \"" *current-directory* "/" (getf file :name) "\"")))
    (:directory
     (cd (getf file :name)))))

(defun event-to-symbol (event)
  (when (eq (getf event :type) termbox:+event-key+)
    (if (not (zerop (getf event :ch)))
	(code-char (getf event :ch))
	(getf event :key))))
(defparameter *bindings* `(:bindings
			   (,termbox:+key-space+ :toggle-mode
						 #\' :open-path
						 #\; :quit
						 #\, :up-one-dir)
			   :submodes
			   (:normal
			    (:bindings
			     (#\j :select-down
				  #\k :select-up)))
			   (:search
			    (:bindings
			     (#\- :delete-char)))))
(defun symbol-to-action (symbol mode bindings)
  (or (getf (getf bindings :bindings) symbol)
      (getf (getf (getf (getf bindings :submodes) mode) :bindings) symbol)))

(defun lightning ()
  (let ((mode *default-mode*)
	(selected-files ())
	(search-buffer ())
	(selected-index 0)
	(all-files ()))
    (flet ((clear-search-state ()
	     (setf mode *default-mode*
		   all-files ()
		   selected-files ()
		   selected-index 0
		   search-buffer ())))
      (cd (read-string-from-file *lightning-initial-path-file*))
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
	       (action (first selected-files) *current-directory*)
	       (clear-search-state))
	     (let ((event (termbox:poll-event)))
	       (case (symbol-to-action (event-to-symbol event) mode *bindings*)
		 (:toggle-mode
		  (let ((result (switch-mode mode selected-index selected-files all-files)))
		    (setf mode (nth 0 result)
			  selected-index (nth 1 result)
			  selected-files ()
			  search-buffer ())))
		 (:up-one-dir
		  (cd "..")
		  (setf search-buffer ()
			all-files ()
			selected-index 0))
		 (:quit
		  (open-file-with-command *current-directory* "true"))
		 (:select-down
		  (setf selected-index (mod (1- selected-index) (length all-files))))
		 (:select-up
		  (setf selected-index (mod (1+ selected-index) (length all-files))))
		 (:open-path
		  (action (nth selected-index all-files) *current-directory*)
		  (clear-search-state))
		 (:delete-char
		  (if (plusp (length search-buffer))
		      (let ((new-search-buffer (subseq search-buffer 0 (1- (length search-buffer)))))
			(setf search-buffer (if (string= new-search-buffer "") nil new-search-buffer)
			      selected-files nil)))))
	       (if (eq (getf event :type) termbox:+event-key+)
		   (let ((letter (code-char (getf event :ch))))
		     (if (and (not (equal letter #\Null)) (eq mode :search))
		      (let ((new-selection (select-files-in-search-buffer (or selected-files all-files) (strcat (or search-buffer "") (to-string (list letter))))))
			(if (and (is-acceptable-char letter) new-selection)
			    (setf search-buffer (strcat (or search-buffer "") (to-string (list letter)))
				  selected-files new-selection))))))))))))
