(asdf:load-system :cl-termbox)

(load "utilities.lisp")
(load "input.lisp")
(load "terminal-io.lisp")
(load "display.lisp")
(load "filter.lisp")

(declaim (optimize (debug 2)))
(declaim (optimize (speed 0)))


(defparameter *default-mode* :search)
(defparameter *editor* "nvim")
(defparameter *lightning-initial-path-file* "/home/grant/Ramdisk/.lightninginitialpath")
(defparameter *lightning-path-file* "/home/grant/Ramdisk/.lightningpath")
(defparameter *lightning-command-file* "/home/grant/Ramdisk/.lightningcommand")
(defparameter *current-directory* nil)


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
     (setf *current-directory* (cd (getf file :name) *current-directory*)))))

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
      (setf *current-directory* (cd (read-string-from-file *lightning-initial-path-file*) *current-directory*))
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
		  (setf *current-directory* (cd ".." *current-directory*))
		  (clear-search-state))
		 (:quit
		  (open-file-with-command *current-directory* "true"))
		 (:select-down
		  (setf selected-index (mod (1+ selected-index) (length all-files))))
		 (:select-up
		  (setf selected-index (mod (1- selected-index) (length all-files))))
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
