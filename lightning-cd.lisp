(asdf:load-system :split-sequence)
(asdf:load-system :trivial-shell)

(asdf:load-system :cl-plus-c)
(use-package :plus-c)

(cffi-sys:%load-foreign-library
 :libtest #p"/usr/lib/libtermbox.so")

(autowrap:c-include "/usr/include/termbox.h"
		    :constant-accessor tb-const)

(defun to-list (item)
  (coerce item 'list))

(defun to-string (item)
  (coerce item 'string))

(defmethod dir-p ((path list))
  "takes a path as a *list* and determines whether the path identifies a directory"
  (plusp (length (trivial-shell:shell-command (concatenate 'string "if [ -d " (to-string path) " ]; then echo 1; fi")))))

(defmethod file-p ((path list))
  "takes a path as a *list* and determines whether the path identifies a file"
  (plusp (length (trivial-shell:shell-command (concatenate 'string "if [ -f " (to-string path) " ]; then echo 1; fi")))))

(defun pwd ()
  "returns a list of characters of the current working directory"
  (butlast (to-list (trivial-shell:shell-command "pwd"))))

(defparameter *show-deselected-files* nil)
(defparameter *default-mode* 'search)
(defparameter *editor* (to-list "nvim"))
(defparameter *lightning-path-file* (to-list "/home/grant/Ramdisk/.lightningpath"))
(defparameter *current-directory* (pwd))

(defun ls ()
  "takes an optional path as a *list* and returns a list of list of characters that correspond to a directory listing"
  (butlast (split-sequence:split-sequence #\newline (to-list (trivial-shell:shell-command (concatenate 'string "ls \"" (to-string *current-directory*) "\""))))))

(defmethod cd ((path list))
  "modifies a special variable that corresponds to the current path"
  (setf *current-directory* (append *current-directory* (list #\/) path)))

(defmethod write-text ((x integer) (y integer) (text list) &optional (fg-bg '(0 . 0)))
  "execute a series of change-cell's in a sequential manner such as to write a line of text"
  (dotimes (i (length text))
    (tb-change-cell (+ x i) y (char-code (nth i (to-list text))) (car fg-bg) (cdr fg-bg))))

(defun get-char-range ()
  "get a string of the characters that are valid to enter in search mode"
  (let ((chars (list #\.)))
    (dotimes (i 255)
      (if (or (and (>= i (char-code #\a)) (<= i (char-code #\z)))
	      (and (>= i (char-code #\0)) (<= i (char-code #\9))))
	  (nconc chars (list (code-char i)))))
    chars))

(defmethod filename-clean ((filename list))
  "convert a raw filename to a simplified one that can be searched for"
  (let ((acceptable-chars (get-char-range)))
    (remove-if-not (lambda (item) (member item acceptable-chars)) (to-list (string-downcase (to-string filename))))))

(defun select-files-in-search-buffer (all-files search-buffer)
  "return a list of selected files by comparing simplified filenames with the search buffer"
  (remove-if-not (lambda (file) (equal search-buffer (subseq (filename-clean file) 0 (length search-buffer)))) all-files))

(defmethod get-file-colors ((mode symbol) (selected-files list) (selected-index integer) (this-file list) (file-list list))
  "return a cons cell containing the foreground and background colors for the given file"
  (cond
    ((and (eq mode 'search) (member this-file selected-files :test #'equal) *show-deselected-files*)
     (cons (tb-const "TB_BLACK") (tb-const "TB_WHITE")))
    ((and (eq mode 'normal) (equal this-file (nth selected-index file-list)))
     (cons (tb-const "TB_BLACK") (tb-const "TB_WHITE")))
    (t
     (cons 0 0))))

(defun show-this-file-p (this-file selected-files)
  (or *show-deselected-files* (member this-file selected-files :test #'equal) (null selected-files)))

(defun write-path (filename path)
  (with-open-file (out filename
		       :direction :output
		       :if-exists :supersede)
    (with-standard-io-syntax
      (print path out))))

(defun draw-file-list (ystart yend mode selected-files selected-index file-list)
  "draw the list of selected file-list onto the screen"
  (let ((x 0)
	(width 1)
	(y ystart))
    (dolist (f file-list)
      (if (= y yend)
	  (setf y ystart
		x (+ x width)
		width 1))
      (when (show-this-file-p f selected-files)
	(if (dir-p f)
	    (setf f (to-string (append (coerce f 'list) (list #\/)))))
	(setf width (max width (+ (length (coerce f 'list)) 1)))
	(write-text x y f (get-file-colors mode selected-files selected-index f file-list))
	(incf y)))))

(defun switch-mode (prev-mode selected selected-files files)
  "switch the mode to either search or normal and do associated setup for each mode"
  (let ((new-mode nil))
    (if (eq prev-mode 'search)
	(setf selected (if (plusp (length selected-files))
			   (position (nth 0 selected-files) files)
			   0)
	      new-mode 'normal)
	(setf new-mode 'search))
    (list new-mode selected)))

(defun open-file-with-command (path command)
  "write the current path, close Lightning, and execute the command"
  (tb-shutdown)
  (write-path (to-string *lightning-path-file*) (to-string path))
  (trivial-shell:shell-command (to-string command))
  (exit))

(defun action (filename path)
  "do something with a filename that the user selected"
  (when (dir-p filename)
    (cd filename))
  (when (file-p filename)
    (open-file-with-command path (append *editor* (list #\space) filename))))

(defun lightning ()
  (let ((mode *default-mode*)
	(selected-files ())
	(search-buffer ())
	(selected-index 0)
	(all-files ())
	(char-range (get-char-range)))
    (tb-init)
    (loop
					; sort files
       (setf all-files (mapcar #'to-list (sort (mapcar #'to-string (ls)) #'string<)))

					; if we're in search mode, only show files that match the buffer
       (if (and (eq mode 'search) search-buffer)
	   (setf selected-files (select-files-in-search-buffer all-files search-buffer)))

					; clear
       (tb-clear)

					; draw file list, search string, mode, and current directory
       (draw-file-list 1 (1- (tb-height)) mode selected-files selected-index all-files)
       (if (eq mode 'search)
	   (write-text 0 (1- (tb-height)) search-buffer))
       (write-text 0 0 (append (to-list (string mode)) (to-list ": ") *current-directory*))
       (tb-present)

					; get and process input
       (cond
	 ((and (eq mode 'search) (= (length selected-files) 1) (plusp (length search-buffer)))
	  (action (nth 0 selected-files) *current-directory*)
	  (setf mode *default-mode*
		selected-files ()
		selected-index 0
		search-buffer ()))
	 (t
	  (plus-c:c-let ((event (:struct (tb-event)) :free t))
	    (tb-poll-event event)
	    (let ((letter (code-char (event :ch)))
		  (keycode (event :key)))
	      (cond
					; go up one directory
		((eq letter #\.)
		 (cd (to-list ".."))
		 (setf search-buffer ()
		       selected-index 0))
					; switch modes
		((eq keycode (tb-const "TB_KEY_SPACE"))
		 (let ((result (switch-mode mode selected-index selected-files all-files)))
		   (setf mode (nth 0 result)
			 selected-index (nth 1 result)
			 selected-files ()
			 search-buffer ())))
					; quit instantly
		((eq keycode (tb-const "TB_KEY_ESC"))
		 (return))
					; quit to the current directory
		((eq letter #\;)
		 (open-file-with-command *current-directory* (to-list "true")))
					; normal-mode-specific commands
		((eq mode 'normal)
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
		    (open-file-with-command *current-directory* (append *editor* (list #\space) (list (nth selected-index all-files)))))))
		((eq mode 'search)
		 (cond
		   (letter
		    (cond
		      ((member letter char-range)
		       (setf search-buffer (append search-buffer (list letter))))
		      ((eq letter #\-)
		       (setf search-buffer (butlast search-buffer)))))
		   ((eq keycode (tb-const "TB_KEY_ENTER"))
		    (action (nth 0 selected-files) *current-directory*))))))))))))

(lightning)
