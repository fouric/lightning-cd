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

(defun ls (&optional (path *current-directory*))
  "takes an optional path as a *list* and returns a list of list of characters that correspond to a directory listing"
  (butlast (split-sequence:split-sequence #\newline (to-list (trivial-shell:shell-command (concatenate 'string "ls \"" (to-string path) "\""))))))

(defun pwd ()
  "returns a list of characters of the current working directory"
  (butlast (to-list (trivial-shell:shell-command "pwd"))))

(defun cd (path)
  "modifies a special variable that corresponds to the current path"
  (setf *current-directory* (append *current-directory* (list #\/) path)))

(defun test ()
  (tb-init)
  (tb-clear)
  (tb-change-cell 0 0 (char-code #\!) 0 0)
  (tb-present)
  (plus-c:c-let ((event (:struct (tb-event)) :free t))
    (tb-poll-event event)
    (tb-change-cell 0 1 (event :ch) 0 0))
  (tb-present)
  (sleep 2)
  (tb-shutdown))

(defparameter *show-deselected-files* nil)
(defparameter *default-mode* 'search)
(defparameter *editor* (to-list "nvim"))
(defparameter *lightning-path-file* (to-list "/home/grant/Ramdisk/.lightningpath"))
(defparameter *current-directory* (pwd))

(defun write-text (x y text fg-bg)
  "execute a series of change-cell's in a sequential manner such as to write a line of text"
  (dotimes (i (length (coerce 'list text)))
    (tb-change-cell (+ x i) y (char-code (nth i (coerce 'list text))) (car fg-bg) (cdr fg-bg))))

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
    (remove-if-not (lambda (item) (member item acceptable-chars)) (to-list (string-downcase (to-string filename))))))

(defun select-files-in-search-buffer (files search-buffer)
  "return a list of selected files by comparing simplified filenames with the search buffer"
  (remove-if-not (lambda (file) (equal search-buffer (subseq (filename-clean file) 0 (length search-buffer)))) files))

(defun get-file-colors (mode selected-files selected this-file file-list)
  (cond
    ((and (eq mode 'search) (member this-file selected-files) *show-deselected-files*)
     (cons (tb-const "TB_BLACK") (tb-const "TB_WHITE")))
    ((and (eq mode 'normal) (eq this-file (nth selected file-list)))
     (cons (tb-const "TB_BLACK") (tb-const "TB_WHITE")))
    (t
     (cons (tb-const "TB_WHITE") 0))))

(defun show-this-file-p (this-file selected-files)
  (or *show-deselected-files* (member this-file selected-files :test #'equal) (null selected-files)))

(defun write-path (filename path)
  (with-open-file (out filename
		       :direction :output
		       :if-exists :supersede)
    (with-standard-io-syntax
      (print path out))))

(defun draw-file-list (ystart yend mode selected-files selected files)
  "draw the list of selected files onto the screen"
  (let ((x 0)
	(width 1)
	(y ystart))
    (dolist (f files)
	    (if (= y yend)
	      (setf y ystart
		    x (+ x width)
		    width 1))
	    (when (show-this-file-p f selected-files)
	      (if (dir-p f)
		  (setf f (coerce (append (coerce f 'list) #\/) 'string)))
	      (setf width (max width (+ (length (coerce f 'list)) 1)))
	      (write-text x y f (get-file-colors mode selected-files selected f files))
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
    (list new-mode selected () ())))

(defun action (filename path)
  "do something with a filename that the user selected"
  (when (dir-p filename)
    (cd filename)
    (list *default-mode* 0 nil nil))
  (when (file-p filename)
    (command path (append *editor* (list #\space) filename))))

(defun command (path command)
  "write the current path, close Lightning, and execute the command"
  (tb-shutdown)
  (write-path *lightning-path-file* path)
  (trivial-shell:shell-command (to-string command))
  (exit))

(defun lightning ()
  (let ((mode *default-mode*)
	(selected-files ())
	(search-buffer ())
	(selected 0)
	(files ())
	(char-range (get-char-range)))
    ;(tb-init)
    (loop
       (let ((normal-files ())
	     (dot-files ()))
	 (tb-clear)
	 (setf files (mapcar #'to-list (sort (mapcar #'to-string (ls)) #'string<)))
	 (mapcar (lambda (f) (if (eq (nth 0 f) #\.)
				 (setf dot-files (append dot-files (list f)))
				 (setf normal-files (append normal-files (list f))))) files)
	 (setf files (append normal-files dot-files))
	 (if (eq mode 'search)
	     (setf selected-files (select-files-in-search-buffer files search-buffer)))
	 (print (mapcar #'to-string selected-files))
	 (return)))
    ))
#|
    while True:
        files = normalfiles + dotfiles
        if mode == SEARCH:
            selectedFiles = selectFilesOnsearchBuffer(files, searchBuffer)
        drawFileList(t, 1, t.height() - 1, mode, selected, selectedFiles)
        if mode == SEARCH:
            if len(selectedFiles) == 1 and len(searchBuffer):
                mode, selected, selectedFiles, searchBuffer = action(selectedFiles[0], os.path.realpath('.'))
                continue
            writeText(t, 0, t.height() - 1, searchBuffer, termbox.WHITE, 0)
        if mode == SEARCH:
            modeText = "search"
        elif mode == NORMAL:
            modeText = "normal"
        writeText(t, 0, 0, modeText + ": ", termbox.WHITE, 0)
        writeText(t, len(modeText) + 2, 0, os.path.realpath('.'), termbox.WHITE, 0)
        t.present()

        event = t.poll_event()
        letter, keycode = event[1], event[2]
        if letter == ',':
            os.chdir('..')
            searchBuffer = ''
            selected = 0
        elif keycode == termbox.KEY_SPACE:
            mode, selected, searchBuffer, selectedFiles = switchMode(mode, selected, selectedFiles)
        elif keycode == termbox.KEY_ESC:
            break
        elif letter == ';':
            command(os.path.realpath('.'), 'true')
        elif mode == NORMAL:
            if letter == 'k':
                selected = selected - 1 % len(files)
            elif letter == 'j':
                selected = selected + 1 % len(files)
            elif letter == '\'':
                mode, selected, selectedFiles, searchBuffer = action(files[selected], os.path.realpath('.'))
            elif letter == 'q':
                break
            elif letter == 'v':
                command(os.path.realpath('.'), editor + ' ' + files[selected])
            elif letter == 'n':
                command(os.path.realpath('.'), 'nautilus ' + os.path.realpath('.') + ' > /dev/null 2>&1')
            elif letter == 't':
                command(os.path.realpath('.'), 'tmux > /dev/null')
            elif letter == 'T':
                command(os.path.realpath('.'), 'tmux > /dev/null')
        elif mode == SEARCH:
            if letter:
                if letter in charRange:
                    searchBuffer = searchBuffer + letter
                elif letter == '-':
                    searchBuffer = searchBuffer[:-1]
            if keycode == termbox.KEY_ENTER:
                mode, selected, selectedFiles, searchBuffer = action(selectedFiles[0], os.path.realpath('.'))

    t.close()
    print os.path.realpath('.')# }}}

|#
