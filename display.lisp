(asdf:load-system :cl-termbox)

(defun write-text (x y text-string &optional (fg-bg (cons termbox:+default+ termbox:+default+)))
  "execute a series of change-cell's in a sequential manner such as to write a line of text"
  (dotimes (i (length text-string))
    (termbox:change-cell (+ x i) y (char-code (schar text-string i)) (car fg-bg) (cdr fg-bg))))

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
