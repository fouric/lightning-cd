(in-package :lightning-cd)

(defun hacky-name (namestring)
  "take a NAMESTRING and heuristically return the actual name component after separating it from the path"
  ;; use PATHNAME-DIRECTORY
  (let ((chunks (remove-if-not (lambda (str) (plusp (length str))) (split-sequence:split-sequence #\/ namestring))))
    (first (last chunks))))

(defun list-items (directory &key (ordering-strategy :by-type))
  "takes DIRECTORY as a pathspec/namestring and returns a list of cons cells where each car is :file or :directory and each cdr is the namestring of an item in the directory"
  (let* ((files (loop for dir in (uiop:directory-files directory) collect (cons :file dir)))
         (directories (loop for file in (uiop:subdirectories directory) collect (cons :directory file))))
    (case ordering-strategy
      (:by-type
       (mapcar (lambda (item) (cons (car item) (namestring (cdr item)))) (append directories files)))
      (:by-name
       (sort (mapcar (lambda (blob) (cons (car blob) (namestring (cdr blob)))) (append files directories)) #'string< :key #'cdr)))))

(defun draw-item (item x y)
  "take a single (type . pathspec) directory listing ITEM and render it at screen position (X, Y)"
  (case (car item)
    (:file
     (f:write-string-at (hacky-name (namestring (cdr item))) x y))
    (:directory
     (f:write-string-at (append-char (hacky-name (namestring (cdr item))) #\/) x y f:+color-blue-black+))))

(defun draw-column (column x y)
  "draw a COLUMN which is a list of cons cells of (type . pathspec) down from screen position (X, Y)"
  ;; each column is a list of CONS CELLS (car is :directory/:file, cdr is path) and not just paths
  (let ((width (+ 3 (apply #'max (mapcar #'length (mapcar #'hacky-name (mapcar #'cdr column))))))
        (y (1- y)))
    (dolist (item column width)
      (draw-item item x (incf y)))))

(defun printable-character? (char)
  "determine whether or not CHAR is a printable (ASCII) character"
  ;; #\null is ascii d0 and #\Us is ascii d31
  (not (or (<= (char-code #\null) (char-code char) (char-code #\Us))
           ;; 127 is escape and 160 is the last unprintable ascii character
           (<= 127 (char-code char) 160))))

(defun filter-character (char)
  (when (and (printable-character? char)
             (not (member char '(#\comma #\space #\semicolon #\' #\" #\?))))
    char))

(defun filter-string (string)
  (remove-if-not #'filter-character string))

(defun draw (buffer current-directory)
  "for a given filtering text BUFFER and pathspec CURRENT-DIRECTORY clear the terminal and draw the contents of said directory"
  ;; only called when dirty so we can do whatever we want here
  (f:clear-window t)
  (let* ((everything (list-items current-directory))
         (filtered (remove-if-not (lambda (term) (search (string-downcase buffer) (string-downcase term))) everything :key (lambda (item) (hacky-name (cdr item)))))
         (height (- f:*screen-height* 2))
         (columns (split-list filtered height))
         (x 1))
    (dolist (column columns)
      (incf x (draw-column column x 0)))
    (f:write-string-at (concatenate 'string "> " buffer) 1 (1- f:*screen-height*) f:+color-white-black+)
    (f:refresh-window)))

(defun main-loop (buffer current-directory)
  (f:update-swank)
  (f:update-charms-dimensions)
  (unless current-directory
    (setf current-directory (concatenate 'string (namestring (user-homedir-pathname)) "/")))
  (let ((char (f:get-char))
        dirty?)
    (when char
      (if (filter-character char)
        (setf buffer (append-char buffer char)
              dirty? t)
        (case char
          (#\esc
           (return-from main-loop))
          (#\rubout ;; backspace
           (setf buffer (subseq buffer 0 (max 0 (1- (length buffer))))
                 dirty? t))
          (#\page ;; C-l
           (format t "~&cwd is ~s~%" current-directory))
          (#\Vt ;; C-k
           (setf buffer ""
                 dirty? t))
          (t
           (format t "~&got code ~s~%" char)))))
    (when dirty?
      ;; we should definitely not re-list directory contents in user interaction loop - use entr(1) or something in a background process, or only refresh when explicitly requested
      (draw buffer current-directory))
    (main-loop buffer current-directory)))

(defun lightning-cd (&optional (current-directory (concatenate 'string (namestring (user-homedir-pathname)) "/")))
  (f:with-charms (:timeout 100 :color t :raw-input t :interpret-control-characters t)
    (draw "" current-directory)
    (main-loop "" current-directory)))

(defun main (args)
  (lightning-cd (first args)))
