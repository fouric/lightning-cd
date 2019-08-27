(in-package :lightning-cd)

(defun hacky-name (namestring)
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
  (case (car item)
    (:file
     (f:write-string-at (hacky-name (namestring (cdr item))) x y))
    (:directory
     (f:write-string-at (append-char (hacky-name (namestring (cdr item))) #\/) x y f:+color-blue-black+))))

(defun draw-column (column x y)
  ;; each column is a list of CONS CELLS (car is :directory/:file, cdr is path) and not just paths
  (let ((width (+ 3 (apply #'max (mapcar #'length (mapcar #'hacky-name (mapcar #'cdr column))))))
        (y (1- y)))
    (dolist (item column width)
      (draw-item item x (incf y)))))

(defun draw (buffer current-directory)
  (f:clear-window t)
  (let* ((everything (list-items current-directory))
         (height (- f:*screen-height* 2))
         (columns (split-list everything height))
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
  (setf current-directory (concatenate 'string (namestring (user-homedir-pathname)) "/"))
  (let ((char (f:get-char))
        dirty?)
    (when char
      (case char
        (#\esc
         (return-from main-loop))
        (#\rubout ;; backspace
         (setf buffer (subseq buffer 0 (max 0 (1- (length buffer))))
               dirty? t))
        (#\page ;; what?
         (fresh-line)
         (format t "cwd is ~s~%" current-directory))
        (#\Vt ;; C-k
         (setf buffer ""
               dirty? t))
        (t
         (if (<= (char-code #\space) (char-code char) (char-code #\~))
           (setf buffer (append-char buffer char)
                 dirty? t)
           (format t "got char ~s~%" char)))))
    ;;(setf current-directory "/home/fouric/async/")
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
