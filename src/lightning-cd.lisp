(in-package :lightning-cd)

(defun append-char (string char)
  (concatenate 'string string (coerce (list char) 'string)))

(defun draw (buffer current-directory)
  (let* ((everything (list-items current-directory))
         (height (- f:*screen-height* 2))
         (columns (split-list everything height)))
    (f:clear-window t)
    (let ((x 1))
      (dolist (column columns)
        ;; each column is a list of CONS CELLS (car is :directory/:file, cdr is path) and not just paths
        (let ((width (+ 3 (apply #'max (mapcar #'length (mapcar #'hacky-name (mapcar #'cdr column))))))
              (y -1))
          (dolist (item column)
            (f:write-string-at (hacky-name (namestring (cdr item))) x (incf y)))
          (incf x width))))
    (f:write-string-at (concatenate 'string "> " buffer) 1 (1- f:*screen-height*) f:+color-white-black+)
    #++(with-color +color-white-black+
         (charms:write-string-at-point *charms-win* (concatenate 'string ": " *shell-contents*) 1 (1- height)))
    (f:refresh-window)))

(defun split-list (list max-sublist-size)
  "takes a single flat list and splits it into a list of sublists each with no more than MAX-SUBLIST-SIZE elements"
  (assert (> max-sublist-size 0))
  (loop :while list
        :collect (loop :repeat max-sublist-size
                       :when list
                         :collect (pop list))))

(defun hacky-name (namestring)
  ;; use PATHNAME-DIRECTORY
  (let ((chunks (remove-if-not (lambda (str) (plusp (length str))) (split-sequence:split-sequence #\/ namestring))))
    (first (last chunks))))

(defun list-items (directory)
  (let* ((directories (loop for dir in (uiop:directory-files directory) collect (cons :directory dir)))
         (files (loop for file in (uiop:subdirectories directory) collect (cons :file file))))
    (sort (mapcar (lambda (blob) (cons (car blob) (namestring (cdr blob)))) (append files directories)) #'string< :key #'cdr)))

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
  (f:with-charms (:timeout 100 :color nil :raw-input t :interpret-control-characters t)
    (draw "" current-directory)
    (main-loop "" current-directory)))

(defun main (args)
  (lightning-cd (first args)))
