(in-package :lightning-cd)

(defun append-char (string char)
  (concatenate 'string string (coerce (list char) 'string)))

(defun draw (chunks buffer)
  (f:clear-window t)
  (let ((y -1))
    (dolist (item (reverse (first (last chunks))))
      (f:write-string-at (hacky-name (namestring item)) 1 (incf y))))
  (f:write-string-at (concatenate 'string "> " buffer) 1 (1- f:*screen-height*) f:+color-white-black+)
  #++(with-color +color-white-black+
       (charms:write-string-at-point *charms-win* (concatenate 'string ": " *shell-contents*) 1 (1- height)))
  (f:refresh-window))

(defun hacky-name (namestring)
  ;; use PATHNAME-DIRECTORY
  (let ((chunks (remove-if-not (lambda (str) (plusp (length str))) (split-sequence:split-sequence #\/ namestring))))
    (first (last chunks))))

(defun main-loop (buffer current-directory)
  (f:update-swank)
  (f:update-charms-dimensions)
  (unless current-directory
    (setf current-directory (user-homedir-pathname)))
  (let ((char (f:get-char))
        dirty?)
    (when char
      (case char
        (#\esc
         (return-from main-loop))
        (#\rubout
         (setf buffer (subseq buffer 0 (max 0 (1- (length buffer))))
               dirty? t))
        (#\page
         (fresh-line)
         (format t "cwd is ~s~%" current-directory))
        (t
         (setf buffer (append-char buffer char)
               dirty? t))))
    ;;(setf current-directory "/home/fouric/async/")
    (when dirty?
      (let* ((directories (uiop:directory-files current-directory))
             (files (uiop:subdirectories current-directory))
             (everything (append files directories))
             (height (- f:*screen-height* 2))
             (chunks nil)
             (count 0)
             chunk)
        (dolist (item everything)
          (when (>= count height)
            (push chunk chunks)
            (setf count 0
                  chunk nil))
          (push item chunk)
          (incf count))
        (push chunk chunks)
        (setf chunks (nreverse chunks))
        (draw chunks buffer)))
    (main-loop buffer current-directory)))

(defun lightning-cd (&optional (current-directory (user-homedir-pathname)))
  (f:with-charms (:timeout 100 :color nil :raw-input t :interpret-control-characters t)
    (main-loop "" current-directory)))

(defun main (args)
  (lightning-cd (first args)))
