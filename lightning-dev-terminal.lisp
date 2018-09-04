;; thanks http://turtleware.eu/posts/cl-charms-crash-course.html
(defun main ()
  (defvar *console-io* *terminal-io*)
  (ql:quickload :swank :silent t)
  (funcall (intern "CREATE-SERVER" :swank) :port 4005 :dont-close t)
  (loop (sleep 1)))
(main)
