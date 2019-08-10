(in-package :lightning-cd)

;; TODO: these don't actually do anything unless you set *screen-width* and *screen-height* - and currently, you don't!
(defparameter *screen-width* 1)
(defparameter *screen-height* 1)
(defun clamp-w (x)
  (clamp x 0 (1- *screen-width*)))
(defun clamp-h (y)
  (clamp y 0 (1- *screen-height*)))

(defmacro defcolors (&rest colors)
  `(progn
     ,@(iterate (for n :from 0)
                (for (constant nil nil) :in colors)
                (collect `(defparameter ,constant ,n)))
     (defun init-colors ()
       ,@(iterate
           (for (constant fg bg) :in colors)
           (collect `(charms/ll:init-pair ,constant ,fg ,bg))))))

(defmacro with-color (color &body body)
  (alexandria:once-only (color)
    `(unwind-protect
          (progn
            (charms/ll:attron (charms/ll:color-pair ,color))
            ,@body)
       (charms/ll:attroff (charms/ll:color-pair ,color)))))

(defun write-string-at (string x y &optional colors)
  (if colors
      (with-color colors
        ;; TODO: port over string clamping code from mercury/monolith
        (charms:write-string-at-point *charms-win* string (clamp-w x) (clamp-h y)))
      (charms:write-string-at-point *charms-win* string (clamp-w x) (clamp-h y)))
  (length string))

(defcolors
    ;; need some way of, when this is recompiled, patching it into running instance

    ;; white
    (+color-white-black+  charms/ll:COLOR_WHITE   charms/ll:COLOR_BLACK)
    (+color-black-white+  charms/ll:COLOR_BLACK charms/ll:COLOR_WHITE)

  ;; blue
  (+color-blue-black+   charms/ll:COLOR_BLUE    charms/ll:COLOR_BLACK)
  (+color-black-blue+   charms/ll:COLOR_BLACK charms/ll:COLOR_BLUE)
  (+color-white-blue+   charms/ll:COLOR_WHITE charms/ll:COLOR_BLUE)
  (+color-blue-white+   charms/ll:COLOR_BLUE charms/ll:COLOR_WHITE)

  ;; cyan
  (+color-cyan-black+   charms/ll:COLOR_CYAN    charms/ll:COLOR_BLACK)

  ;; green
  (+color-green-black+  charms/ll:COLOR_GREEN   charms/ll:COLOR_BLACK)
  (+color-black-green+  charms/ll:COLOR_BLACK charms/ll:COLOR_GREEN)
  (+color-white-green+  charms/ll:COLOR_WHITE charms/ll:COLOR_GREEN)
  (+color-green-white+  charms/ll:COLOR_GREEN charms/ll:COLOR_WHITE)

  ;; yellow
  (+color-yellow-black+  charms/ll:COLOR_YELLOW charms/ll:COLOR_BLACK)
  (+color-black-yellow+  charms/ll:COLOR_BLACK charms/ll:COLOR_YELLOW)
  (+color-yellow-white+  charms/ll:COLOR_YELLOW charms/ll:COLOR_WHITE)
  (+color-white-yellow+  charms/ll:COLOR_WHITE charms/ll:COLOR_YELLOW )

  ;; red
  (+color-red-black+  charms/ll:COLOR_RED charms/ll:COLOR_BLACK)
  (+color-black-red+  charms/ll:COLOR_BLACK charms/ll:COLOR_RED)
  (+color-white-red+    charms/ll:COLOR_WHITE charms/ll:COLOR_RED)
  (+color-red-white+    charms/ll:COLOR_RED charms/ll:COLOR_WHITE)

  ;; pink
  (+color-pink-black+   charms/ll:COLOR_MAGENTA charms/ll:COLOR_BLACK)
  (+color-black-pink+   charms/ll:COLOR_BLACK charms/ll:COLOR_MAGENTA)
  (+color-pink-white+   charms/ll:COLOR_MAGENTA charms/ll:COLOR_WHITE)
  (+color-white-pink+   charms/ll:COLOR_WHITE charms/ll:COLOR_MAGENTA)

  (+color-black-black+   charms/ll:COLOR_BLACK charms/ll:COLOR_BLACK))

(defparameter *charms-win* nil)

(defun init-charms ()
  (force-output *terminal-io*)
  (charms:initialize)
  (charms/ll:timeout 100)
  ;; disable-non-blocking-mode made redundant by setting timeout to positive value
  ;;(charms:disable-non-blocking-mode *charms-win*)
  (setf *charms-win* (charms:standard-window))
  (charms:disable-echoing)
  (charms/ll:curs-set 0) ;; invisible cursor
  (charms/ll:start-color)
  (init-colors)
  (charms:enable-raw-input :interpret-control-characters t))

(defmacro with-charms (&body body)
  `(unwind-protect
        (progn
          (init-charms)
          ,@body)
     (charms:finalize)))

(defun append-char (string char)
  (concatenate 'string string (coerce (list char) 'string)))


(defun draw ()
  (multiple-value-bind (width height) (charms:window-dimensions *charms-win*)
    (setf *screen-width* width
          *screen-height* height)
    (charms:clear-window *charms-win* :force-repaint t)
    #++(with-color +color-white-black+
         (charms:write-string-at-point *charms-win* (concatenate 'string ": " *shell-contents*) 1 (1- height)))
    (charms:refresh-window *charms-win*)))

(defun main-loop (buffer)
  (f:update-swank)
  (multiple-value-bind (width height) (charms:window-dimensions *charms-win*)
    (setf *screen-width* (1- width)
          *screen-height* height))
  (let ((char (charms:get-char *charms-win* :ignore-error t)))
    (when char
      (case char
        (#\esc
         (return-from main-loop))
        (#\rubout
         (setf buffer (subseq buffer 0 (max 0 (1- (length buffer)))))
         (format t "buffer is now ~s~%" buffer))
        (t
         (setf buffer (append-char buffer char))
         (format t "buffer is now ~s~%" buffer))))
    (charms:clear-window *charms-win* :force-repaint t)
    (write-string-at (concatenate 'string "> " buffer) 1 (1- *screen-height*) +color-white-black+)
    (charms:refresh-window *charms-win*)
    (main-loop buffer)))

(defun lightning-cd ()
  (with-charms
    (main-loop "")))

(defun main (args)
  (declare (ignore args))
  (lightning-cd))
