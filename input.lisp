(defparameter *bindings* `(:bindings
			   (,termbox:+key-space+ :toggle-mode
						 #\' :open-path
						 #\; :quit
						 #\, :up-one-dir)
			   :submodes
			   (:normal
			    (:bindings
			     (#\j :select-down
				  #\k :select-up))
			    :search
			    (:bindings
			     (#\- :delete-char)))))

(defun event-to-symbol (event)
  (when (eq (getf event :type) termbox:+event-key+)
    (if (not (zerop (getf event :ch)))
	(code-char (getf event :ch))
	(getf event :key))))

(defun symbol-to-action (symbol mode bindings)
  (or (getf (getf bindings :bindings) symbol)
      (getf (getf (getf (getf bindings :submodes) mode) :bindings) symbol)))
