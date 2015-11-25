(asdf:load-system :cl-termbox)

(defun resolve-termbox-symbols (list)
  (mapcar (lambda (item)
	    (typecase item
	      (cons (resolve-termbox-symbols item))
	      (symbol
	       (if (find-symbol (string item) :termbox)
		   (symbol-value item)
		   item))
	      (t item))) list))

; this will soon be moved into a separate file
(defparameter *bindings* (resolve-termbox-symbols '(:bindings
						    (termbox:+key-space+ :toggle-mode
						     #\' :open-path
						     #\; :quit
						     #\, :up-one-dir
						     #\" :refresh-file-list)
						    :submodes
						    (:normal
						     (:bindings
						      (#\j :select-down
							   #\k :select-up))
						     :search
						     (:bindings
						      (#\- :delete-char))))))

(defun event-to-symbol (event)
  (when (eq (getf event :type) termbox:+event-key+)
    (if (not (zerop (getf event :ch)))
	(code-char (getf event :ch))
	(getf event :key))))

(defun symbol-to-action (symbol mode bindings)
  (or (getf (getf bindings :bindings) symbol)
      (getf (getf (getf (getf bindings :submodes) mode) :bindings) symbol)))
