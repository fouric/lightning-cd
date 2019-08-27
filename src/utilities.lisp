(in-package :lightning-cd)

(defun split-list (list max-sublist-size)
  "takes a single flat list and splits it into a list of sublists each with no more than MAX-SUBLIST-SIZE elements"
  (assert (> max-sublist-size 0))
  (loop :while list
        :collect (loop :repeat max-sublist-size
                       :when list
                         :collect (pop list))))

(defun append-char (string char)
  (concatenate 'string string (coerce (list char) 'string)))
