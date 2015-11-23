(defun is-acceptable-char (char)
  (let ((code (char-code char)))
    (or (eq char #\.)
	(<= (char-code #\a) code (char-code #\z))
	(<= (char-code #\0) code (char-code #\9)))))

(defun get-char-range ()
  "get a string of the characters that are valid to enter in search mode"
  (let ((chars nil))
    (dotimes (i 255)
      (if (is-acceptable-char (code-char i))
	  (push (code-char i) chars)))
    chars))

(defun filename-clean (filename)
  "convert a raw filename to a simplified one that can be searched for"
  (let ((acceptable-chars (get-char-range)))
    (to-string (remove-if-not (lambda (item)
				(member item acceptable-chars))
			      (to-list (string-downcase filename))))))

(defun select-files-in-search-buffer (all-files search-buffer)
  "return a list of selected files by comparing simplified filenames with the search buffer"
  (let ((result nil))
    (dolist (f all-files)
      (if (string= search-buffer (subseq (getf f :clean-name) 0 (min (length search-buffer) (length (getf f :clean-name)))))
	  (push f result)))
    (reverse result)))
