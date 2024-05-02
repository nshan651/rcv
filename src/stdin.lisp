(in-package #:cl-user)
(defpackage #:rcv.stdin
  (:use #:cl)
  (:export :get-ballots :get-candidates))
(in-package #:rcv.stdin)

(defun get-ballots (source)
  "Read ballots from stdin or a file."
  (let ((csv-str (if (and source (probe-file source))
		     (parse-namestring source)
		     (with-output-to-string (output)
		       (loop for line = (read-line *standard-input* nil)
			     while line
			     do (write-line line output))))))
    (remove-empty (cl-csv:read-csv csv-str))))

(defun get-candidates (ballots)
  "Get a list of candidates, derived from the submitted ballots."
  (remove-duplicates (reduce #'append ballots :key #'identity)
		     :test #'equal))

(defun remove-empty (ballots)
  "Remove nil values and empty strings from ballots."
  (mapcar #'(lambda (ballot)
	      (remove-if #'(lambda (x) (or (null x) (string= x "")))
			 ballot))
	  ballots))
