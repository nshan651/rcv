(in-package #:cl-user)
(defpackage #:rcv/election
  (:use #:cl #:cl-csv)
  ;; (:export :get-ballots :get-candidates)
  )
(in-package #:rcv/election)

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

(defun compute-quota (ballots seats)
  (multiple-value-bind (quotient)
      (floor (1+ (/ (length ballots) (1+ seats))))
    quotient))

(defstruct (election (:constructor make-election (source
						  &optional number-of-seats
						  &aux (ballots (get-ballots source))
						    (candidates (get-candidates ballots))
						    (seats (or number-of-seats 1))
						    (quota (compute-quota ballots seats)))))
  (ballots :read-only)
  (candidates :read-only)
  (seats :read-only :type integer)
  (quota :read-only :type integer)
  ranking-history)

(let ((e (make-election "../data/3-seats.csv" 3)))
  election-ballots e)
