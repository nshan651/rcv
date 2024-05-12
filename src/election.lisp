(in-package #:cl-user)
(defpackage #:rcv/election
  (:use #:cl #:cl-csv)
  (:export :get-ballots :get-candidates))
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

(defun add-party (candidates)
  "Get a mapping of the candidate and their respective party."
  (mapcar #'(lambda (c) (cons c 'Unkown)) candidates))

(defun display-results (ranking-history)
  "Print out the ranking history."
  (let ((round-nbr 0))
    (dolist (round ranking-history)
      (format t "Round ~a~%" (incf round-nbr))
      (format t "Candidate                       Votes
------------------------------  ---------~%")
      (dolist (rankpair (reverse round))
	(let* ((candidate (car rankpair))
	       (votes (cdr rankpair))
	       (hspace-col1 (- 32 (length candidate)))
	       ;; (hspace-col2 (- 10 (length (format nil "~d" votes))))
	       )
	  (format t "~a~vA~a~%" candidate hspace-col1 #\Space votes)))
      (format t "~%"))))

(defun election-context (candidates seats quota)
  "Closure encapsulating an election quota."
  (lambda (f) (funcall f candidates seats quota)))

(defun example-function (election-context)
  "Example function using the encapsulated data."
  (election-context #'(lambda (candidates seats quota)
                                 (format t "Candidates: ~a~%Seats: ~a~%Quota: ~a~%"
                                         candidates seats quota))))

(let ((election (election-context 69 420 666)))
  (example-function election))

;; (defstruct (election (:constructor make-election
;; 			 (ballots &optional number-of-seats
;; 			  &aux (candidates (get-candidates ballots))
;; 			    (seats (or number-of-seats 1))
;; 			    (quota (compute-quota ballots seats)))))
;;   (candidates :read-only)
;;   (seats :read-only :type integer)
;;   (quota :read-only :type integer)
;;   ranking-history)

;; (let ((e (make-election "../data/3-seat.csv")))
;;   election-ballots e)
