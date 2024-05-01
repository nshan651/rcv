;;;; Ranked choice voting program
;;;; Instant Runoff Voting(IRV)
;;;; The instant-runoff vote counting procedure is as follows:
;;;;    1. Eliminate the candidate appearing as the first preference on the fewest ballots.
;;;;    2. If only one candidate remains, elect this candidate and stop.
;;;;    3. Otherwise go to 1.
;;;; https://en.wikipedia.org/wiki/Instant-runoff_voting
;;;; Python project for IRV: https://github.com/jontingvold/pyrankvote

(in-package #:cl-user)
(defpackage #:rcv
  (:use #:cl #:clingon)
  (:export :display-results :main :rcv :tournament))
(in-package #:rcv)

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

(defun rank-prefs (top-choices)
  "Return the rankings of each ballot's top choice candidates."
  (let ((ranking '()))
    (dolist (choice top-choices)
      ;; Add a new cons pair if the key doesn't already exist.
      (pushnew (cons choice 0) ranking
	       :key #'(lambda (x) (equal (car x) choice)))
      ;; Increment the cons cdr
      (let ((existing (assoc choice ranking :test #'equal)))
	(setf (cdr existing) (1+ (cdr existing)))))
    ;; ranking))
    (sort ranking #'(lambda (a b)
		       (< (cdr a) (cdr b))))))

(defun eliminate (rankings ballots)
  "Eliminate the candidate with the fewest votes from the ballot."
  (let ((candidate (caar rankings)))
    (mapcar (lambda (ballot)
	      (remove-if (lambda (c) (string= c candidate)) ballot))
	      ballots)))

(defun add-eliminated (rankings candidates)
  "Add eliminated candidates to current round history with a vote count of zero."
  (dolist (candidate candidates)
    (pushnew (cons candidate 0) rankings
	     :key #'(lambda (x) (equal (car x) candidate))))
  (list rankings))

(defun has-majority (rankings)
  "Check if there is a majority winner in the rankings."
  (let ((total-votes (reduce #'+ rankings :key #'cdr)))
    (> (cdar (last rankings)) (/ total-votes 2))))

(defun update-rhist (rankings candidates ranking-history)
  "Add a new entry to the ranking history including previously eliminated
candidates."
  (append ranking-history (add-eliminated rankings candidates)))

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

(defun tournament (ballots candidates &optional (ranking-history '()))
  "Tournament round."
  (let* ((top-choices (mapcar #'car ballots))
	 (rankings (rank-prefs top-choices))
	 (rhist (update-rhist rankings candidates ranking-history)))
    (if (has-majority rankings)
	;; Terminal case: print end results and/or return ranking hist
	(progn
	  (display-results rhist)
	  rhist)
	;; Move to next tourn round; eliminating last place candidates
	(tournament (eliminate rankings ballots)
		    candidates
		    rhist))))

(defun rcv (source)
  "Ranked choice voting.
1. Eliminate the candidate with the fewest votes.
2. If only one candidate remains, elect this candidate and stop.
3. Otherwise, go back to 1."
  (let* ((ballots (get-ballots source))
	 (candidates (get-candidates ballots)))
    (tournament ballots candidates)))

(defun cli/handler (cmd)
  "Command-line argument handler which enters rcv."
  (let ((args (clingon:command-arguments cmd)))
    (rcv (car args))))

(defun cli/options ()
  "A list of cli options."
  (list
   (clingon:make-option
    :string
    :description "A csv file containing rank choice ballots."
    :short-name #\f
    :long-name "file"
    :key :filename)))

(defun cli/command ()
  "Command-line entrypoint."
  (clingon:make-command
   :name "rcv"
   :description "Ranked choice voting."
   :version "0.1"
   :authors '("nshan651 <public@nshan651.com")
   :license "GPL-3.0"
   :options (cli/options)
   :handler #'cli/handler))

(defun main ()
  "Program entrypoint."
  (clingon:run (cli/command)))
