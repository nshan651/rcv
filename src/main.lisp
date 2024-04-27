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
  (if (and (not (null source)) (probe-file source))
      ;; Parse csv.
      (remove-empty (cl-csv:read-csv (parse-namestring source)))

      (let ((lines '()))
	(format t "READ STDIN")
        ;; Parse stdin.
        (loop for line = (read-line *standard-input* nil)
	      while line
	      do (push line lines))
        ;; Split each line into a list of values
        (mapcar #'(lambda (line) (cl-ppcre:split #\, line)) lines)
	(remove-empty lines))
      ))

(defun get-candidates (ballots)
  "Get a list of candidates, derived from the submitted ballots."
  (remove-duplicates (reduce #'append ballots :key #'identity)
		     :test #'string=))

(defun remove-empty (ballots)
  "Remove nil values and empty strings from ballots."
  (mapcar #'(lambda (ballot)
	      (remove-if #'(lambda (x)
			     (or (null x) (string= x "")))
			 ballot))
	  ballots))

(defun rank-prefs (top-choices)
  "Return the rankings of each ballot's top choice candidates."
  (let ((ranking '()))
    (dolist (choice top-choices)
      ;; Add a new cons pair if the key doesn't already exist.
      (pushnew (cons choice 0) ranking :key #'car)
      ;; Increment the cons cdr
      (let ((existing (assoc choice ranking)))
	(setf (cdr existing) (1+ (cdr existing)))))

    ;; Return a sorted assoc list of candidates
    (sort ranking #'(lambda (a b) (> (cdr a) (cdr b))))))

(defun get-last (alist)
  "Find the last place candidate."
  (car (car (last alist))))

(defun eliminate (rankings ballots)
  "Eliminate the candidate with the fewest votes from the ballot."
  (let ((candidate (get-last rankings)))
    (mapcar (lambda (ballot)
	      (remove-if (lambda (c) (string= c candidate)) ballot))
	      ballots)))

(defun add-eliminated (rankings candidates)
  "Add eliminated candidates to current round history with a vote count of zero."
  (let ((complete-rankings (copy-list rankings)))
    (dolist (candidate candidates)
      (unless (assoc candidate rankings :test #'string=)
	(nconc complete-rankings (list (cons candidate 0)))))
    complete-rankings))

(defun has-majority (rankings)
  "Check if there is a majority winner in the rankings."
  (let ((total-votes (reduce '+ rankings :key #'cdr)))
    (if (and (>= (cdr (first rankings)) (/ total-votes 2))
	     (= (length rankings) 2))
	t
	nil)))

(defun update-ranking-history (candidates rankings ranking-history)
  "Add the current rankings to the history data structure."
  (append ranking-history
	  ;; Add back in previously eliminated candidates with vote total 0.
	  (list (add-eliminated rankings candidates))))

(defun tournament (ballots candidates &optional (counter 0) (ranking-history '()))
  "Ranked choice voting.
1. Eliminate the candidate with the fewest votes.
2. If only one candidate remains, elect this candidate and stop.
3. Otherwise, go back to 1."
  (let* ((top-choices (mapcar #'car ballots))
	 (rankings (rank-prefs top-choices))
	 (rhist (update-ranking-history candidates rankings ranking-history)))
    (if (has-majority rankings)
	;; Terminal case: print end results and/or return ranking hist
	(progn
	  (display-results rhist)
	  rhist)
	;; Move to next tourn round; eliminating last place candidates
	(tournament
	 (eliminate rankings ballots)
	 candidates
	 (incf counter)
	 rhist))))

(defun display-results (ranking-history)
  "Print out the ranking history."
  (dolist (round ranking-history)
    (format t "Candidate                       Votes
------------------------------  ---------~%")
    (dolist (rankpair round)
      (let* ((candidate (car rankpair))
	     (votes (cdr rankpair))
	     (hspace-col1 (- 32 (length candidate)))
	     ;; (hspace-col2 (- 10 (length (format nil "~d" votes))))
	     )
	(format t "~a~vA~a~%" candidate hspace-col1 #\Space votes)))
      (format t "~%")))

(defun print-ballots (ballots)
  "Helper to print ballots."
  (dolist (ballot ballots)
    (dolist (candidate ballot)
      (format t "~a -> " candidate))
    (format t "~%")
  (format t "Col len: ~a~%" (length ballot))
  ))

(defun rcv (cmd)
  "Handler function for top-level cli command."
  (let ((args (clingon:command-arguments cmd)))
    (let* ((ballots (get-ballots (car args)))
	   (candidates (get-candidates ballots)))
      (tournament ballots candidates 1 '()))
    )
  )

(defun cli/command ()
  "Command-line entrypoint."
  ;;; Add -h and -v as shorthands by updating default options.
  ;; (cli/update-defaults)
  (clingon:make-command
   :name "rcv"
   :description "Ranked choice voting."
   :version "0.1"
   :authors '("nshan651 <public@nshan651.com")
   :license "GPL-3.0"
   :options (list
	     (clingon:make-option
	      :string
	      :description "A csv file containing rank choice ballots."
	      :short-name #\f
	      :long-name "file"
	      :key :filename))
   :handler #'rcv))

(defun main ()
  "Program entrypoint."
  (clingon:run (cli/command)))
