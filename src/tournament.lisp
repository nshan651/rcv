;;;; Ranked choice voting

(in-package #:cl-user)
(defpackage #:rcv.tournament
  (:use #:cl)
  (:export :display-results :tournament))
(in-package #:rcv.tournament)

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
  ;;; TODO: Handle ties!!!
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
  "Check for majority winner(s) in the rankings."
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

(defun add-party (candidates)
  "Get a mapping of the candidate and their respective party."
  (mapcar #'(lambda (c) (cons c 'Unkown)) candidates))

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
