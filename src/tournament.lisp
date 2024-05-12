;;;; Ranked choice voting
(in-package #:cl-user)
(defpackage #:rcv/tournament
  (:use #:cl)
  (:export :display-results :tournament))
(in-package #:rcv/tournament)

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

(defun find-majority-winners (rankings seats)
  "Check for majority winner(s) in the rankings."
  (let* ((total-votes (reduce #'+ rankings :key #'cdr))
	 ;; Use the droop quote formula.
	 (threshold (1+ (/ total-votes (1+ seats)))))
    (format t "THRESHOLD: ~A~%" threshold)
    (remove-if #'(lambda (x) (< (cdr x) threshold))
	       rankings)))

(defun update-rhist (rankings candidates ranking-history)
  "Add a new entry to the ranking history including previously eliminated
candidates."
  (append ranking-history (add-eliminated rankings candidates)))

(defun tournament (ballots candidates seats &optional elected-candidates ranking-history)
  "Tournament round."
  (let* ((top-choices (mapcar #'car ballots))
	 (rankings (rank-prefs top-choices))
	 (rhist (update-rhist rankings election)))

    (let* ((elected (find-majority-winners rankings seats))
	   (remaining-candidates
	     (remove-if #'(lambda (x) (member x elected)) rankings)))

      (format t "ELECTED: ~A~%" elected)
      (format t "REMAINING: ~A~%" remaining-candidates)
      ;; (if (and elected (= seats (length elected)))
      (if (or (= 5 (length rhist))
	   (and elected (= seats (length elected))))
	  ;; Terminal case: print end results and/or return ranking hist
	  (progn
	    (display-results rhist)
	    rhist)
	  ;; Move to next tourn round; eliminating last place candidates
	  (tournament (eliminate remaining-candidates ballots)
		      candidates
		      seats
		      elected
		      rhist)))))
