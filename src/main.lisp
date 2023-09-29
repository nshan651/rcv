;;; Ranked choice voting program
;;; Instant Runoff Voting(IRV)
;;; The instant-runoff vote counting procedure is as follows:
;;;    1. Eliminate the candidate appearing as the first preference on the fewest ballots.
;;;    2. If only one candidate remains, elect this candidate and stop.
;;;    3. Otherwise go to 1.
;;; https://en.wikipedia.org/wiki/Instant-runoff_voting
;;; Python project for IRV: https://github.com/jontingvold/pyrankvote

;; Define the candidates
;; (defvar candidates 
;;     '("Bob" "Leeroy" "Sally" "Joel" "Hilary" "Archibold"))

(defvar in-list '("Bush" "Bush" "Bush" "Bush" "Nader" "Nader" "Gore" "Gore" "Gore"))

(defvar ballots
  '(("Bush" "Nader" "Gore")
    ("Bush" "Nader" "Gore")
    ("Bush" "Nader")
    ("Bush" "Nader")
    ("Nader" "Gore" "Bush")
    ("Nader" "Gore")
    ("Gore" "Nader" "Bush")
    ("Gore" "Nader")
    ("Gore" "Nader")))

(defun rank-prefs (ballots)
  "Rank the top preferences for a given round."
  (let ((vote-tbl '())				; Assoc list of candidates and votes.
	(top-candidates (mapcar 'car ballots))) ; First choice prefs.
    (dolist (candidate top-candidates)
      (let ((existing (assoc candidate vote-tbl :test #'string=)))
        (if existing
            (setf (cdr existing) (+ (cdr existing) 1)) ; Increment the count
            (push (cons candidate 1) vote-tbl))))
    (sort vote-tbl #'(lambda (a b) (> (cdr a) (cdr b))))))

(defun elim-candidate (target ballots)
  "Remove :candidate from each sublist in a list of lists."
  (mapcar (lambda (ballot)
            (remove-if (lambda (candidate)
                         (string= candidate target)) ballot))
          ballots))

(defun get-last (alist)
  "Find the last place candidate."
  (car (car (last alist))))

(defun has-majority (rankings)
  "Check if there's a majority winner in the rankings."
  (let ((total-votes (reduce '+ rankings :key #'cdr)))
    (format t "Highest ranked: ~a~%Total: ~a" (cdr (first rankings)) total-votes)
    (if (and (>= (cdr (first rankings)) (/ total-votes 2))
	     (= (length rankings) 2))
	t
	nil)))

(defun tournament (ballots)
  "Simulate several rounds of voting."
  (format t "starting tourn~%")
  (labels ((tourn-round (ballots)
	     (format t "entering tourn~%")
	     (let ((rankings (rank-prefs ballots)))
	       (if (has-majority rankings) 
		   rankings
		   (let* ((last-place (get-last rankings))
			  (result (elim-candidate last-place ballots)))
		     (tourn-round result))))))
	   (tourn-round ballots)))

(defun display-results ()
  "Display the results for each round."
  ;; (format t "~a"
  (format nil 
	  "ROUND 1 ~
    Candidate                      Votes  Status~
    ---------------------------  -------  --------~%
    George W. Bush (Republican)        4  Hopeful~%
    Al Gore (Democratic)               3  Hopeful~%
    Ralph Nader (Green)                2  Rejected~%
    FINAL RESULT~%
    Candidate                      Votes  Status~%
    ---------------------------  -------  --------~%
    Al Gore (Democratic)               5  Elected~%
    George W. Bush (Republican)        4  Rejected~%
    Ralph Nader (Green)                0  Rejected"))
