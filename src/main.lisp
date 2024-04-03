;;; Ranked choice voting program
;;; Instant Runoff Voting(IRV)
;;; The instant-runoff vote counting procedure is as follows:
;;;    1. Eliminate the candidate appearing as the first preference on the fewest ballots.
;;;    2. If only one candidate remains, elect this candidate and stop.
;;;    3. Otherwise go to 1.
;;; https://en.wikipedia.org/wiki/Instant-runoff_voting
;;; Python project for IRV: https://github.com/jontingvold/pyrankvote
;; (ql:quickload "cl-csv")
;; (asdf:load-system :cl-csv)

;; Vars for testing
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

(defun rank-top-choices (top-choices)
  "Return the rankings of each ballot's top choice candidates."
  (let ((ranking '()))
    (dolist (choice top-choices)
      ;; Add a new cons pair if the key doesn't already exist.
      (pushnew (cons choice 0) ranking :key #'car)
      ;; Increment the cons cdr
      (let ((existing (assoc choice ranking)))
	(setf (cdr existing) (1+ (cdr existing)))))
    ranking))

(rank-top-choices '(Bush Gore Bush Gore Nader Gore Yeet))

;; 1. Get each ballot's first choice candidate and add to a list
;; 2. Filter eliminated candidates from the top choices
;; 3. If there is a single candidate remaining, stop. Otherwise, continue the process
(defun tournament (ballots)
  (let* ((top-choices (mapcar #'car candidates))
	 (rankings (rank-top-choices top-choices)))
    rankings)
  )

(defun elimination (rankings))

(defun rcv (ballots)
  "Ranked choice voting.
1. Eliminate the candidate with the fewest votes.
2. If only one candidate remains, elect this candidate and stop.
3. Otherwise, go back to 1."
  (let ((candidates '()))
    (while (> (length candidates) 1)
	   (tournament top-choices)
	   )
    )
  )
