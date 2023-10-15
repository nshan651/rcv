;;; Ranked choice voting program
;;; Instant Runoff Voting(IRV)
;;; The instant-runoff vote counting procedure is as follows:
;;;    1. Eliminate the candidate appearing as the first preference on the fewest ballots.
;;;    2. If only one candidate remains, elect this candidate and stop.
;;;    3. Otherwise go to 1.
;;; https://en.wikipedia.org/wiki/Instant-runoff_voting
;;; Python project for IRV: https://github.com/jontingvold/pyrankvote
;; (asdf:load-system :cl-csv)

(defpackage :rcv
  (:use :cl)
  (:export :ballots :main))

(in-package :rcv)

(ql:quickload "flexi-streams")
(ql:quickload "cl-csv")
(ql:quickload "clingon")

;; Vars for testing
(defvar r1 '(("Bush" . 5) ("Gore" . 3) ("Nader" . 2)))
(defvar r2 '(("Mike Wazowski (Dank)" . 100000) ("George W. Bush (Republican)" . 5) ("Al Gore (Democratic)" . 69) ("Ralph Nader (Green)" . 372)))

(defvar ballots2
  '(("Bush" "Nader" "Gore")
    ("Bush" "Nader" "Gore")
    ("Bush" "Nader")
    ("Bush" "Nader")
    ("Nader" "Gore" "Bush")
    ("Nader" "Gore")
    ("Gore" "Nader" "Bush")
    ("Gore" "Nader")
    ("Gore" "Nader")))

(defvar ballots
  (cl-csv:read-csv #P"/home/nick/git/rcv/data/ballots.csv"))

(defvar candidates (get-candidates ballots))

(defun get-candidates (ballots)
  "Get a list of all unique candidates on the ballot."
    (if (null ballots)
	'()
	(remove-duplicates (reduce #'append ballots :key #'identity) :test #'string=)))

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
    (if (and (>= (cdr (first rankings)) (/ total-votes 2))
	     (= (length rankings) 2))
	t
	nil)))

(defun compare-eliminated (rankings candidates)
  "Compare the current rankings with the list of candidates."
  (let ((complete-rankings (copy-list rankings))) 
    (dolist (candidate candidates)
      (unless (assoc candidate rankings :test #'string=)
	(nconc complete-rankings (list (cons candidate 0)))))
    complete-rankings))

(defun tournament (ballots)
  "Simulate several rounds of voting."
  (labels ((tourn-round (ballots round-num)
	     (format t "Round: ~a~%" round-num)
	     (let* ((rankings (rank-prefs ballots))
		    (complete-rankings (compare-eliminated rankings candidates)))
	       (if (has-majority rankings) 
		   (progn
		     (display-results complete-rankings)
		     complete-rankings)
		   (let* ((last-place (get-last rankings))
			  (result (elim-candidate last-place ballots)))
		     (display-results complete-rankings)
		     (tourn-round result (incf round-num)))))))
    (tourn-round ballots 1)))

(defun display-results (results)
  "Display the results for each round."
  (format t "Candidate                    Votes     Status~%")
  (format t "---------------------------  --------  --------~%")
  (dolist (pair results)
    (let* ((candidate (car pair))
	   (votes (cdr pair))
	   (hspace-col1 (- 29 (length candidate)))
	   (hspace-col2 (- 10 (length (format nil "~d" votes)))))
      (format t "~a~vA~a~vASTATUS~%" candidate hspace-col1 #\Space votes hspace-col2 #\Space))))

(defun main ()
  "Program entrypoint."
  ;; (sb-ext:disable-debugger)
  (tournament ballots))
