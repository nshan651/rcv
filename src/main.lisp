;;;; Ranked choice voting program
;;;; Instant Runoff Voting(IRV)
;;;; The instant-runoff vote counting procedure is as follows:
;;;;    1. Eliminate the candidate appearing as the first preference on the fewest ballots.
;;;;    2. If only one candidate remains, elect this candidate and stop.
;;;;    3. Otherwise go to 1.
;;;; https://en.wikipedia.org/wiki/Instant-runoff_voting
;;;; Python project for IRV: https://github.com/jontingvold/pyrankvote

;; (in-package #:cl-user)
(defpackage #:rcv
  (:use #:cl)
  (:export :ballots :display-results :complete-rankings :main))

;; Paste this into a repl to switch namespaces
(in-package #:rcv)

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
    ("Gore" "Nader" "Yeet")))

(defvar *ballots*
  (cl-csv:read-csv #P"/home/nick/git/rcv/data/ballots.csv"))

(defvar *candidates*
  (remove-duplicates (reduce #'append *ballots* :key #'identity)
		     :test #'string=))

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

;;; TODO: Revise this to be more concise/efficient
(defun add-eliminated (rankings candidates)
  "Compare the current rankings with the list of candidates."
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

(defun rcv (ballots candidates counter ranking-history)
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
	  (format t "TODO: show final results here~%")
	  (format t "counter: ~A~%" counter)
	  rhist)
	;; Move to next tourn round; eliminating last place candidates
	(rcv
	 (eliminate rankings ballots)
	 candidates
	 (incf counter)
	 rhist))))

(defun main ()
  "Program entrypoint."
  (rcv ballots *candidates* 1 '() )

  )
