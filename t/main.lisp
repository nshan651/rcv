(in-package #:cl-user)
(defpackage #:rcv/t/main
  (:use #:cl
	#:fiveam
	#:rcv))
(in-package #:rcv/t/main)

(def-suite test-rcv
  :description "Test my system")
(in-suite test-rcv)

(defvar *ballots-str*
  '(("Bush" "Nader" "Gore")
    ("Bush" "Nader" "Gore")
    ("Bush" "Nader")
    ("Bush" "Nader")
    ("Nader" "Gore" "Bush")
    ("Nader" "Gore")
    ("Gore" "Nader" "Bush")
    ("Gore" "Nader")
    ("Gore" "Nader" "Yeet"))
  "Define a nested list of ballots of type string.")

(defvar *ballots-sym*
  '((Bush Nader Gore)
    (Bush Nader Gore)
    (Bush Nader)
    (Bush Nader)
    (Nader Gore Bush)
    (Nader Gore)
    (Gore Nader Bush)
    (Gore Nader)
    (Gore Nader Yeet)))
  ;; "Define a nested list of ballots of type symbol.")

(defvar *ballots*
  (cl-csv:read-csv (parse-namestring "data/ballots.csv")))

(defvar *candidates*
  (remove-duplicates (reduce #'append *ballots* :key #'identity)
		     :test #'string=))

(fiveam:test irv-1
  (let ((res (tournament *ballots* *candidates*)))
    (fiveam:is
     (equal res
	    '((("Bush" . 4) ("Gore" . 3) ("Nader" . 2) ("Yeet" . 0))
	      (("Gore" . 5) ("Bush" . 4) ("Nader" . 0) ("Yeet" . 0)))))))

(fiveam:run!)
