(in-package #:cl-user)
(defpackage #:rcv/t/main
  (:use #:cl
	#:fiveam
        #:rcv/stdin
	#:rcv/tournament))
(in-package #:rcv/t/main)

(def-suite test-rcv
  :description "Test rank choice voting.")
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
    (Gore Nader Yeet))
  "Define a nested list of ballots of type symbol.")

(defvar *ballots-f*
  (rcv/stdin:get-ballots "./data/ballots.csv")
  "Read a nested list of ballots from a file.")

(defvar *candidates*
  (rcv/stdin:get-candidates *ballots-f*)
  "Obtain a list of eligible candidates, derived from the ballots.")

(fiveam:test irv-1
  (let ((res (tournament *ballots-f* *candidates* 1)))
    (fiveam:is
     (equal res
	    '((("Yeet" . 0) ("Nader" . 2) ("Gore" . 3) ("Bush" . 4))
	      (("Yeet" . 0) ("Nader" . 0) ("Bush" . 4) ("Gore" . 5)))))))

(fiveam:run!)
