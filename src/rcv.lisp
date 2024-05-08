;;;; Ranked choice voting.
;;;; https://en.wikipedia.org/wiki/Instant-runoff_voting
;;;; Python project for IRV: https://github.com/jontingvold/pyrankvote

(in-package #:cl-user)
(defpackage #:rcv
  (:use #:cl #:clingon)
  (:import-from :rcv/tournament
   :tournament)
  (:import-from :rcv/stdin
   :get-ballots
   :get-candidates)
  (:export :main :rcv))
(in-package #:rcv)

(defun rcv (source seats)
  "Ranked choice voting.
1. Eliminate the candidate with the fewest votes.
2. If only one candidate remains, elect this candidate and stop.
3. Otherwise, go back to 1."
  (let* ((ballots (get-ballots source))
	 (candidates (get-candidates ballots)))
    (tournament ballots candidates seats)))

(defun cli/handler (cmd)
  "Command-line argument handler which enters rcv."
  (let ((args (clingon:command-arguments cmd))
	(seats (parse-integer (clingon:getopt cmd :seats))))
    (rcv (car args) seats)))

(defun cli/options ()
  "A list of cli options."
  (list
   (clingon:make-option
    :string
    :description "Specify a mapping of eligible candidates and their affiliated parties. By default, rcv will use the set of all candidates in the ballot."
    :short-name #\c
    :long-name "candidates"
    :key :candidates)
   (clingon:make-option
    :string
    :description "Specify how many seats (winners) the election will have."
    :short-name #\n
    :long-name "number-of-seats"
    :initial-value '1
    :key :seats)
   (clingon:make-option
    :flag
    :description "Instant runoff vote."
    :long-name "irv"
    :key :irv)
   (clingon:make-option
    :flag
    :description "Minimax vote."
    :long-name "minimax"
    :key :minimax)
   (clingon:make-option
    :flag
    :description "Preferential block vote"
    :long-name "pbv"
    :key :pbv)
   (clingon:make-option
    :flag
    :description "Single transferable vote."
    :long-name "stv"
    :key :stv)
))

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
