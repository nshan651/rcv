# rcv

Ranked Choice Voting in Lisp.

## Meta Notes

Collection of misc. thoughts and ideas to organize later.

- By default, rcv will assume all candidates in the ballots are eligible. To add a custom candidate list, pass in the `-c | --candidates` flag.

## Types of Ranked-Choice Voting

### Instant Runoff Voting (IRV)

The algorithm for IRV is as follows:

1. Eliminate the candidate with the fewest votes.
2. If only one candidate remains, elect this candidate and stop.
3. Otherwise, go back to 1."

This can be implemented with a single seat (one winner), or multiple seats.

### Block-Preferential Voting (BPV)

## Resources

- https://www.rcvresources.org/types-of-rcv
