# rcv

Ranked Choice Voting in Lisp.

## Usage

```sh
NAME:
  rcv - Ranked choice voting.

USAGE:
  rcv [options] [arguments ...]

OPTIONS:
      --irv                      Instant runoff vote.
      --minimax                  Minimax vote.
      --pbv                      Preferential block vote
      --stv                      Single transferable vote.
  -c, --candidates <VALUE>       Specify a mapping of eligible candidates and their affiliated parties.
                                 By default, rcv will use the set of all candidates in the ballot.
  -h, --help                     Display usage information and exit.
  -n, --number-of-seats <VALUE>  Specify how many seats (winners) the election will have. [default: 1]
  -v, --version                  Display version and exit.

AUTHORS:
  nshan651 <public@nshan651.com>

LICENSE:
  GPL-3.0
```

## Types of Ranked-Choice Voting

### Instant Runoff Voting (IRV)

The algorithm for [IRV](https://en.wikipedia.org/wiki/Instant-runoff_voting) is as follows:

1. Eliminate the candidate with the fewest votes.
2. If only one candidate remains, elect this candidate and stop.
3. Otherwise, go back to 1.

This can be implemented with a single seat (one winner), or multiple seats.

### Minimax

[Minimax Condorcet method](https://en.wikipedia.org/wiki/Minimax_Condorcet_method)

### Preferential Block Voting (PBV)

[Preferential block voting](https://en.wikipedia.org/wiki/Preferential_block_voting)

### Single Transferable Voting (STV)

[Single transferable voting](https://en.wikipedia.org/wiki/Single_transferable_vote)

1. Find candidates that exceed the quota and elect them.
2. Count surplus votes of elected candidate(s).
3. Transfer surplus votes to the next choice of sizeof(surplus) ballots who elected the winner with their first choice.
  - e.g. 8 votes go to candidate A with a surplus of 2. Suppose candidate B was listed by 5 of the voters who elected candidate A. 2 votes are then transferred to candidate B.
  - Even if 3 voters of candidate A selected candidate C as their second choice, the votes would still be transferred to B as they had the majority preference of 5.
4. If seats still remain to be filled, there are no surplus votes to transfer, and none of the remaining candidates have reached the quota, the last place candidate is eliminated.
5. Votes for the last place candidate are transferred to the next-preferred candidate:
  - If the next-preferred candidate has been eliminated or elected, the procedure is iterated to lower-ranked candidates.

## Resources

- https://www.rcvresources.org/types-of-rcv

### Misc Notes

Unsorted notes.

- Formula for determining the threshold of votes each candidates need in a multi-seat election:
	- $C_{threshold} = \frac{1}{C_{seats} + 1} + 1$
	- i.e. in a 3-seat election, Candidate A needs greater than 25% of the votes to win.
