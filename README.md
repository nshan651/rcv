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
  nshan651 <public@nshan651.com

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

## Resources

- https://www.rcvresources.org/types-of-rcv

### Misc Notes

Unsorted notes.

- Formula for determining the threshold of votes each candidates need in a multi-seat election:
	- $C_{threshold} = \frac{1}{C_{seats} + 1} + 1$
	- i.e. in a 3-seat election, candidate A needs greater than 25% of the votes to win.
