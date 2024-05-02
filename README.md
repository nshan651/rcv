# rcv

Ranked Choice Voting in Lisp.

## Meta Notes

Collection of misc. thoughts and ideas to organize later.

- By default, rcv will assume all candidates in the ballots are eligible. To add a custom candidate list, pass in the `-c | --candidates` flag.
- Use `-n | --number-of-seats` to specify how many seats (winners) the election will have (default is 1).

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

The algorithm for IRV is as follows:

1. Eliminate the candidate with the fewest votes.
2. If only one candidate remains, elect this candidate and stop.
3. Otherwise, go back to 1."

This can be implemented with a single seat (one winner), or multiple seats.

### Block-Preferential Voting (BPV)

## Resources

- https://www.rcvresources.org/types-of-rcv
