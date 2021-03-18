# HydraGame

Implementation of the Hydra Game described in the paper
_Accessible independence results for Peano Arithmetic_
by L. Kirby and J. Paris. A good explanation of the rules can be found
[here](https://slate.com/human-interest/2014/06/hydra-game-an-example-of-a-counterintuitive-mathematical-result.html).

## How to use

Use `leftclick` to select a node to chop off, scroll to om in or out, and
`ctrl`+`leftclick` to drag the picture.

## Installation

Install Haskell's tool [Stack](https://docs.haskellstack.org/en/stable/README/). Then,
inside the repo's main folder run

```
stack setup
stack build
stack exec hydra
```
