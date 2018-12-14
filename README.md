# Algebraic Effects vs Monads in theory and practice

This repo contains slides and code from my seminar talk. For the best experience:
* Read the slides
* Play with the code in the following order:
  * Interpreter/Pure/ - three interpreters for CBV lambda calculus with integers and addition written in Haskell, Idris and Koka. Compare these to get acquainted with the languages.
  * Interpreter/OneEffect - interpreters for LC with added errors, operation count, nondeterminism and logging, taken from Wadler's "The essence of functional programming". First see the Haskell version for the "traditional" monadic approach, then the Idris version for poor man's algebraic effects version (using Idris' Effect library) and then the Koka version for full-blown algebraic effects.
  * Interpreter/TwoEffects - interpreters for LC with both errors and operation count. First see ErrorCount_Monad.hs to see why monads don't compose too well. Then see ErrorCount_Trans.hs for a solution using monad transformers. The Idris and Koka versions show that algebraic effects compose better than monads or transformers.
  * Interpreter/AllEffects - interpreters for LC having all four effects mentioned.
  * Free - a free monad example in Haskell (the package free is required for this) and a showcase for how much easier the same is in Koka.

## TODO

Figure out how to run the all-effect interpreter in Idris. Finish writing comments.