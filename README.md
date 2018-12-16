# Algebraic Effects vs Monads in theory and practice

This repo contains slides and code from my seminar talk. For the best experience:
* Read the slides.
* Play with the code in the following order:
  * Interpreter/Pure/ - three interpreters for CBV lambda calculus with integers and addition written in Haskell, Idris and Koka. Compare these to get acquainted with the languages.
  * Interpreter/OneEffect - interpreters for LC with added error messages, operation count, nondeterminism and logging, taken from Wadler's "The essence of functional programming". First see the Haskell version for the "traditional" monadic approach, then the Idris version for poor man's algebraic effects version (using Idris' Effect library) and then the Koka version for full-blown algebraic effects.
  * Interpreter/AllEffects - interpreters for LC with all the above effects combined. In order, read:
    * All_Monads.hs - to see why monads don't compose too well.
    * All_Transformers.hs - for a solution using monad transformers.
    * All_Classes.hs - for a solution using typeclasses like MonadError, MonadState etc.
    * All.idr - the Idris solution showing that Idris's effects are not that easy to use.
    * All.kk - Koka solution which shows that built-in algebraic effects are awesome.
  * Free - a free monad example in Haskell (the package free is required for this) and a showcase for how much easier the same thing is in Koka.

## TODO

* Figure out how to run the all-effect interpreter in Idris. Finish writing comments.
* Figure out how to get final state in Count.idr
* Rewrite the simple interpreters not the use class operations like get, put, tell etc.