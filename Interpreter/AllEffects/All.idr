import Effects
import Effect.State
import Effect.Exception
import Effect.Select

import Control.Monad.Writer
import Control.Monad.Identity

-- The LOG effect is defined the same way as in Out.idr
data Log : Type -> Effect where
    Tell : a -> Log a () (List a) (\_ => List a)

LOG : Type -> EFFECT
LOG t = MkEff (List t) (Log t)

tell : a -> Eff () [LOG a]
tell x = call (Tell x)

implementation Handler (Log a) (WriterT (List a) Identity) where
    handle rest (Tell msg) k = do
        tell [msg]
        k () rest

Name : Type
Name = String

data Term = Var Name
          | Const Int
          | Add Term Term
          | Lam Name Term
          | App Term Term
          | Count
          | Fail
          | Amb Term Term
          | Out Term

-- The type of our computations is as verbose as the transformers one and less
-- verbose that all class constraints together and it is better than Haskell's
-- class solution in that we can log Values and not just Strings. The order in
-- which effects are performed is not determined now, but only when we will run
-- our interpreter.
data Value = Wrong
           | Num Int
           | Fun (Value -> Eff Value [STATE Int, EXCEPTION String, SELECT, LOG Value])

Env : Type
Env = List (Name, Value)

-- Unlike with transformers and just like with classes, we can only mention
-- the effects that we are really using in a given function.
lookupEnv : Name -> Env -> Eff Value [EXCEPTION String]
lookupEnv x [] = raise $ "Variable " ++ x ++ " not bound!"
lookupEnv x ((y, v) :: env) = if x == y then pure v else lookupEnv x env

-- Operations like update (and also raise that we used above) are defined for
-- all computations that support the given effect. They are similar to class
-- operations like throwError or modify from Haskell.
tick : Eff () [STATE Int]
tick = update (+1)

add : Value -> Value -> Eff Value [STATE Int, EXCEPTION String]
add (Num n) (Num m) = tick *> pure (Num (n + m))
add _ _ = raise "Can't add!"

-- Of course we can create aliases for things that have many effects.
Computation : Type -> Type
Computation t = Eff t [STATE Int, EXCEPTION String, SELECT, LOG Value]

apply : Value -> Value -> Computation Value
apply (Fun f) x = tick *> f x
apply _ _ = raise "Can't apply!"

-- Implementing the interpreter is very easy. It is most similar to Haskell's
-- class solution.
interp : Term -> Env -> Computation Value
interp (Var x) env = lookupEnv x env
interp (Const n) _ = pure (Num n)
interp (Add t1 t2) env = do
    n1 <- interp t1 env
    n2 <- interp t2 env
    add n1 n2
interp (Lam x t) env = pure $ Fun (\a => interp t ((x, a) :: env))
interp (App t1 t2) env = do
    f <- interp t1 env
    x <- interp t2 env
    apply f x
interp Count _ = do
    n <- get
    pure $ Num n
interp Fail _ = select []
interp (Amb t1 t2) env = do
    b <- select [True, False]
    if b then interp t1 env else interp t2 env
interp (Out t) env = do
    v <- interp t env
    tell v
    pure v

Show Term where
    show (Var x) = x
    show (Const n) = show n
    show (Add t1 t2) = show t1 ++ " + (" ++ show t2 ++ ")"
    show (Lam x t) = "λ" ++ x ++ "." ++ show t
    show (App t1 t2) = "(" ++ show t1 ++ ")(" ++ show t2 ++ ")"
    show (Count) = "Count"
    show Fail = "Fail"
    show (Amb t1 t2) = "Amb (" ++ show t1 ++ ") (" ++ show t2 ++ ")"
    show (Out t) = "Out (" ++ show t ++ ")"

Show Value where
    show Wrong = "<wrong>"
    show (Num n) = show n
    show (Fun f) = "<function>"

-- To run our interpreter, we need to find an adequate computational context.
-- We can handle STATE in any context, ERROR using Maybe or Either, SELECT
-- with List and LOG with WriterT.
-- But, to be honest, doing this correctly was very difficult for me. This
-- means that Idris' effects don't compose as well as they in theory should.
--
-- EXERCISE: Find out a way to run the interpreter. Fill the hole ?t with
-- something sensible.

-- By the way, in Idris programs can contain holes, named with ? at the start,
-- which allow the programmer to postpone implementing some part of a program.
-- Running a program with a hole will result in failure. Holes are much nicer
-- than using undefined in Haskell for the same purpose.
test : Term -> String
test t = ?t

term0 : Term
term0 = App (Lam "x" (Add (Var "x") (Var "x")))
            (Add (Const 10) (Const 11))

count_term0 : Term
count_term0 = Add Count (Add Count Count)

count_term1 : Term
count_term1 = Add (Add Count Count) Count
            
error_term0 : Term
error_term0 = Var "O BOŻE TO JEST ZBOŻE"

failamb_term0 : Term
failamb_term0 = Add (Const 42) Fail

failamb_term1 : Term
failamb_term1 = Amb (Const 100) (Const 12345)

out_term0 : Term
out_term0 = Out (Add (Out (Const 42)) (Out (Const 54321)))

term1 : Term
term1 =
    Amb
        (Add
            (Amb count_term0 out_term0)
            (Amb failamb_term0 out_term0))
        (Add term0 count_term1)

testTerms : List Term
testTerms =
    [term0, count_term0, count_term1, error_term0, failamb_term0, failamb_term1, out_term0, term1]

main : IO ()
main = do
    for_ testTerms $ \t => do
        putStrLn $ cast $ replicate 50 '-'
        putStrLn $ "Interpreting " ++ show t
        putStrLn $ test t
        putStrLn $ cast $ replicate 50 '-'