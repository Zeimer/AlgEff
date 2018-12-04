import Effects

import Control.Monad.Writer

-- Idris has no built-in LOG effect, co we have to make one ourselves.
-- First, we have to know what an Effect is. We can check this using
-- the command :doc Effect or :printdef Effect 
--
-- Effect : Type
-- Effect = (x : Type) -> Type -> (x -> Type) -> Type
--
-- x is the return type of the computation.
--
-- The second type is the input resource, because in Idris effects are tied to
-- resources. For example, the resource tied to the STATE effect is... well,
-- the state itself.
--
-- The third thing, (x -> Type), is a family of types parametrized by x, i.e.
-- if we give it something of type x, it gives us back a type. It represents
-- a computation that is run on the input resource and that gives us back the
-- output resource. For the STATE effect, for example, we could change the type
-- of state we have using the put operation.

data Log : Type -> Effect where
    Tell : a -> Log a () (List a) (\_ => List a)

-- Thus, the declaration
--
-- data Log : Effect
--
-- means in fact
--
-- Log : (x : Type) -> Type -> (x -> Type) -> Type
--
-- We want the resource of our Log effect to be a list of messages, where the
-- messages may be of any type a, not just String. We want the output resource
-- to be the same, so we put here (\_ => List a), a type family that always
-- returns List a.
--
-- We want only one operation, Tell, which takes a message of type a, returns () and
-- has the Log effect, whose input and output resource is List a, the list of messages
-- that we will append to.

LOG : Type -> EFFECT
LOG t = MkEff (List t) (Log t)

-- The Log data type of above has to be promoted to a 'concrete' effect (as the
-- documentation under :doc EFFECT says). Thus in our code we will write our effect
-- as LOG (List a).

tell : a -> Eff () [LOG a]
tell x = call (Tell x)

-- We have to write some boilerplate to promote the Tell operation to Eff,
-- a monad which is an environment in which effects are run.
--
-- Eff () [LOG (List a)] means that our tell function returns () (just as the
-- Tell operation) and has the effect LOG (List a), which is the promoted
-- version of Log.

Name : Type
Name = String

data Term = Var Name
            | Const Int
            | Add Term Term
            | Lam Name Term
            | App Term Term
            | Out Term

-- Our function values are represented as Value -> Eff Value [LOG Value],
-- which means that they take a value and return a value while possibly appending
-- some values to the log.
data Value = Wrong
            | Num Int
            | Fun (Value -> Eff Value [LOG Value])

Env : Type
Env = List (Name, Value)

lookupEnv : Name -> Env -> Eff Value [LOG Value]
lookupEnv x [] = pure Wrong
lookupEnv x ((y, v) :: env) = if x == y then pure v else lookupEnv x env

add : Value -> Value -> Eff Value [LOG Value]
add (Num n) (Num m) = pure $ Num (n + m)
add _ _ = pure Wrong

apply : Value -> Value -> Eff Value [LOG Value]
apply (Fun f) x = f x
apply _ _ = pure Wrong

-- We interpret Out t using the function tell.
interp : Term -> Env -> Eff Value [LOG Value]
interp (Var x) env = lookupEnv x env
interp (Const n) _ = pure $ Num n
interp (Add t1 t2) env = do
    n1 <- interp t1 env
    n2 <- interp t2 env
    add n1 n2
interp (Lam x t) env = pure $ Fun (\a => interp t ((x, a) :: env))
interp (App t1 t2) env = do
    f <- interp t1 env
    x <- interp t2 env
    apply f x
interp (Out t) env = do
    v <- interp t env
    tell v
    pure v

Show Term where
    show (Var x) = x
    show (Const n) = show n
    show (Add t1 t2) = show t1 ++ " + (" ++ show t2 ++ ")"
    show (Lam x t) = "λ" ++ x ++ "." ++ show t
    show (App t1 t2) = "(" ++ show t1 ++ ")" ++ show t2
    show (Out t) = "Out (" ++ show t ++ ")"

Show Value where
    show Wrong = "<wrong>"
    show (Num n) = show n
    show (Fun _) = "<function>"

Handler (Log a) m where
    handle xs (Tell x) k = k () (x :: xs)

-- TODO
test : Term -> String
test t = ?a
    
--        (v, w) -> "log: " ++ show w ++ "\nresult: " ++ show v

term0 : Term
term0 = App (Lam "x" (Add (Var "x") (Var "x")))
            (Add (Const 10) (Const 11))

out_term0 : Term
out_term0 = Out (Add (Out (Const 42)) (Out (Const 23456789)))

testTerms : List Term
testTerms = [term0, out_term0]

main : IO ()
main = do
    for_ testTerms $ \t => do
        putStrLn $ "Interpreting " ++ show t
        putStrLn $ test t