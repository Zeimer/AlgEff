import Effects

import Control.Monad.Writer
import Control.Monad.Identity

-- Idris has no built-in LOG effect (or rather it has, but a more complicated
-- than we need), so we have to make one ourselves. First, we have to know what
-- an Effect is. We can check this using the command :doc Effect or
-- :printdef Effect 
--
-- Effect : Type
-- Effect = (x : Type) -> Type -> (x -> Type) -> Type
--
-- x is the return type of the computation.
--
-- The second type is the input resource, because in Idris effects are tied to
-- resources. For example, the resource tied to the STATE effect is... well,
-- the state itself (represented as a value of appropriate type).
--
-- The third thing, (x -> Type), is a family of types parametrized by x, i.e.
-- if we give it something of type x, it gives us back a type. It represents
-- a computation that is run on the input resource and that gives us back the
-- output resource. For the STATE effect, for example, we could change the type
-- of state by using the put operation.

data Log : Type -> Effect where
    Tell : a -> Log a () (List a) (\_ => List a)

-- Thus, the declaration
--
-- data Log : Type -> Effect
--
-- means in fact
--
-- Log : Type -> (x : Type) -> Type -> (x -> Type) -> Type
--
-- We want the resource of our Log effect to be a list of messages, where the
-- messages may be of any type a, not just String. We want the output resource
-- to be the same, so we put here (\_ => List a), a type family that always
-- returns List a.
--
-- We want only one operation, Tell, which takes a message of type a, returns ()
-- and whose input and output resource is List a, the list of messages that we will
-- append to.

LOG : Type -> EFFECT
LOG t = MkEff (List t) (Log t)

-- The above Log data type has to be promoted to a 'concrete' effect (as the
-- documentation under :doc EFFECT says). Thus in our code we will write our effect
-- as [LOG a].

tell : a -> Eff () [LOG a]
tell x = call (Tell x)

-- We have to write some boilerplate to promote the Tell operation to Eff,
-- an applicative (in the sense of applicative functors) environment in which
-- effects are run.
--
-- Eff () [LOG a] means that our tell function returns () (just as the
-- Tell operation) and has the effect LOG a, which is the promoted
-- version of Log.

implementation Handler (Log a) (WriterT (List a) Identity) where
    handle rest (Tell msg) k = do
        tell [msg]
        k () rest

-- To be able to use our effect, we need to write a handler for it. A handler
-- is a function that can run the effectful computation in some computational
-- context. For example, the state effect can be used in any context, because
-- a cell of state of type s can be simulated using the type s -> (a, s). This
-- is how the state monad is implemented in Haskell.
--
-- In our case, logging can't be used in any context. Why is that? Even though
-- logging messages of type w can be simulated using the type (a, [w]) (and
-- this is how the Writer monad is implemented in Haskell), running it in the
-- Identity context (Identity is, as the name suggests, an identity functor/monad)
-- would make us unable to access the log we produced.
--
-- Our handler will therefore only handle logging in the Writer monad. For
-- technical reasons we need to write WriterT (List a) Identity instead of the
-- simpler Writer (List a). This is because "Implementation arguments must be
-- type or data constructors" and Writer a is just a synonym for
-- WriterT a Identity, which makes Idris unhappy.

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

lookupEnv : Name -> Env -> Value
lookupEnv x [] = Wrong
lookupEnv x ((y, v) :: env) = if x == y then v else lookupEnv x env

add : Value -> Value -> Value
add (Num n) (Num m) = Num (n + m)
add _ _ = Wrong

apply : Value -> Value -> Eff Value [LOG Value]
apply (Fun f) x = f x
apply _ _ = pure Wrong

-- We interpret Out t using the function tell.
interp : Term -> Env -> Eff Value [LOG Value]
interp (Var x) env = pure $ lookupEnv x env
interp (Const n) _ = pure $ Num n
interp (Add t1 t2) env = do
    n1 <- interp t1 env
    n2 <- interp t2 env
    pure $ add n1 n2
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
    show (App t1 t2) = "(" ++ show t1 ++ ")(" ++ show t2 ++ ")"
    show (Out t) = "Out (" ++ show t ++ ")"

Show Value where
    show Wrong = "<wrong>"
    show (Num n) = show n
    show (Fun _) = "<function>"

-- To run our computation, we call run. This produces a computation in some
-- applicative context m. Using the (Writer (List Value) Value) we set this
-- context to the Writer monad. Then we run this monad by calling runWriterT
-- and runIdentity (sadly, there's no runWriter).
test : Term -> String
test t =
    case runIdentity $ runWriterT $ the (Writer (List Value) Value) $ run (interp t []) of
        (value, log) =>
            "log: " ++ show log ++ "\n" ++
            "result: " ++ show value

term0 : Term
term0 = App (Lam "x" (Add (Var "x") (Var "x")))
            (Add (Const 10) (Const 11))

out_term0 : Term
out_term0 = Out (Add (Out (Const 42)) (Out (Const 54321)))

testTerms : List Term
testTerms = [term0, out_term0]

main : IO ()
main = do
    for_ testTerms $ \t => do
        putStrLn $ cast $ replicate 50 '-'
        putStrLn $ "Interpreting " ++ show t
        putStrLn $ test t
        putStrLn $ cast $ replicate 50 '-'