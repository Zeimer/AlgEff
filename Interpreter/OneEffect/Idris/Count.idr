-- To use the Effects library, we first have to import Effects and then import
-- every single ffect from Effect.
import Effects
import Effect.State

Name : Type
Name = String

data Term = Var Name
          | Const Int
          | Add Term Term
          | Lam Name Term
          | App Term Term
          | Count

-- The simplest syntax for effectful computations is Eff r es, where r is the
-- return type of the computation and es is a list of effects.
-- We use the STATE Int effect.
data Value = Wrong
           | Num Int
           | Fun (Value -> Eff Value [STATE Int])

Env : Type
Env = List (Name, Value)

-- In Idris, for programming with effects we can use monadic style.
lookupEnv : Name -> Env -> Eff Value [STATE Int]
lookupEnv x [] = pure Wrong
lookupEnv x ((y, v) :: env) = if x == y then pure v else lookupEnv x env

tick : Eff () [STATE Int]
tick = update (+1)

-- Idris doesn't have both >> and *> like Haskell, but only *>
add : Value -> Value -> Eff Value [STATE Int]
add (Num n) (Num m) = tick *> pure (Num (n + m))
add _ _ = pure Wrong

apply : Value -> Value -> Eff Value [STATE Int]
apply (Fun f) x = tick *> f x
apply _ _ = pure Wrong

-- While programming with effects we can also uso do notation. When this is too
-- much boilerplate, we can also use the ! notation, like
--
-- pure $ Num !get
-- 
-- This is a syntactic sugar that means something like
--
-- do
-- n <- get
-- pure $ Num n
interp : Term -> Env -> Eff Value [STATE Int]
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
interp Count _ = pure $ Num !get

Show Term where
    show (Var x) = x
    show (Const n) = show n
    show (Add t1 t2) = show t1 ++ " + (" ++ show t2 ++ ")"
    show (Lam x t) = "λ" ++ x ++ "." ++ show t
    show (App t1 t2) = "(" ++ show t1 ++ ")" ++ show t2
    show Count = "Count"

Show Value where
    show Wrong = "<wrong>"
    show (Num n) = show n
    show (Fun f) = "<function>"

-- We can run our effectful computations in various ways. One of them is
-- the function runPure, which runs the computation in a pure context
-- (so this is viable for handling STATE Int, but wouldn't be for STDIO).
-- runPure would automatically initialize the initial state to 0, but
-- we can also do that by manually giving it the default argument env
-- in curly braces.
test : Term -> String
test t = show $ runPure (interp t []) {env = [0]}

term0 : Term
term0 = App (Lam "x" (Add (Var "x") (Var "x")))
            (Add (Const 10) (Const 11))

count_term0 : Term
count_term0 = Add Count (Add Count Count)

count_term1 : Term
count_term1 = Add (Add Count Count) Count

testTerms : List Term
testTerms = [term0, count_term0, count_term1]

-- Haskell's forM_ is called for_ in Idris and its class constraint is
-- the more general Applicative instead of Monad.
main : IO ()
main = do
    for_ testTerms $ \t => do
        putStrLn $ "Interpreting " ++ show t
        putStrLn $ test t