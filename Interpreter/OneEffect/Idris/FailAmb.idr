import Effects
import Effect.Select

Name : Type
Name = String

data Term = Var Name
          | Const Int
          | Add Term Term
          | Lam Name Term
          | App Term Term
          | Fail
          | Amb Term Term

-- For nondeterminism we use the SELECT effect. It will allow us to
-- nondeterministically select a value from a list.
data Value = Wrong
           | Num Int
           | Fun (Value -> Eff Value [SELECT])

Env : Type
Env = List (Name, Value)

-- Failure in lookup, add and apply is represented by selecting from the empty
-- list.
lookupEnv : Name -> Env -> Eff Value [SELECT]
lookupEnv x [] = select []
lookupEnv x ((y, v) :: env) = if x == y then pure v else lookupEnv x env

add : Value -> Value -> Eff Value [SELECT]
add (Num n) (Num m) = pure $ Num (n + m)
add _ _ = select []

apply : Value -> Value -> Eff Value [SELECT]
apply (Fun f) x = f x
apply _ _ = select []

-- In the Amb case we can't just concatenate like we did in Haskell, because
-- our SELECT effect is more abstract than Haskell's list monad. Instead we
-- select either True or False and then continue intepreting the appropriate
-- branch.
interp : Term -> Env -> Eff Value [SELECT]
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
interp Fail _ = select []
interp (Amb t1 t2) env = do
    b <- select [True, False]
    if b then interp t1 env else interp t2 env

Show Term where
    show (Var x) = x
    show (Const n) = show n
    show (Add t1 t2) = show t1 ++ " + (" ++ show t2 ++ ")"
    show (Lam x t) = "λ" ++ x ++ "." ++ show t
    show (App t1 t2) = "(" ++ show t1 ++ ")(" ++ show t2 ++ ")"
    show Fail = "Fail"
    show (Amb t1 t2) = "Amb (" ++ show t1 ++ ") (" ++ show t2 ++ ")"

Show Value where
    show Wrong = "<wrong>"
    show (Num n) = show n
    show (Fun f) = "<function>"

-- Because SELECT is more abstract than Haskell's list monad, we can handle it
-- in more ways. The function 'the' has type (a : Type) -> a -> a, so it is a
-- cleverly named identity function for disambiguating types. We use it so that
-- our computation is interpreted as Maybe (which means "take the first answer")
-- or as a list (which means "take all the answers").
test_Maybe : Term -> String
test_Maybe t = show $ the (Maybe _) $ run (interp t [])

test_List : Term -> String
test_List t = show $ the (List _) $ run (interp t [])

term0 : Term
term0 = App (Lam "x" (Add (Var "x") (Var "x")))
            (Add (Const 10) (Const 11))

failamb_term0 : Term
failamb_term0 = Add (Const 42) Fail

failamb_term1 : Term
failamb_term1 = Amb (Const 100) (Const 12345)

testTerms : List Term
testTerms = [term0, failamb_term0, failamb_term1]

main : IO ()
main = do
    putStrLn "Testing the Maybe handler"
    for_ testTerms $ \t => do
        putStrLn $ cast $ replicate 50 '-'
        putStrLn $ "Interpreting " ++ show t
        putStrLn $ test_Maybe t
        putStrLn $ cast $ replicate 50 '-'
    putStrLn "Testing the List handler"
    for_ testTerms $ \t => do
        putStrLn $ cast $ replicate 50 '-'
        putStrLn $ "Interpreting " ++ show t
        putStrLn $ test_List t
        putStrLn $ cast $ replicate 50 '-'