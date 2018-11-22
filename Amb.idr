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

data Value = Wrong
           | Num Int
           | Fun (Value -> Eff Value [SELECT])

Env : Type
Env = List (Name, Value)

Show Value where
    show Wrong = "<wrong>"
    show (Num n) = show n
    show (Fun f) = "<function>"

lookupEnv : Name -> Env -> Eff Value [SELECT]
lookupEnv x [] = select []
lookupEnv x ((y, v) :: env) = if x == y then pure v else lookupEnv x env

add : Value -> Value -> Eff Value [SELECT]
add (Num n) (Num m) = pure $ Num (n + m)
add _ _ = select []

apply : Value -> Value -> Eff Value [SELECT]
apply (Fun f) x = f x
apply _ _ = select []

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
    v1 <- interp t1 env
    v2 <- interp t2 env
    select [v1, v2]

term0 : Term
term0 = App (Lam "x" (Add (Var "x") (Var "x")))
            (Add (Const 10) (Const 11))

term1 : Term
term1 = Fail

term2 : Term
term2 = Amb (Const 100) (Const 1234567890)

test : Term -> String
test t = show $ the (Maybe _) $ run (interp t [])

test' : Term -> String
test' t = show $ the (List _) $ run (interp t [])

main : IO ()
main = do
    for [term0, term1, term2] $ \t => putStrLn $ test t
    for [term0, term1, term2] $ \t => putStrLn $ test' t
    pure ()