import Effects
import Effect.Exception

Name : Type
Name = String

data Term = Var Name
          | Const Int
          | Add Term Term
          | Lam Name Term
          | App Term Term

data Value = Wrong
           | Num Int
           | Fun (Value -> Eff Value [EXCEPTION String])

Env : Type
Env = List (Name, Value)

Show Value where
    show Wrong = "<wrong>"
    show (Num n) = show n
    show (Fun f) = "<function>"

lookupEnv : Name -> Env -> Eff Value [EXCEPTION String]
lookupEnv x [] = raise $ "Variable " ++ x ++ " not bound!"
lookupEnv x ((y, v) :: env) = if x == y then pure v else lookupEnv x env

add : Value -> Value -> Eff Value [EXCEPTION String]
add (Num n) (Num m) = pure $ Num (n + m)
add _ _ = raise $ "Can't add!"

apply : Value -> Value -> Eff Value [EXCEPTION String]
apply (Fun f) x = f x
apply _ _ = raise $ "Can't apply!"

interp : Term -> Env -> Eff Value [EXCEPTION String]
interp (Var x) env = lookupEnv x env
interp (Const n) _ = pure $ Num n
interp (Add t1 t2) env = do
    n <- interp t1 env
    m <- interp t2 env
    add n m

interp (Lam x t) env = pure $ Fun (\a => interp t ((x, a) :: env))
interp (App t1 t2) env = do
    f <- interp t1 env
    x <- interp t2 env
    apply f x

test : Term -> String
test t =
    case run (interp t []) of
        Left msg => msg
        Right v => show v

term0 : Term
term0 = App (Lam "x" (Add (Var "x") (Var "x")))
            (Add (Const 10) (Const 11))
            
term1 : Term
term1 = Var "OBOŻETOJESTZBOŻE"

main : IO ()
main = do
    putStrLn $ test term0
    putStrLn $ test term1