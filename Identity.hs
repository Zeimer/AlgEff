type Name = String

data Term = Var Name
          | Const Int
          | Add Term Term
          | Lam Name Term
          | App Term Term

data Value = Wrong
           | Num Int
           | Fun (Value -> Value)

type Env = [(Name, Value)]

instance Show Value where
    show Wrong = "<wrong>"
    show (Num n) = show n
    show (Fun f) = "<function>"

lookupEnv :: Name -> Env -> Value
lookupEnv x [] = Wrong
lookupEnv x ((y, v) : env) = if x == y then v else lookupEnv x env

interp :: Term -> Env -> Value
interp (Var x) env = lookupEnv x env
interp (Const n) _ = Num n
interp (Add t1 t2) env = add (interp t1 env) (interp t2 env)
interp (Lam x t) env = Fun (\a -> interp t ((x, a) : env))
interp (App t1 t2) env = apply (interp t1 env) (interp t2 env)

add :: Value -> Value -> Value
add (Num n) (Num m) = Num (n + m)
add _ _ = Wrong

apply :: Value -> Value -> Value
apply (Fun f) x = f x
apply _ _ = Wrong

term0 :: Term
term0 = App (Lam "x" (Add (Var "x") (Var "x")))
            (Add (Const 10) (Const 11))

test :: Term -> String
test t = show $ interp t []