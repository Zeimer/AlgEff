type Name = String

data Term = Var Name
          | Const Int
          | Add Term Term
          | Lam Name Term
          | App Term Term
          | Fail
          | Amb Term Term

data Value = Wrong
           | Num Int
           | Fun (Value -> [Value])

type Env = [(Name, Value)]

instance Show Value where
    show Wrong = "<wrong>"
    show (Num n) = show n
    show (Fun f) = "<function>"

lookupEnv :: Name -> Env -> [Value]
lookupEnv x [] = []
lookupEnv x ((y, v) : env) = if x == y then pure v else lookupEnv x env

interp :: Term -> Env -> [Value]
interp (Var x) env = lookupEnv x env
interp (Const n) _ = pure (Num n)
interp (Add t1 t2) env = do
    n1 <- interp t1 env
    n2 <- interp t2 env
    add n1 n2
interp (Lam x t) env = pure $ Fun (\a -> interp t ((x, a) : env))
interp (App t1 t2) env = do
    f <- interp t1 env
    x <- interp t2 env
    apply f x
interp Fail _ = []
interp (Amb t1 t2) env = interp t1 env ++ interp t2 env

add :: Value -> Value -> [Value]
add (Num n) (Num m) = pure $ Num (n + m)
add _ _ = []

apply :: Value -> Value -> [Value]
apply (Fun f) x = f x
apply _ _ = []

term0 :: Term
term0 = App (Lam "x" (Add (Var "x") (Var "x")))
            (Add (Const 10) (Const 11))

failamb_term0 :: Term
failamb_term0 = Add (Const 42) Fail

failamb_term1 :: Term
failamb_term1 = Amb (Const 100) (Const 1234567890)