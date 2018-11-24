type Name = String

data Term = Var Name
          | Const Int
          | Add Term Term
          | Lam Name Term
          | App Term Term

data Value = Wrong
           | Num Int
           | Fun (Value -> Either String Value)

type Env = [(Name, Value)]

instance Show Value where
    show Wrong = "<wrong>"
    show (Num n) = show n
    show (Fun f) = "<function>"

lookupEnv :: Name -> Env -> Either String Value
lookupEnv x [] = Left $ "Variable " ++ x ++ " not bound!"
lookupEnv x ((y, v) : env) = if x == y then pure v else lookupEnv x env

interp :: Term -> Env -> Either String Value
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

add :: Value -> Value -> Either String Value
add (Num n) (Num m) = pure $ Num (n + m)
add _ _ = Left $ "Can't add!"

apply :: Value -> Value -> Either String Value
apply (Fun f) x = f x
apply f _ = Left $ show f ++ " should be a function!"

term0 :: Term
term0 = App (Lam "x" (Add (Var "x") (Var "x")))
            (Add (Const 10) (Const 11))

test :: Term -> String
test t = show $ interp t []