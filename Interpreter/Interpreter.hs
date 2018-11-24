type Name = String

data Term = Var Name
          | Const Int
          | Add Term Term
          | Lam Name Term
          | App Term Term

data Value m = Wrong
           | Num Int
           | Fun (Value m -> m (Value m))

type Env m = [(Name, Value m)]

instance Show (Value m) where
    show Wrong = "<wrong>"
    show (Num n) = show n
    show (Fun f) = "<function>"

lookupEnv :: (Monad m) => Name -> Env m -> m (Value m)
lookupEnv x [] = pure Wrong
lookupEnv x ((y, v) : env) = if x == y then pure v else lookupEnv x env

interp :: (Monad m) => Term -> Env m -> m (Value m)
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

add :: (Monad m) => Value m -> Value m -> m (Value m)
add (Num n) (Num m) = pure $ Num (n + m)
add _ _ = pure Wrong

apply :: (Monad m) => Value m -> Value m -> m (Value m)
apply (Fun f) x = f x
apply _ _ = pure Wrong

term0 :: Term
term0 = App (Lam "x" (Add (Var "x") (Var "x")))
            (Add (Const 10) (Const 11))

test :: (Monad m) => Term -> m String
test t = interp t [] >>= pure . show