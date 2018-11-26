import Control.Monad.Trans.Except
import Control.Monad.State

type Name = String

data Term = Var Name
          | Const Int
          | Add Term Term
          | Lam Name Term
          | App Term Term
          | Count

data Value = Wrong
           | Num Int
           | Fun (Value -> ExceptT String (State Int) Value)

type Env = [(Name, Value)]

instance Show Value where
    show Wrong = "<wrong>"
    show (Num n) = show n
    show (Fun f) = "<function>"

lookupEnv :: Name -> Env -> ExceptT String (State Int) Value
lookupEnv x [] = --except $ pure "Variable " ++ x ++ " not bound!"
lookupEnv x ((y, v) : env) = if x == y then pure v else lookupEnv x env

interp :: Term -> Env -> ExceptT String (State Int) Value
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
interp Count _ = do
    n <- get
    pure $ Num n

tick :: ExceptT String (State Int) ()
tick = modify (+1)

add :: Value -> Value -> ExceptT String (State Int) Value
add (Num n) (Num m) = tick >> (pure $ Num (n + m))
add _ _ = pure Wrong

apply :: Value -> Value -> ExceptT String (State Int) Value
apply (Fun f) x = tick >> f x
apply _ _ = pure Wrong

term0 :: Term
term0 = App (Lam "x" (Add (Var "x") (Var "x")))
            (Add (Const 10) (Const 11))

term1 :: Term
term1 = Add Count (Add Count Count)

term2 :: Term
term2 = Add (Add Count Count) Count