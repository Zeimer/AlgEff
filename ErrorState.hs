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
           | Fun (Value -> Either String (State Int Value))

type Env = [(Name, Value)]

instance Show Value where
    show Wrong = "<wrong>"
    show (Num n) = show n
    show (Fun f) = "<function>"

lookupEnv :: Name -> Env -> Either String (State Int Value)
lookupEnv x [] = Left $ "Variable " ++ x ++ " not bound!"
lookupEnv x ((y, v) : env) = if x == y then pure (pure v) else lookupEnv x env

interp :: Term -> Env -> Either String (State Int Value)
interp (Var x) env = lookupEnv x env
interp (Const n) _ = pure $ pure (Num n)
interp (Add t1 t2) env = do
    s1 <- interp t1 env
    s2 <- interp t2 env
    add <$> s1 <*> s2
interp (Lam x t) env = pure $ pure $ Fun (\a -> interp t ((x, a) : env))
interp (App t1 t2) env = do
    f <- interp t1 env
    x <- interp t2 env
    do
        f' <- f
        x' <- x
        apply f x
interp Count _ = do
    n <- get
    pure $ pure $ Num n

tick :: Either String (State Int ())
tick = Right $ modify (+1)

add :: Value -> Value -> Either String (State Int Value)
add (Num n) (Num m) = do
    pure tick
    pure $ pure $ Num (n + m)
add _ _ = pure $ pure Wrong

apply :: Value -> Value -> Either String (State Int Value)
apply (Fun f) x = do
    pure tick
    f x
apply _ _ = pure $ pure Wrong

term0 :: Term
term0 = App (Lam "x" (Add (Var "x") (Var "x")))
            (Add (Const 10) (Const 11))

term1 :: Term
term1 = Add Count (Add Count Count)

term2 :: Term
term2 = Add (Add Count Count) Count

{-
test :: Term -> String
test t =
    case runState (interp t []) 0 of
        Left msg -> msg
        Right (Wrong, s) -> "<wrong> (in " ++ show s ++ " steps)"
        Right (Num n, s) -> show n ++ " (in " ++ show s ++ " steps)"
        Right (Fun f, s) -> "<function> (in " ++ show s ++ " steps)"
-}