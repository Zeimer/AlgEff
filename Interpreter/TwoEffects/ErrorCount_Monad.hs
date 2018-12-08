import Control.Monad.State

type Name = String

data Term = Var Name
          | Const Int
          | Add Term Term
          | Lam Name Term
          | App Term Term
          | Count

-- WARNING: this file is utterly broken. If you think that monads are all you
-- need, try to fix it.

data Value = Wrong
           | Num Int
           | Fun (Value -> Either String (State Int Value))

type Env = [(Name, Value)]

lookupEnv :: Name -> Env -> Either String (State Int Value)
lookupEnv x [] = Left $ "Variable " ++ x ++ " not bound!"
lookupEnv x ((y, v) : env) = if x == y then pure (pure v) else lookupEnv x env

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

interp :: Term -> Env -> Either String (State Int Value)
interp (Var x) env = lookupEnv x env
interp (Const n) _ = pure $ pure (Num n)
interp (Add t1 t2) env = do
    s <- pure get
    s1 <- interp t1 env
    s2 <- interp t2 env
    Left "" {-
    s <- do
        s1' <- s1
        s2' <- s2
        add s1 s2
    pure s
    
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
-}

instance Show Term where
    show (Var x) = x
    show (Const n) = show n
    show (Add t1 t2) = show t1 ++ " + (" ++ show t2 ++ ")"
    show (Lam x t) = "λ" ++ x ++ "." ++ show t
    show (App t1 t2) = "(" ++ show t1 ++ ")" ++ show t2
    show (Count) = "Count"

instance Show Value where
    show Wrong = "<wrong>"
    show (Num n) = show n
    show (Fun f) = "<function>"

test :: Term -> String
test t =
    case interp t [] of
        Left msg -> msg
        Right t' -> case runState t' 0 of
            (v, s) -> show v ++ " (in " ++ show s ++ " steps)"

term0 :: Term
term0 = App (Lam "x" (Add (Var "x") (Var "x")))
            (Add (Const 10) (Const 11))

count_term0 :: Term
count_term0 = Add Count (Add Count Count)

count_term1 :: Term
count_term1 = Add (Add Count Count) Count
            
error_term0 :: Term
error_term0 = Var "O BOŻE TO JEST ZBOŻE"

testTerms :: [Term]
testTerms = [term0, count_term0, count_term1, error_term0]

main :: IO ()
main = do
    forM_ testTerms $ \t -> do
        putStrLn $ "Interpreting " ++ show t
        putStrLn $ test t