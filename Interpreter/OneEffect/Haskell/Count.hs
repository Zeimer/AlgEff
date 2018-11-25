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
           | Fun (Value -> State Int Value)

type Env = [(Name, Value)]

lookupEnv :: Name -> Env -> State Int Value
lookupEnv x [] = pure Wrong
lookupEnv x ((y, v) : env) = if x == y then pure v else lookupEnv x env

tick :: State Int ()
tick = modify (+1)

add :: Value -> Value -> State Int Value
add (Num n) (Num m) = tick >> (pure $ Num (n + m))
add _ _ = pure Wrong

apply :: Value -> Value -> State Int Value
apply (Fun f) x = tick >> f x
apply _ _ = pure Wrong

interp :: Term -> Env -> State Int Value
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

instance Show Term where
    show (Var x) = x
    show (Const n) = show n
    show (Add t1 t2) = show t1 ++ " + (" ++ show t2 ++ ")"
    show (Lam x t) = "λ" ++ x ++ "." ++ show t
    show (App t1 t2) = "(" ++ show t1 ++ ")" ++ show t2
    show Count = "Count"

instance Show Value where
    show Wrong = "<wrong>"
    show (Num n) = show n
    show (Fun f) = "<function>"

test :: Term -> String
test t =
    case runState (interp t []) 0 of
        (v, s) -> show v ++ " (in " ++ show s ++ " steps)"

term0 :: Term
term0 = App (Lam "x" (Add (Var "x") (Var "x")))
            (Add (Const 10) (Const 11))

count_term0 :: Term
count_term0 = Add Count (Add Count Count)

count_term1 :: Term
count_term1 = Add (Add Count Count) Count

testTerms :: [Term]
testTerms = [term0, count_term0, count_term1]

main :: IO ()
main = do
    forM_ testTerms $ \t -> do
        putStrLn $ "Interpreting " ++ show t
        putStrLn $ test t