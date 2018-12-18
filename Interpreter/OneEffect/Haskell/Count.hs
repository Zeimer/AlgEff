import Control.Monad.State

type Name = String

-- We extend the syntax with Count, which should mean the number of times
-- addition or application was performed during reduction.
data Term = Var Name
          | Const Int
          | Add Term Term
          | Lam Name Term
          | App Term Term
          | Count

-- Because of this our representation of functions also changes.
data Value = Wrong
           | Num Int
           | Fun (Value -> State Int Value)

type Env = [(Name, Value)]

lookupEnv :: Name -> Env -> Value
lookupEnv x [] = Wrong
lookupEnv x ((y, v) : env) = if x == y then v else lookupEnv x env

-- Increase the count by one.
tick :: State Int ()
tick = state $ \s -> ((), s + 1)

-- The types of add and apply change because we need to maintain the counter.
add :: Value -> Value -> State Int Value
add (Num n) (Num m) = tick >> (pure $ Num (n + m))
add _ _ = pure Wrong

apply :: Value -> Value -> State Int Value
apply (Fun f) x = tick >> f x
apply _ _ = pure Wrong

-- Because the return type is now State Int Value, the body of interp has to
-- be written in monadic style.
-- We interpret Count by just returning the counter we hold in our state.
interp :: Term -> Env -> State Int Value
interp (Var x) env = pure $ lookupEnv x env
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

-- Count is shown simply as "Count"
instance Show Term where
    show (Var x) = x
    show (Const n) = show n
    show (Add t1 t2) = show t1 ++ " + (" ++ show t2 ++ ")"
    show (Lam x t) = "λ" ++ x ++ "." ++ show t
    show (App t1 t2) = "(" ++ show t1 ++ ")(" ++ show t2 ++ ")"
    show Count = "Count"

instance Show Value where
    show Wrong = "<wrong>"
    show (Num n) = show n
    show (Fun f) = "<function>"

-- To run our interpreter, we have to call runState with the second argument
-- being the initial state. When printing the value, we also show the operation
-- count.
test :: Term -> String
test t =
    case runState (interp t []) 0 of
        (v, s) -> show v ++ " (in " ++ show s ++ " steps)"

term0 :: Term
term0 = App (Lam "x" (Add (Var "x") (Var "x")))
            (Add (Const 10) (Const 11))

-- More tests terms.
count_term0 :: Term
count_term0 = Add Count (Add Count Count)

count_term1 :: Term
count_term1 = Add (Add Count Count) Count

testTerms :: [Term]
testTerms = [term0, count_term0, count_term1]

-- We iterate over the test terms and display each one and the result of
-- evaluating it.
main :: IO ()
main = do
    forM_ testTerms $ \t -> do
        putStrLn $ replicate 50 '-'
        putStrLn $ "Interpreting " ++ show t
        putStrLn $ test t
        putStrLn $ replicate 50 '-'