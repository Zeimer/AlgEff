import Control.Monad.Writer

type Name = String

-- We extend the syntax with Out, which should mean writing to the screen.
data Term = Var Name
          | Const Int
          | Add Term Term
          | Lam Name Term
          | App Term Term
          | Out Term

-- We use the Writer [Value] monad to represent the result of our functions.
-- This means every value carries with itself a log of all the values that
-- were printed so far.
data Value = Wrong
           | Num Int
           | Fun (Value -> Writer [Value] Value)

type Env = [(Name, Value)]

lookupEnv :: Name -> Env -> Writer [Value] Value
lookupEnv x [] = pure Wrong
lookupEnv x ((y, v) : env) = if x == y then pure v else lookupEnv x env

add :: Value -> Value -> Writer [Value] Value
add (Num n) (Num m) = pure $ Num (n + m)
add _ _ = pure Wrong

apply :: Value -> Value -> Writer [Value] Value
apply (Fun f) x = f x
apply _ _ = pure Wrong

-- We interpret Out t by appending t to the log.
interp :: Term -> Env -> Writer [Value] Value
interp (Var x) env = lookupEnv x env
interp (Const n) _ = pure $ Num n
interp (Add t1 t2) env = do
    n1 <- interp t1 env
    n2 <- interp t2 env
    add n1 n2
interp (Lam x t) env = pure $ Fun (\a -> interp t ((x, a) : env))
interp (App t1 t2) env = do
    f <- interp t1 env
    x <- interp t2 env
    apply f x
interp (Out t) env = do
    v <- interp t env
    writer ((), [v])
    pure v

-- We diplay Out verbatim.
instance Show Term where
    show (Var x) = x
    show (Const n) = show n
    show (Add t1 t2) = show t1 ++ " + (" ++ show t2 ++ ")"
    show (Lam x t) = "λ" ++ x ++ "." ++ show t
    show (App t1 t2) = "(" ++ show t1 ++ ")" ++ show t2
    show (Out t) = "Out (" ++ show t ++ ")"
    
instance Show Value where
    show Wrong = "<wrong>"
    show (Num n) = show n
    show (Fun _) = "<function>"

-- We run our interpreter using runWriter. We don't need to give any initial
-- value because the neutral element of the [Value] monoid (which is []) is
-- used. We then append the log and the result.
test :: Term -> String
test t =
    case runWriter (interp t []) of
        (v, w) -> "log: " ++ show w ++ "\nresult: " ++ show v

term0 :: Term
term0 = App (Lam "x" (Add (Var "x") (Var "x")))
            (Add (Const 10) (Const 11))

-- More test terms.
out_term0 :: Term
out_term0 = Out (Add (Out (Const 42)) (Out (Const 23456789)))

testTerms :: [Term]
testTerms = [term0, out_term0]

main :: IO ()
main = do
    forM_ testTerms $ \t -> do
        putStrLn $ "Interpreting " ++ show t
        putStrLn $ test t