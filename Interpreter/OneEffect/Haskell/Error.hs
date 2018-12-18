import Control.Monad

type Name = String

-- This time the syntax doesn't change.
data Term = Var Name
          | Const Int
          | Add Term Term
          | Lam Name Term
          | App Term Term

-- But the values do, since we want to do proper error handling instead of
-- just using Wrong as the result.
data Value = Wrong
           | Num Int
           | Fun (Value -> Either String Value)

type Env = [(Name, Value)]

-- We "raise" an error using left when a variable is not in the environment.
lookupEnv :: Name -> Env -> Either String Value
lookupEnv x [] = Left $ "Variable " ++ x ++ " not bound!"
lookupEnv x ((y, v) : env) = if x == y then pure v else lookupEnv x env

-- Similarly add and apply raise an error when given wrong arguments.
add :: Value -> Value -> Either String Value
add (Num n) (Num m) = pure $ Num (n + m)
add _ _ = Left $ "Can't add!"

apply :: Value -> Value -> Either String Value
apply (Fun f) x = f x
apply f _ = Left $ show f ++ " should be a function!"

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

instance Show Term where
    show (Var x) = x
    show (Const n) = show n
    show (Add t1 t2) = show t1 ++ " + (" ++ show t2 ++ ")"
    show (Lam x t) = "λ" ++ x ++ "." ++ show t
    show (App t1 t2) = "(" ++ show t1 ++ ")(" ++ show t2 ++ ")"

instance Show Value where
    show Wrong = "<wrong>"
    show (Num n) = show n
    show (Fun f) = "<function>"

-- To get a string representation of the result, we run our interpreter and
-- see whether it produces an error or a value.
test :: Term -> String
test t =
    case interp t [] of
        Left msg -> msg
        Right v -> show v

term0 :: Term
term0 = App (Lam "x" (Add (Var "x") (Var "x")))
            (Add (Const 10) (Const 11))

-- More test terms.
error_term0 :: Term
error_term0 = Var "OBOŻETOJESTZBOŻE"

testTerms :: [Term]
testTerms = [term0, error_term0]

main :: IO ()
main = do
    forM_ testTerms $ \t -> do
        putStrLn $ replicate 50 '-'
        putStrLn $ "Interpreting " ++ show t
        putStrLn $ test t
        putStrLn $ replicate 50 '-'