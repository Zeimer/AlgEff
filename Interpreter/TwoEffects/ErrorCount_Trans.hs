import Control.Monad.Trans.Except
import Control.Monad.State

type Name = String

data Term = Var Name
          | Const Int
          | Add Term Term
          | Lam Name Term
          | App Term Term
          | Count

-- To handle both errors and state, we use the ExceptT Stirng (State Int)
-- monad transformer stack. Order of transformers matters here. What we
-- get is in fact State Int (Either String Value).
data Value = Wrong
           | Num Int
           | Fun (Value -> ExceptT String (State Int) Value)

type Env = [(Name, Value)]

-- The type of our computations is somewhat verbose, but using the error
-- and state functionality is rather easy.
lookupEnv :: Name -> Env -> ExceptT String (State Int) Value
lookupEnv x [] = throwE $ "Variable " ++ x ++ " not bound!"
lookupEnv x ((y, v) : env) = if x == y then pure v else lookupEnv x env

tick :: ExceptT String (State Int) ()
tick = modify (+1)

add :: Value -> Value -> ExceptT String (State Int) Value
add (Num n) (Num m) = tick >> (pure $ Num (n + m))
add _ _ = throwE $ "Can't add!"

apply :: Value -> Value -> ExceptT String (State Int) Value
apply (Fun f) x = tick >> f x
apply _ _ = throwE $ "Can't apply!"

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

-- Running our interpreter is a bit difficult. First we have to call runExceptT
-- to get State Int (Either String Value). Then we have to call runState with
-- an initial state of 0 to get (Either String Value, Int).
test :: Term -> String
test t =
    case runState (runExceptT (interp t [])) 0 of
        (Left msg, _) -> msg
        (Right v, s) -> show v ++ " (in " ++ show s ++ " steps)"

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