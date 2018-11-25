type Name = String

data Term = Var Name
          | Const Int
          | Add Term Term
          | Lam Name Term
          | App Term Term
          | Count
          | Fail
          | Amb Term Term
          | Out Term

data Value = Wrong
           | Num Int
           | Fun (Value -> TODO Value)

type Env = [(Name, Value)]

instance Show Value where
    show Wrong = "<wrong>"
    show (Num n) = show n
    show (Fun f) = "<function>"

lookupEnv :: Name -> Env -> Either String Value
lookupEnv x [] = Left $ "Variable " ++ x ++ " not bound!"
lookupEnv x ((y, v) : env) = if x == y then pure v else lookupEnv x env

interp :: Term -> Env -> TODO Value
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
interp Fail _ = []
interp (Amb t1 t2) env = interp t1 env ++ interp t2 env
interp (Out t) env = do
    v <- interp t env
    tell $ show v ++ "; "
    pure v

tick :: State Int ()
tick = modify (+1)
    
add :: Value -> Value -> TODO Value
add (Num n) (Num m) = tick >> (pure $ Num (n + m))
add _ _ = Left $ "Can't add!"

apply :: Value -> Value -> TODO Value
apply (Fun f) x = tick >> f x
apply f _ = Left $ show f ++ " should be a function!"

test :: TODO
test = undefined

term0 :: Term
term0 = App (Lam "x" (Add (Var "x") (Var "x")))
            (Add (Const 10) (Const 11))

error_term0 :: Term
error_term0 = Var "OBOŻETOJESTZBOŻE"

count_term0 :: Term
count_term0 = Add Count (Add Count Count)

count_term1 :: Term
count_term1 = Add (Add Count Count) Count

failamb_term0 :: Term
failamb_term0 = Add (Const 42) Fail

failamb_term1 :: Term
failamb_term1 = Amb (Const 100) (Const 1234567890)

out_term0 :: Term
out_term0 = Out (Add (Out (Const 42)) (Out (Const 23456789)))

main :: IO ()
main = do
    forM_ [term0, count_term0, count_term1, failamb_term0, failamb_term1, out_term0]
          (putStrLn . test)