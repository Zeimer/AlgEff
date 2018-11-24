import Control.Monad.Writer

type Name = String

data Term = Var Name
          | Const Int
          | Add Term Term
          | Lam Name Term
          | App Term Term
          | Out Term

data Value = Wrong
           | Num Int
           | Fun (Value -> Writer String Value)

instance Show Value where
    show Wrong = "<wrong>"
    show (Num n) = show n
    show (Fun _) = "<function>"

type Env = [(Name, Value)]

lookupEnv :: Name -> Env -> Writer String Value
lookupEnv x [] = pure Wrong
lookupEnv x ((y, v) : env) = if x == y then pure v else lookupEnv x env

interp :: Term -> Env -> Writer String Value
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
    tell $ show v ++ "; "
    pure v

add :: Value -> Value -> Writer String Value
add (Num n) (Num m) = pure $ Num (n + m)
add _ _ = pure Wrong

apply :: Value -> Value -> Writer String Value
apply (Fun f) x = f x
apply _ _ = pure Wrong

showwriter :: Writer String Value -> String
showwriter w =
    case runWriter w of
        (v, w) -> "log: " ++ w ++ "\nresult: " ++ show v

test :: Term -> String
test t = showwriter $ interp t []

term0 :: Term
term0 = App (Lam "x" (Add (Var "x") (Var "x")))
            (Add (Const 10) (Const 11))

out_term0 :: Term
out_term0 = Out (Add (Out (Const 42)) (Out (Const 23456789)))

main :: IO ()
main = do
    forM_ [term0, out_term0] (putStrLn . test)