import Effects
import Effect.Exception
import Effect.State

Name : Type
Name = String

data Term = Var Name
          | Const Int
          | Add Term Term
          | Lam Name Term
          | App Term Term
          | Count

data Value = Wrong
           | Num Int
           | Fun (Value -> Eff Value [STATE Int, EXCEPTION String])

Env : Type
Env = List (Name, Value)

Show Value where
    show Wrong = "<wrong>"
    show (Num n) = show n
    show (Fun f) = "<function>"

lookupEnv : Name -> Env -> Eff Value [STATE Int, EXCEPTION String]
lookupEnv x [] = raise $ "Variable " ++ x ++ " not bound!"
lookupEnv x ((y, v) :: env) = if x == y then pure v else lookupEnv x env

tick : Eff () [STATE Int]
tick = do
    n <- get
    put $ n + 1
    
add : Value -> Value -> Eff Value [STATE Int, EXCEPTION String]
add (Num n) (Num m) = do
    tick
    pure $ Num (n + m)
add _ _ = raise "Can't add!"

apply : Value -> Value -> Eff Value [STATE Int, EXCEPTION String]
apply (Fun f) x = do
    tick
    f x
apply _ _ = raise "Can't apply"

interp : Term -> Env -> Eff Value [STATE Int, EXCEPTION String]
interp (Var x) env = lookupEnv x env
interp (Const n) _ = pure (Num n)
interp (Add t1 t2) env = do
    n1 <- interp t1 env
    n2 <- interp t2 env
    add n1 n2
interp (Lam x t) env = pure $ Fun (\a => interp t ((x, a) :: env))
interp (App t1 t2) env = do
    f <- interp t1 env
    x <- interp t2 env
    apply f x
interp Count _ = do
    n <- get
    pure $ Num n

term0 : Term
term0 = App (Lam "x" (Add (Var "x") (Var "x")))
            (Add (Const 10) (Const 11))

term1 : Term
term1 = Add Count (Add Count Count)

term2 : Term
term2 = Add (Add Count Count) Count
            
term3 : Term
term3 = Var "O BOŻE TO JEST ZBOŻE"

test : Term -> String
test t =
    case the (Either String _) $ run (interp t []) of
        Left msg => msg
        Right v => show v

main : IO ()
main = do
    putStrLn $ "term0: " ++ test term0
    putStrLn $ "term1: " ++ test term1
    putStrLn $ "term2: " ++ test term2
    putStrLn $ "term3: " ++ test term3