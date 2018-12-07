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

-- The type of our computations is no less verbose than in Haskell, but at least
-- the order of effects doesn't matter that much.
data Value = Wrong
           | Num Int
           | Fun (Value -> Eff Value [STATE Int, EXCEPTION String])

Env : Type
Env = List (Name, Value)

lookupEnv : Name -> Env -> Eff Value [STATE Int, EXCEPTION String]
lookupEnv x [] = raise $ "Variable " ++ x ++ " not bound!"
lookupEnv x ((y, v) :: env) = if x == y then pure v else lookupEnv x env

tick : Eff () [STATE Int]
tick = update (+1)

add : Value -> Value -> Eff Value [STATE Int, EXCEPTION String]
add (Num n) (Num m) = tick *> pure (Num (n + m))
add _ _ = raise "Can't add!"

apply : Value -> Value -> Eff Value [STATE Int, EXCEPTION String]
apply (Fun f) x = tick *> f x
apply _ _ = raise "Can't apply!"

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

Show Term where
    show (Var x) = x
    show (Const n) = show n
    show (Add t1 t2) = show t1 ++ " + (" ++ show t2 ++ ")"
    show (Lam x t) = "λ" ++ x ++ "." ++ show t
    show (App t1 t2) = "(" ++ show t1 ++ ")" ++ show t2
    show (Count) = "Count"

Show Value where
    show Wrong = "<wrong>"
    show (Num n) = show n
    show (Fun f) = "<function>"

test : Term -> String
test t =
    case the (Either String _) $ run (interp t []) of
        Left msg => msg
        Right v => show v

term0 : Term
term0 = App (Lam "x" (Add (Var "x") (Var "x")))
            (Add (Const 10) (Const 11))

count_term0 : Term
count_term0 = Add Count (Add Count Count)

count_term1 : Term
count_term1 = Add (Add Count Count) Count
            
error_term0 : Term
error_term0 = Var "O BOŻE TO JEST ZBOŻE"

testTerms : List Term
testTerms = [term0, count_term0, count_term1, error_term0]

main : IO ()
main = do
    for_ testTerms $ \t => do
        putStrLn $ "Interpreting " ++ show t
        putStrLn $ test t