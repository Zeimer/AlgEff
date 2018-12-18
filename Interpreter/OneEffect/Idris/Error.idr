import Effects
import Effect.Exception

Name : Type
Name = String

data Term = Var Name
          | Const Int
          | Add Term Term
          | Lam Name Term
          | App Term Term

-- For errors we use the EXCEPTION String effect.
data Value = Wrong
           | Num Int
           | Fun (Value -> Eff Value [EXCEPTION String])

Env : Type
Env = List (Name, Value)

-- This effect allows us the use the function raise.
lookupEnv : Name -> Env -> Eff Value [EXCEPTION String]
lookupEnv x [] = raise $ "Variable " ++ x ++ " not bound!"
lookupEnv x ((y, v) :: env) = if x == y then pure v else lookupEnv x env

add : Value -> Value -> Eff Value [EXCEPTION String]
add (Num n) (Num m) = pure $ Num (n + m)
add _ _ = raise $ "Can't add!"

apply : Value -> Value -> Eff Value [EXCEPTION String]
apply (Fun f) x = f x
apply _ _ = raise $ "Can't apply!"

interp : Term -> Env -> Eff Value [EXCEPTION String]
interp (Var x) env = lookupEnv x env
interp (Const n) _ = pure $ Num n
interp (Add t1 t2) env = do
    n <- interp t1 env
    m <- interp t2 env
    add n m
interp (Lam x t) env = pure $ Fun (\a => interp t ((x, a) :: env))
interp (App t1 t2) env = do
    f <- interp t1 env
    x <- interp t2 env
    apply f x

Show Term where
    show (Var x) = x
    show (Const n) = show n
    show (Add t1 t2) = show t1 ++ " + (" ++ show t2 ++ ")"
    show (Lam x t) = "λ" ++ x ++ "." ++ show t
    show (App t1 t2) = "(" ++ show t1 ++ ")(" ++ show t2 ++ ")"

Show Value where
    show Wrong = "<wrong>"
    show (Num n) = show n
    show (Fun f) = "<function>"

-- We handle our effect by calling run. We don't need to do any initialization
-- like we had for the STATE effect. The context in which to run the computation
-- is inferred to be Either String String, because of the case expression.
test : Term -> String
test t =
    case run (interp t []) of
        Left msg => msg
        Right v => show v

term0 : Term
term0 = App (Lam "x" (Add (Var "x") (Var "x")))
            (Add (Const 10) (Const 11))

error_term0 : Term
error_term0 = Var "OBOŻETOJESTZBOŻE"

testTerms : List Term
testTerms = [term0, error_term0]

main : IO ()
main = do
    for_ testTerms $ \t => do
        putStrLn $ cast $ replicate 50 '-'
        putStrLn $ "Interpreting " ++ show t
        putStrLn $ test t
        putStrLn $ cast $ replicate 50 '-'