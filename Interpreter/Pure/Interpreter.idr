-- In Idris, types are first class values so that type aliases are defined like
-- ordinary values and functions.
Name : Type
Name = String

-- Terms and values are represented as in Haskell.
data Term = Var Name
          | Const Int
          | Add Term Term
          | Lam Name Term
          | App Term Term

data Value = Wrong
           | Num Int
           | Fun (Value -> Value)

-- Idris has List a instead of Haskell's [a].
Env : Type
Env = List (Name, Value)

-- You have probably noticed, but Idris has : for typing instead of Haskell's ::
lookupEnv : Name -> Env -> Value
lookupEnv x [] = Wrong
lookupEnv x ((y, v) :: env) = if x == y then v else lookupEnv x env

-- In Idris, the order of definitions matters, so we have to place helper
-- functions before functions which use them (we did the same in Haskell,
-- but we didn't need to).
add : Value -> Value -> Value
add (Num n) (Num m) = Num (n + m)
add _ _ = Wrong

apply : Value -> Value -> Value
apply (Fun f) x = f x
apply _ _ = Wrong

-- The interpreter is as in Haskell, but we use :: for Cons instead of :
-- and => instead of -> in anonymous functions.
interp : Term -> Env -> Value
interp (Var x) env = lookupEnv x env
interp (Const n) _ = Num n
interp (Add t1 t2) env = add (interp t1 env) (interp t2 env)
interp (Lam x t) env = Fun (\a => interp t ((x, a) :: env))
interp (App t1 t2) env = apply (interp t1 env) (interp t2 env)

-- Typeclass instances don't need the "instance" keyword.
-- So much typing saved!
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

test : Term -> String
test t = show $ interp t []

term0 : Term
term0 = App (Lam "x" (Add (Var "x") (Var "x")))
            (Add (Const 10) (Const 11))

-- An important thing to notice is that in Idris' REPL IO actions like main are
-- not run, but printed. To run main in the REPL, we have to write :exec main
main : IO ()
main = do
    putStrLn $ "Interpreting " ++ show term0
    putStrLn $ test term0