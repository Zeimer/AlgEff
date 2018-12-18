-- Variable names are strings.
type Name = String

-- Terms are: variables, integer constants, addition of two terms,
-- lambda abstractions and applications.
data Term = Var Name
          | Const Int
          | Add Term Term
          | Lam Name Term
          | App Term Term

-- Values are: an error (like when trying to apply a constant to something), an integer, a function.
data Value = Wrong
           | Num Int
           | Fun (Value -> Value)

-- The environment binds names to values.
type Env = [(Name, Value)]

-- Look up a a name in the environment.
lookupEnv :: Name -> Env -> Value
lookupEnv x [] = Wrong
lookupEnv x ((y, v) : env) = if x == y then v else lookupEnv x env

-- Helper function for addition. We can only add numbers, anything else is Wrong.
add :: Value -> Value -> Value
add (Num n) (Num m) = Num (n + m)
add _ _ = Wrong

-- Helper function for application. We can perform it only when the first argument is a function.
apply :: Value -> Value -> Value
apply (Fun f) x = f x
apply _ _ = Wrong

-- An interpreter. We look variables up in the environment.
-- For lambdas, we create a new function with an extended environment.
-- The rest is straightforward.
interp :: Term -> Env -> Value
interp (Var x) env = lookupEnv x env
interp (Const n) _ = Num n
interp (Add t1 t2) env = add (interp t1 env) (interp t2 env)
interp (Lam x t) env = Fun (\a -> interp t ((x, a) : env))
interp (App t1 t2) env = apply (interp t1 env) (interp t2 env)

-- A somewhat retarded printing for terms. The parentheses in the Add case are
-- there so that we can tell whether it's Add x (Add y z) or Add (Add x y) z by
-- looking at the printed term.
instance Show Term where
    show (Var x) = x
    show (Const n) = show n
    show (Add t1 t2) = show t1 ++ " + (" ++ show t2 ++ ")"
    show (Lam x t) = "λ" ++ x ++ "." ++ show t
    show (App t1 t2) = "(" ++ show t1 ++ ")(" ++ show t2 ++ ")"

-- Functions are displayed using a placeholder.
instance Show Value where
    show Wrong = "<wrong>"
    show (Num n) = show n
    show (Fun f) = "<function>"

-- A test function that runs the interpreter and tranasforms the result into
-- a string.
test :: Term -> String
test t = show $ interp t []

-- A test term. Its interpretation should be 42.
term0 :: Term
term0 = App (Lam "x" (Add (Var "x") (Var "x")))
            (Add (Const 10) (Const 11))

-- Print the test term and run the test.
main :: IO ()
main = do
    putStrLn $ "Interpreting " ++ show term0
    putStrLn $ test term0