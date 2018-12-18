import Control.Monad.State
import Control.Monad.Writer

-- EXERCISE: this file is utterly broken. If you think that monads are all you
-- need, try fixing it. I assure you that you will fail miserably.

type Name = String

-- This time we want to have all the effects at once.
data Term = Var Name
          | Const Int
          | Add Term Term
          | Lam Name Term
          | App Term Term
          | Count
          | Fail
          | Amb Term Term
          | Out Term

-- The naivest way to compose monads is to just compose them manually.
data Stack s e w r = State s (Writer w [Either e r])

data Value = Wrong
           | Num Int
           | Fun (Value -> Stack Int String [Value] Value)

-- We can make our life easier with this nice type alias.
type Computation r = Stack Int String [Value] r

type Env = [(Name, Value)]

-- But the code is insanely hard to write. We would like to use Left to raise
-- an error, but we can't because the outer monad is State, not Either. We
-- need to find a way to "lift" Left into the State monad, but that's quite
-- hard to do with bare monads.
-- Note that the following code DOESN'T work.
lookupEnv :: Name -> Env -> Computation Value
lookupEnv x [] = Left $ "Variable " ++ x ++ " not bound!"
lookupEnv x ((y, v) : env) = if x == y then pure v else lookupEnv x env

tick :: Computation ()
tick = modify (+1)

add :: Value -> Value -> Computation Value
add (Num n) (Num m) = do
    tick
    pure $ Num (n + m)
add _ _ = pure Wrong

apply :: Value -> Value -> Computation Value
apply (Fun f) x = do
    pure tick
    f x
apply _ _ = pure Wrong

interp :: Term -> Env -> Computation Value
interp (Var x) env = lookupEnv x env
interp (Const n) _ = pure (Num n)
interp (Add t1 t2) env = do
    s1 <- interp t1 env
    s2 <- interp t2 env
    add s1 s2
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
    show (App t1 t2) = "(" ++ show t1 ++ ")(" ++ show t2 ++ ")"
    show Count = "Count"
    show Fail = "Fail"
    show (Amb t1 t2) = "Amb (" ++ show t1 ++ ") (" ++ show t2 ++ ")"
    show (Out t) = "Out (" ++ show t ++ ")"

instance Show Value where
    show Wrong = "<wrong>"
    show (Num n) = show n
    show (Fun f) = "<function>"

-- The problem with running the interpreter is that I'm not even sure in what
-- order should the monads be run. Try figuring it out.
test :: Term -> String
test t =
    case runWriter $ runState (interp t []) 0 of
        Left msg -> msg
        Right l -> unlines $
            flip map l $ \((val, state), log) ->
                "log: " ++ show log ++ "\n" ++
                "result: " ++ show val ++ " (in " ++ show state ++ " steps)"

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
failamb_term1 = Amb (Const 100) (Const 12345)

out_term0 :: Term
out_term0 = Out (Add (Out (Const 42)) (Out (Const 54321)))

term1 :: Term
term1 =
    Amb
        (Add
            (Amb count_term0 out_term0)
            (Amb failamb_term0 out_term0))
        (Add term0 count_term1)

testTerms :: [Term]
testTerms =
    [term0, count_term0, count_term1, error_term0, failamb_term0, failamb_term1, out_term0, term1]

main :: IO ()
main = do
    forM_ testTerms $ \t -> do
        putStrLn $ replicate 50 '-'
        putStrLn $ "Interpreting " ++ show t
        putStrLn $ test t
        putStrLn $ replicate 50 '-'