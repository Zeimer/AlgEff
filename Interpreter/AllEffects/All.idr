import Effects
import Effect.State
import Effect.Exception
import Effect.Select

import Control.Monad.Writer
import Control.Monad.Identity

data Log : Type -> Effect where
    Tell : a -> Log a () (List a) (\_ => List a)

LOG : Type -> EFFECT
LOG t = MkEff (List t) (Log t)

tell : a -> Eff () [LOG a]
tell x = call (Tell x)

implementation Handler (Log a) (WriterT (List a) Identity) where
    handle rest (Tell msg) k = do
        tell [msg]
        k () rest

data M s w e r = Err e | Nil | Cons (s -> (r, List w, s)) (M s w e r)


implementation Handler (Log a) (M s (List a) e) where
    handle rest (Tell msg) k = do
        case k () rest of
            Err msg => Err msg
            Nil => Nil
            Cons h t => flip Cons t $ \s =>
                case h s of
                    (r, msgs, s') => (r, msg :: msgs, s')

Name : Type
Name = String

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
           | Fun (Value -> Eff Value [STATE Int, EXCEPTION String, SELECT, LOG Value])

Env : Type
Env = List (Name, Value)

lookupEnv : Name -> Env -> Eff Value [EXCEPTION String]
lookupEnv x [] = raise $ "Variable " ++ x ++ " not bound!"
lookupEnv x ((y, v) :: env) = if x == y then pure v else lookupEnv x env

tick : Eff () [STATE Int]
tick = update (+1)

add : Value -> Value -> Eff Value [STATE Int, EXCEPTION String]
add (Num n) (Num m) = tick *> pure (Num (n + m))
add _ _ = raise "Can't add!"

apply : Value -> Value -> Eff Value [STATE Int, EXCEPTION String, SELECT, LOG Value]
apply (Fun f) x = tick *> f x
apply _ _ = raise "Can't apply!"

interp : Term -> Env -> Eff Value [STATE Int, EXCEPTION String, SELECT, LOG Value]
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
interp Fail _ = select []
interp (Amb t1 t2) env = do
    b <- select [True, False]
    if b then interp t1 env else interp t2 env
interp (Out t) env = do
    v <- interp t env
    tell v
    pure v

Show Term where
    show (Var x) = x
    show (Const n) = show n
    show (Add t1 t2) = show t1 ++ " + (" ++ show t2 ++ ")"
    show (Lam x t) = "λ" ++ x ++ "." ++ show t
    show (App t1 t2) = "(" ++ show t1 ++ ")" ++ show t2
    show (Count) = "Count"
    show Fail = "Fail"
    show (Amb t1 t2) = "Amb (" ++ show t1 ++ ") (" ++ show t2 ++ ")"
    show (Out t) = "Out (" ++ show t ++ ")"

Show Value where
    show Wrong = "<wrong>"
    show (Num n) = show n
    show (Fun f) = "<function>"

test : Term -> String
test t = ?t
{-
    case the (Maybe _) $ run (interp t []) of
        Left msg => msg
        Right l => unlines $
            for l $ \((val, state), log) =>
                "log: " ++ log ++ "\n" ++
                "result: " ++ show val ++ " (in " ++ show state ++ " steps)"
-}
{-
    case runIdentity $ runWriterT $ the (Writer String _) $ run (interp t []) of
        Left msg => msg
        Right l => unlines $
            for l $ \((val, state), log) =>
                "log: " ++ log ++ "\n" ++
                "result: " ++ show val ++ " (in " ++ show state ++ " steps)"
-}

term0 : Term
term0 = App (Lam "x" (Add (Var "x") (Var "x")))
            (Add (Const 10) (Const 11))

count_term0 : Term
count_term0 = Add Count (Add Count Count)

count_term1 : Term
count_term1 = Add (Add Count Count) Count
            
error_term0 : Term
error_term0 = Var "O BOŻE TO JEST ZBOŻE"

failamb_term0 : Term
failamb_term0 = Add (Const 42) Fail

failamb_term1 : Term
failamb_term1 = Amb (Const 100) (Const 1234567890)

testTerms : List Term
testTerms = [term0, count_term0, count_term1, error_term0, failamb_term0, failamb_term1]

main : IO ()
main = do
    for_ testTerms $ \t => do
        putStrLn $ "Interpreting " ++ show t
        putStrLn $ test t