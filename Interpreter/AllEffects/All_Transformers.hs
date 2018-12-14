{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

import Control.Monad.State
import Control.Monad.Trans.Except
import Control.Monad.Error.Class
import Control.Monad.Writer
import Control.Monad.Trans.List
import Control.Monad.Identity

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

type Stack s w e r = StateT s (WriterT w (ListT (Either e))) r

data Value = Wrong
           | Num Int
           | Fun (Value -> Stack Int [Value] String Value)

type Computation r = Stack Int [Value] String r

type Env = [(Name, Value)]

lookupEnv :: Name -> Env -> Computation Value
lookupEnv x [] = throwError $ "Variable " ++ x ++ " not bound!"
lookupEnv x ((y, v) : env) = if x == y then pure v else lookupEnv x env

tick :: Computation ()
tick = modify (+1)
    
add :: Value -> Value -> Computation Value
add (Num n) (Num m) = tick >> pure (Num (n + m))
add _ _ = throwError $ "Can't add!"

apply :: Value -> Value -> Computation Value
apply (Fun f) x = tick >> f x
apply f _ = throwError $ show f ++ " should be a function!"

interp :: Term -> Env -> Computation Value
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
interp Fail _ = mzero -- lift $ lift []
interp (Amb t1 t2) env = mplus (interp t1 env) (interp t2 env)
interp (Out t) env = do
    v <- interp t env
    tell [v]
    pure v

instance Show Term where
    show (Var x) = x
    show (Const n) = show n
    show (Add t1 t2) = show t1 ++ " + (" ++ show t2 ++ ")"
    show (Lam x t) = "λ" ++ x ++ "." ++ show t
    show (App t1 t2) = "(" ++ show t1 ++ ")" ++ show t2
    show (Count) = "Count"
    show Fail = "Fail"
    show (Amb t1 t2) = "Amb (" ++ show t1 ++ ") (" ++ show t2 ++ ")"
    show (Out t) = "Out (" ++ show t ++ ")"

instance Show Value where
    show Wrong = "<wrong>"
    show (Num n) = show n
    show (Fun f) = "<function>"

test :: Term -> String
test t =
    case runListT $ runWriterT $ runStateT (interp t []) 0 of
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