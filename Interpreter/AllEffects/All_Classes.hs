{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

import Control.Monad.Trans.Except
import Control.Monad.State
import Control.Monad.Error.Class
import Control.Monad.Writer
import Control.Monad.Identity
import Control.Monad.Trans.List
import Control.Monad.Trans

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

data Value m = Wrong
           | Num Int
           | Fun (Value m -> m (Value m))

type Env m = [(Name, Value m)]

lookupEnv :: MonadError String m => Name -> Env m -> m (Value m)
lookupEnv x [] = throwError $ "Variable " ++ x ++ " not bound!"
lookupEnv x ((y, v) : env) = if x == y then pure v else lookupEnv x env

tick :: MonadState Int m => m ()
tick = modify (+1)
    
add :: (MonadError String m, MonadState Int m) => Value m -> Value m -> m (Value m)
add (Num n) (Num m) = tick >> (pure $ Num (n + m))
add _ _ = throwError $ "Can't add!"

apply :: (MonadError String m, MonadState Int m) => Value m -> Value m -> m (Value m)
apply (Fun f) x = tick >> f x
apply f _ = throwError $ show f ++ " should be a function!"

class Monad m => MonadNondet m where
    fail' :: m a
    choice :: m Bool

instance MonadNondet [] where
    fail' = []
    choice = [True, False]

-- It would be good if this worked, but sadly it doesn't. #PrayForHaskell
{-
instance (MonadNondet m, MonadTranst t) => MonadNondet (t m) where
    fail' = lift fail'
    choice = lift choice
-}

instance MonadNondet m => MonadNondet (StateT s m) where
    fail' = lift fail'
    choice = lift choice

instance MonadNondet m => MonadNondet (ExceptT e m) where
    fail' = lift fail'
    choice = lift choice

instance Monad m => MonadNondet (ListT m) where
    fail' = ListT $ pure []
    choice = ListT $ pure [True, False]

instance (MonadNondet m, Monoid w) => MonadNondet (WriterT w m) where
    fail' = lift fail'
    choice = lift choice

{-
select :: MonadNondet m => [a] -> m a
select [] = fail'
select (x:xs) = do
    b <- choice
    if b then pure x else select xs
-}

interp ::
    (MonadState Int m, MonadError String m, MonadNondet m, MonadWriter String m) =>
        Term -> Env m -> m (Value m)
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
interp Fail _ = fail'
interp (Amb t1 t2) env = do
    b <- choice
    if b then interp t1 env else interp t2 env
interp (Out t) env = do
    v <- interp t env
    tell $ show v ++ "; "
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

instance Show (Value m) where
    show Wrong = "<wrong>"
    show (Num n) = show n
    show (Fun f) = "<function>"

test :: Term -> String
test t =
    case runIdentity $ runExceptT $ runListT $ runWriterT $ runStateT (interp t []) 0 of
        Left msg -> msg
        Right l -> unlines $
            flip map l $ \((val, state), log) ->
                "log: " ++ log ++ "\n" ++
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