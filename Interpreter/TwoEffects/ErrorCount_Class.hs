{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

import Control.Monad.Trans.Except
import Control.Monad.State
import Control.Monad.Error.Class

type Name = String

data Term = Var Name
          | Const Int
          | Add Term Term
          | Lam Name Term
          | App Term Term
          | Count

-- This time the idea for doing effects is to parametrize our type of values
-- with an arbitrary m of kind * -> * and then constrain it using classes
-- like MonadError and MonadState, and run it using some concrete monad stack.
-- This leads to the somewhat ugly-looking m (Value m).
data Value m = Wrong
             | Num Int
             | Fun (Value m -> m (Value m))

-- Our design choice spills to related types: now we have to write Env m
-- instead of just Env, like before, but this isn't too much trouble.
type Env m = [(Name, Value m)]

-- This is how it works in practice: we constrain m using MonadError Stirng m,
-- so that we can now use the function throwError.
--
-- Note that this improves modularity a lot, because with transformers we had
-- to write the type of this as ExceptT String (State Int) Value. If we wanted
-- it to mention just ExceptT, we would have had to do some ugly lifting.
--
-- Also note that we need the pragma FlexibleContexts to be able to write
-- MonadError String m, which would be otherwise considered an error, because
-- String is not a type variable.
lookupEnv :: MonadError String m => Name -> Env m -> m (Value m)
lookupEnv x [] = throwError $ "Variable " ++ x ++ " not bound!"
lookupEnv x ((y, v) : env) = if x == y then pure v else lookupEnv x env

tick :: MonadState Int m => m ()
tick = modify (+1)

-- One sad thing is that the more effects we want to use, the longer our list
-- of constraints becomes. This one is about 25 characters longer than the one
-- using transformers.
add :: (MonadError String m, MonadState Int m) => Value m -> Value m -> m (Value m)
add (Num n) (Num m) = tick >> pure (Num (n + m))
add _ _ = throwError $ "Can't add!"

-- But we can solve this problem with the pragma ConstraintKinds. Thanks to it we
-- can define a new type alias and use it instead of the long constraint list.
-- This makes our type signatures even shorter than before! However, the problem
-- persists for cases in which we don't use all the effects - we then have to list
-- them manually (or give the shorter, but less meaningful list of effects using
-- the alias).
type Eff m = (MonadError String m, MonadState Int m)

apply :: Eff m => Value m -> Value m -> m (Value m)
apply (Fun f) x = tick >> f x
apply _ _ = throwError $ "Can't apply!"

interp :: Eff m => Term -> Env m -> m (Value m)
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

instance Show Term where
    show (Var x) = x
    show (Const n) = show n
    show (Add t1 t2) = show t1 ++ " + (" ++ show t2 ++ ")"
    show (Lam x t) = "λ" ++ x ++ "." ++ show t
    show (App t1 t2) = "(" ++ show t1 ++ ")" ++ show t2
    show (Count) = "Count"

instance Show (Value m) where
    show Wrong = "<wrong>"
    show (Num n) = show n
    show (Fun f) = "<function>"

-- We can run and test our interpreter the same way as before - we exploit the
-- fact that the monad transformer stack
--
-- ExceptT String (State Int) (Value (ExceptT String (State Int)))
--
-- implements the classes we need. This is another huge win for us, because we
-- need to specify the order of effect handling only when we want to handle
-- them and not in every type signature.
--
-- One sad thing is that the type of the above transformer is ugly due to how
-- we defined Value, but the modularity is worth it.
test :: Term -> String
test t =
    case runState (runExceptT (interp t [])) 0 of
        (Left msg, _) -> msg
        (Right v, s) -> show v ++ " (in " ++ show s ++ " steps)"

term0 :: Term
term0 = App (Lam "x" (Add (Var "x") (Var "x")))
            (Add (Const 10) (Const 11))

count_term0 :: Term
count_term0 = Add Count (Add Count Count)

count_term1 :: Term
count_term1 = Add (Add Count Count) Count
            
error_term0 :: Term
error_term0 = Var "O BOŻE TO JEST ZBOŻE"

testTerms :: [Term]
testTerms = [term0, count_term0, count_term1, error_term0]

main :: IO ()
main = do
    forM_ testTerms $ \t -> do
        putStrLn $ "Interpreting " ++ show t
        putStrLn $ test t