{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

import Control.Monad.State
import Control.Monad.Error.Class
import Control.Monad.Trans.List
import Control.Monad.Writer

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

-- This time the idea for doing effects is to parametrize our type of values
-- with an arbitrary m of kind * -> * and then constrain it using classes
-- like MonadState and MonadError, and run it using some concrete monad stack.
-- This leads to the somewhat ugly-looking codomain of our functions, namely
-- m (Value m).
data Value m = Wrong
           | Num Int
           | Fun (Value m -> m (Value m))

-- Our design choice spills to related types: now we have to write Env m
-- instead of just Env, like before, but this isn't too much trouble.
type Env m = [(Name, Value m)]

-- Haskell doesn't have a good class for nondeterminism, so we have to make it
-- ourselves. m can be MonadNondet when it's a monad that has two additional
-- operations: fail' and choice. They will fit perfectly for handling Fail and Amb.
--
-- The prime in the name of fail' comes from the fact that there's a function
-- named fail, which is a part of the Monad class, but this is a design mistake
-- on the part of the standard library, because failing need not make sense in
-- an arbitrary monad.
class Monad m => MonadNondet m where
    fail' :: m a
    choice :: m Bool

-- The list monad of course is a natural candidate for the nondeterminism monad.
-- Failure can be represented with an empty list and choice with a two element
-- list, containig True and False. We will however not use this instance, because
-- we will be using ListT and not the plain old [].
instance MonadNondet [] where
    fail' = []
    choice = [True, False]

instance Monad m => MonadNondet (ListT m) where
    fail' = ListT $ pure []
    choice = ListT $ pure [True, False]

-- It would be good if we could just make an instance saying that nondeterministic
-- monads get transformed into nondeterministic monads, but sadly this doesn't work.
-- Even if we turn on the FlexibleInstances and UndecidableInstances languages
-- extensions, we have a problem with overlapping instances. We thus have to write
-- some boilerplate below to get around this. This situation is extremely annoying
-- in the bigger picture, because if we have N monad transformers, we have to write
-- N^2 instances to make everything work as intended.
{-
instance (MonadNondet m, MonadTrans t, Monad (t m)) => MonadNondet (t m) where
    fail' = lift fail'
    choice = lift choice
-}

instance MonadNondet m => MonadNondet (StateT s m) where
    fail' = lift fail'
    choice = lift choice

instance (MonadNondet m, Monoid w) => MonadNondet (WriterT w m) where
    fail' = lift fail'
    choice = lift choice

-- This is how classes work in practice: we constrain m using
-- MonadError Stirng m, so that we can now use the function throwError.
--
-- Note that this improves modularity a lot, because we only need to mention
-- one class to make this work. With transformers, we had to to mention the
-- whole transformer stack explicitly (or we could have done it like here, but
-- then we would need to do some heavy lifting).

-- Also note that we need the pragma FlexibleContexts to be able to write
-- MonadError String m, which would be otherwise considered an error, because
-- String is not a type variable.
lookupEnv :: MonadError String m => Name -> Env m -> m (Value m)
lookupEnv x [] = throwError $ "Variable " ++ x ++ " not bound!"
lookupEnv x ((y, v) : env) = if x == y then pure v else lookupEnv x env

-- We don't need to do any shenanigans with the StateT constructor, because
-- we can use the function modify, which works for any monad which has an
-- instance of MonadState.
tick :: MonadState Int m => m ()
tick = modify (+1)

-- Combining classes is easy: we just add more constraints. The sad thing is
-- that the more effects we want to use, the longer our list of constraints
-- becomes. This signature of add is about 40 characters longer than when we
-- used transformers.
add :: (MonadState Int m, MonadError String m) => Value m -> Value m -> m (Value m)
add (Num n) (Num m) = tick >> (pure $ Num (n + m))
add _ _ = throwError $ "Can't add!"

apply :: (MonadState Int m, MonadError String m) => Value m -> Value m -> m (Value m)
apply (Fun f) x = tick >> f x
apply f _ = throwError $ show f ++ " should be a function!"

-- But we can solve this problem with the pragma ConstraintKinds. Thanks to it we
-- can define a new type alias and use it instead of the long constraint list.
-- This makes our type signatures even shorter than before! However, the problem
-- persists for cases in which we don't use all the effects - we then have to list
-- them manually (or give the shorter, but less precise list of effects using the
-- alias).
type Computation m =
    (MonadState Int m, MonadError String m, MonadNondet m, MonadWriter String m)

-- Interpreting our calculus is so much easier using classes.
-- For Count, we can simply use the operation get :: MonadState s m => m s
-- which returns the current state.
-- For Fail and Amb, we use the operations fail' and choice from our MonadNondet
-- class that we tailored precisely to fir this purpose.
-- For Out, we use tell :: MonadWriter String m => m () which appends a message
-- to the log. Note that our log is now a String instead of [Value m]. We can't
-- use the latter, because our Value type is now parametrized by m, so that if
-- we wanted a log of type [Value m], we would need to construct the infinite
-- type
--
-- Value m ~ StateT Int (WriterT [Value m] (ListT (Either String))) (Value m).
--
-- This is impossible, as ghc would tell us if we tried to do that. Therefore,
-- classes are not perfect and have their own little quirks.
interp :: Computation m => Term -> Env m -> m (Value m)
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
    show (App t1 t2) = "(" ++ show t1 ++ ")(" ++ show t2 ++ ")"
    show Count = "Count"
    show Fail = "Fail"
    show (Amb t1 t2) = "Amb (" ++ show t1 ++ ") (" ++ show t2 ++ ")"
    show (Out t) = "Out (" ++ show t ++ ")"

instance Show (Value m) where
    show Wrong = "<wrong>"
    show (Num n) = show n
    show (Fun f) = "<function>"

-- We can run and test our interpreter the same way as we did before. This is
-- possible because monad transformers and the base monads implement the classes
-- we need. We can think of it similarly to Idris: we can "run" an effectful
-- computation (where the effect is represented by a class) in a monadic context
-- (which in this case is built using monad transformers).
--
-- However, note that now our code is way more modular, because we could also run
-- the effects in different order without having to do any refactoring. This is a
-- huge win if we change our mind in the future and it's possible because we have
-- to specify a particular transformer stack only when we want to run our code, not
-- when we're writing it.
test :: Term -> String
test t =
    case runListT $ runWriterT $ runStateT (interp t []) 0 of
        Left msg -> msg
        Right l -> unlines $
            flip map l $ \((val, state), log) ->
                "log: " ++ log ++ "\n" ++
                "result: " ++ show val ++ " (in " ++ show state ++ " steps)"

-- We can also run our effects in a different order, so that, for example,
-- we cant count how many steps were performed before we encountered an
-- error. Doing the same the transformers way would make us copy-paste and
-- hand-adjust the whole file.
--
-- EXERCISE: I'm too lazy to do this. Do it yourself.
--test' :: Term -> String
--test' t =

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
    -- Uncomment this when you have solved the exercise.
    {-
    forM_ testTerms $ \t -> do
        putStrLn $ replicate 50 '-'
        putStrLn $ "Interpreting " ++ show t
        putStrLn $ test t
        putStrLn $ replicate 50 '-'
    -}