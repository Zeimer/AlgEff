{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

import Control.Monad.State
import Control.Monad.Trans.List
import Control.Monad.Writer

-- We can make composing monads manageable with monad transformers. They are
-- things of kind (* -> *) -> (* -> *) which take monads on input and return
-- monads on output. Each monad (like State s, Either e, [], Writer w, etc.)
-- has a corresponding monad transformer. In fact, the ordinary monads are
-- implemented using transformers, so that for example Writer w is a synonym
-- for WriterT w Identity, where Identity is the identity monad.

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

-- Our transformer stack looks similar to our monad stack from All_Monads.hs,
-- but there's a slight difference. To "compose" monads (or rather, to pretend
-- to compose monads) we would write e.g. State s (Either e r). To compose two
-- transformers, we write StateT s (Either e) r. Here the monad Either e is an
-- argument to StateT s, whereas previously as an argument to State s we woudl
-- give just the type Either e r.
type Stack s e w r = StateT s (WriterT w (ListT (Either e))) r

data Value = Wrong
           | Num Int
           | Fun (Value -> Stack Int String [Value] Value)

type Computation r = Stack Int String [Value] r

type Env = [(Name, Value)]

-- lift is a function that makes an operation from an underlying monad
-- accessible at the level of the monad transformer. Its type signature is
--
-- lift :: (Monad m, MonadTrans t) => m a -> t m a
--
-- To actually use Left (which is of type Either String a) as a value of type
-- Computation Value, which is a synonym for three layers of monad transformers
-- on top of Either String, we have to call lift thrice. The first lift lifts
-- Left to ListT, the second lifts that to WriterT and the third one lifts the
-- lifted thing to StateT.
--
-- Yo man! We heard you like to lift, so we built a lift into your lift so you can
-- lift while you lift.
--
-- Raising an error looks very ugly, but at least it is manageable, unlike in the
-- case of bare monads. Notice that pure works well without any lifting.
lookupEnv :: Name -> Env -> Computation Value
lookupEnv x [] = lift $ lift $ lift $ Left $ "Variable " ++ x ++ " not bound!"
lookupEnv x ((y, v) : env) = if x == y then pure v else lookupEnv x env

-- Increasing the counter is similar to before, but we have to use StateT
-- instead of state and we have to include an additional call to pure,
-- because the signature of StateT is
--
-- StateT :: (s -> m (a, s)) -> StateT s m a
--
-- where m is the inner monad.
tick :: Computation ()
tick = StateT $ \s -> pure ((), s + 1)

-- To raise an error, we once more have to use quite a few lifts...
add :: Value -> Value -> Computation Value
add (Num n) (Num m) = tick >> pure (Num (n + m))
add _ _ = lift $ lift $ lift $ Left $ "Can't add!"

apply :: Value -> Value -> Computation Value
apply (Fun f) x = tick >> f x
apply f _ = lift $ lift $ lift $ Left $ show f ++ " should be a function!"

-- Previously we used operations like get and tell to handle state and logging.
-- This was very comfortable. Thus, to feel how bad transformers really are,
-- let's handle our effects without using such operations.
--
-- To interpret Fail, we have to create an empty ListT, which we can do with
-- ListT $ pure []. Then we have to lift it twice so it can be used as a
-- value of type Computation Value.
--
-- To interpret Amb, we shamelessly use the mplus function, which comes from
-- the class MonadPlus. If you think monad transformers are easy to use, try
-- interpreting Amb without using mplus.
--
-- Finally for logging we use
--
-- lift $ WriterT $ pure ((), [v])
--
-- Let's dissect this expression. First, pure ((), [v]) creates a computation
-- of type ListT (Either String) ((), [Value]). Then we call WriterT which
-- transformers into something of type WriterT [Value] (ListT (Either String))).
-- Finally we have to lift it into the StateT transformer.
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
interp Count _ = StateT $ \s -> pure (Num s, s)
interp Fail _ = lift $ lift $ ListT $ pure []
interp (Amb t1 t2) env = mplus (interp t1 env) (interp t2 env)
interp (Out t) env = do
    v <- interp t env
    lift $ WriterT $ pure ((), [v])
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

instance Show Value where
    show Wrong = "<wrong>"
    show (Num n) = show n
    show (Fun f) = "<function>"

-- Running our interpreter is quite easy. We run the monadic layers one by one,
-- going from the outer one, which is StateT, then through WriterT and finally
-- ListT. What we get is a computation sitting in the innermost monad, which is
-- Either String. Thus we get something of type
--
-- Either String [((Value, Int), [Value])]
--
-- We then do error handling and map over the list of results to print them all.
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

-- A big weakness of monad transformer stacks is that they are fixed. We have
-- to think up front what the ordering of computational effects will be. If we
-- want to change it, we have to refactor our code, which might not be the
-- easiest thing to do with all those lifts sitting 'round there...
main :: IO ()
main = do
    forM_ testTerms $ \t -> do
        putStrLn $ replicate 50 '-'
        putStrLn $ "Interpreting " ++ show t
        putStrLn $ test t
        putStrLn $ replicate 50 '-'