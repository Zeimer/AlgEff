import Control.Monad.Free
import System.Exit hiding (ExitSuccess)

-- NOTE: this code is adapted from
-- http://www.haskellforall.com/2012/07/purify-code-using-free-monads.html

-- The free monad construction is a way to create a monad from any functor
-- whatsoever. This is one possible way of creating custom computational
-- effects. If we want a new effect, we define it using algebraic data types,
-- define a functor instance for it and then transform it into a monad using
-- the free monad construction.

-- In our example, we want to have a computational effect which lets us read
-- from the standard input and write to the standard output, but we don't
-- want to use IO for this, because IO also allows opening files, connecting
-- to the Internet, throwing IO exceptions and so on.

data ConsoleF a
    = PutStrLn String a
    | GetLine (String -> a)

-- To define this effect, first we define a datatype ConsoleF. We want this
-- to be a functor, so it has to have a type parameter a. The two operations
-- we permit, namely reading and writing, are represented using constructors.
--
-- PutStrLn represents the operation of writing its argument to the standard
-- output. The first argument is the string we want to write and the second
-- one represents the "rest" of the computation.
--
-- GetLine represents the operation of reading from the standard input. The
-- argument of type String -> a means that for every possible input we can
-- get (which is going to be a string) we need to return the rest of the
-- computation, which is represented by the argument type a.

instance Functor ConsoleF where
    fmap f (PutStrLn str x) = PutStrLn str (f x)
    fmap f (GetLine k) = GetLine (f . k)

-- The functor instance is easy. For each constructor, we just need to pass
-- f deeper.

type Console = Free ConsoleF

-- To get our monad, we just need to define Console to be Free ConsoleF.
-- The definition of Free is
--
-- data Free (f :: * -> *) a = Pure a | Free (f (Free f a))

-- The first constructor will be the return of our monad. The second looks like
-- join - it allows us to collapse one layer of the functor f. Thanks to this,
-- Free f is a monad for any f that is a functor.

putStrLn' :: String -> Console ()
putStrLn' str = liftF $ PutStrLn str ()

getLine' :: Console String
getLine' = liftF $ GetLine id

-- We can't directly use PutStrLn and GetLine, since they are sitting in the
-- bare datatype ConsoleF and we want to use them monadically. To get around
-- this, we write some wrappers.
--
-- liftF :: (Functor f, MonadFree f m) => f a -> m a
--
-- The function liftF can be used to lift any computation from our functor
-- ConsoleF into the monad Console.

echo :: Console ()
echo = do
    putStrLn' "Write something!"
    str <- getLine'
    putStrLn' $ "You wrote: " ++ str

-- We can now write a simple echo program which pings back the message that the
-- user entered. Thanks to its type, Console (), we can be sure that it can't do
-- any kind of evil things, like hacking us.

run :: Console a -> IO a
run (Pure x) = return x
run (Free (PutStrLn str x)) = putStrLn str >> run x
run (Free (GetLine f)) = getLine >>= run . f

-- To actually run a program which uses the Console effect, we have to somehow
-- interpret it. The easiest way of doing this is to transform a computation of
-- type Console a into a computation of type IO a, which can then be executed.

main :: IO ()
main = run echo >> return ()

-- main works as if we wrote it without all this hassle and we can't see from
-- it's type that it doesn't do evil IO things. But as soon as we look up the
-- type of echo and run, we are assure that we are really safe.