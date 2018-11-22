import Control.Monad.Free
import System.Exit hiding (ExitSuccess)

data TeletypeF x
  = PutStrLn String x
  | GetLine (String -> x)
  | ExitSuccess

instance Functor TeletypeF where
    fmap f (PutStrLn str x) = PutStrLn str (f x)
    fmap f (GetLine      k) = GetLine (f . k)
    fmap f  ExitSuccess     = ExitSuccess

type Teletype = Free TeletypeF

putStrLn' :: String -> Teletype ()
putStrLn' str = liftF $ PutStrLn str ()

getLine' :: Teletype String
getLine' = liftF $ GetLine id

exitSuccess' :: Teletype r
exitSuccess' = liftF ExitSuccess

run :: Teletype r -> IO r
run (Pure r) = return r
run (Free (PutStrLn str t)) = putStrLn str >>  run t
run (Free (GetLine  f    )) = getLine      >>= run . f
run (Free  ExitSuccess    ) = exitSuccess

echo :: Teletype ()
echo = do str <- getLine'
          putStrLn' str
          exitSuccess'
          putStrLn' "Finished"