module Examples.MonadTransformers where

import Effects.Vertical.Lift
import Effects.Vertical.Transformers.EitherT
import Effects.Vertical.Transformers.MaybeT
import Effects.Vertical.Transformers.FutureT
import Effects.Writer
import Effects.Future (Future (Async, Sync))
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef (newIORef, readIORef, modifyIORef)
import Data.List (find, deleteBy)

newtype Error = Error String deriving (Show)

type DbOperation a = MaybeT (FutureT (EitherT IO Error)) a

data Entry = Entry { index :: String, version :: Integer, payload :: Int } deriving (Show)

dbRef = unsafePerformIO $ newIORef [Entry { index = "a", version = 1, payload = 2 }]

io :: a -> IO a
io = return

fromDB :: String -> IO (Maybe Entry)
fromDB id = do
        db <- readIORef dbRef
        return (find value db)
    where value = (== id) . index 

hash :: Entry -> String
hash (Entry id version payload) = id <> "::" <> (show version) <> "::" <> (show payload)

encrypt :: Maybe Entry -> Future (Maybe String)
encrypt (Just entry) = Async $ return $ Just $ hash entry
encrypt (Nothing) = Sync $ Nothing

echo :: String -> Writer [String] ()
echo msg = tell [msg]