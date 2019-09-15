module Effects.Vertical.TransformersExample where

import Effects.Lift
import Effects.Vertical.Transformers.EitherT
import Effects.Vertical.Transformers.MaybeT
import Effects.Vertical.Transformers.FutureT
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

fetch :: String -> IO (Maybe Entry)
fetch id = do
        db <- readIORef dbRef
        return (find value db)
    where value = (== id) . index 

insert :: Entry -> IO ()
insert entry = modifyIORef dbRef (insert entry)
    where insert entry db = entry : (deleteBy id entry db)
          id this that    = (index this) == (index that) 

lookupInDB :: String -> DbOperation Entry
lookupInDB id = MaybeT (FutureT (EitherT (io (Right (Async (fetch id))))))

writeToDB :: Entry -> DbOperation ()
writeToDB e = lift (FutureT (EitherT (io (Right (Async (insert e)))))) 

updateInDB :: String -> (Entry -> Entry) -> DbOperation ()
updateInDB id f = do
        entry <- lookupInDB id
        let newEntry = f entry
        _     <- writeToDB newEntry
        return ()

unwrap :: DbOperation a -> IO (Either Error (Future (Maybe a)))
unwrap = runEitherT . runFutureT . runMaybeT