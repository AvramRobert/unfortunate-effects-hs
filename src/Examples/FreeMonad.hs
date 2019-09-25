{-# LANGUAGE GADTs, RankNTypes #-}

module Examples.FreeMonad where

import Effects.Vertical.Free.Free
import Effects.Future
import Effects.Writer
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef (newIORef, readIORef, modifyIORef)
import Data.List (find, deleteBy)
import Data.Monoid ((<>))

data Entry = Entry { index :: String, version :: Integer, payload :: Int } deriving (Show)

data Op a = 
    Get String (Maybe Entry -> a)             |
    Encrypt (Maybe Entry) (Maybe String -> a) |
    Echo String (() -> a)

type DBOp = Free Op

instance Functor Op where
    fmap f (Get x g)     = Get x (f . g)
    fmap f (Encrypt m g) = Encrypt m (f . g)
    fmap f (Echo s g)    = Echo s (f . g)

dbRef = unsafePerformIO $ newIORef [Entry { index = "a", version = 1, payload = 2 }]

fromDB :: String -> IO (Maybe Entry)
fromDB id = fmap (find value) $ readIORef dbRef
    where value = (== id) . index

hash :: Entry -> String
hash (Entry id version payload) = id <> "::" <> (show version) <> "::" <> (show payload)

getComp :: String -> IO (Maybe Entry)
getComp id = do
        db <- readIORef dbRef
        return (find value db)
    where value = (== id) . index 

encryptComp :: Maybe Entry -> Future (Maybe String)
encryptComp (Just entry) = Async $ return $ Just $ hash entry
encryptComp (Nothing)    = Sync Nothing

echoComp :: String -> Writer [String] ()
echoComp msg = tell [msg]

get :: String -> DBOp (Maybe Entry)
get index = liftF $ Get index id

encrypt :: Maybe Entry -> DBOp (Maybe String)
encrypt mentry = liftF $ Encrypt mentry id

echo :: String -> DBOp ()
echo msg = liftF $ Echo msg id

readEncrypt :: String -> DBOp (Maybe String)
readEncrypt id = do
    mentry    <- get id
    _         <- echo ("Entry is " <> (show mentry))
    encrypted <- encrypt mentry
    _         <- echo ("Encryped entry")
    _         <- echo ("Value is: " <> (show encrypted))
    return encrypted