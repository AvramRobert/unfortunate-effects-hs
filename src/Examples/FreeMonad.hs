{-# LANGUAGE GADTs #-}

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

hash :: Entry -> String
hash (Entry id version payload) = id <> "::" <> (show version) <> "::" <> (show payload)

getComp :: String -> IO (Maybe Entry)
getComp id = fmap (find value) $ readIORef dbRef
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

run :: DBOp (Maybe String) -> Writer [String] (Maybe String)
run = interpret unravel
    where unravel (Get id f)         = return $ f $ unsafePerformIO $ getComp id
          unravel (Encrypt entry f)  = return $ f $ perform $ encryptComp entry
          unravel (Echo msg f)       = fmap f $ echoComp msg
          perform (Async io)         = unsafePerformIO io
          perform (Sync a)           = a

-- I can sort-of write inter-dependent computations, but I have to coerce them to the same return type and monad of choice
-- If I want logs, I have to coerce them to `Writer`, so IO shit has to be done separately 
-- The function in every branch of the algebra coerces the things accordingly, but this just looks wrong.

readEncrypt :: String -> DBOp (Maybe String)
readEncrypt id = do
    mentry    <- get id
    _         <- echo ("Entry is " <> (show mentry))
    encrypted <- encrypt mentry
    _         <- echo ("Encryped entry")
    _         <- echo ("Value is: " <> (show encrypted))
    return encrypted

main :: IO ()
main = let (Writer logs _) = run $ readEncrypt "a"
       in putStrLn $ unlines logs