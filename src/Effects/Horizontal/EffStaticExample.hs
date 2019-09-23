{-# LANGUAGE GADTs #-}

module Effects.Horizontal.EffStaticExample where

import Effects.Horizontal.Eff
import Effects.Horizontal.Static.Union2
import Effects.Future
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef (newIORef, readIORef, modifyIORef)
import Data.List (find, deleteBy)

data Entry = Entry { index :: String, version :: Integer, payload :: Int } deriving (Show)

dbRef = unsafePerformIO $ newIORef [Entry { index = "a", version = 1, payload = 2 }]

fromDB :: String -> IO (Maybe Entry)
fromDB id = do
        db <- readIORef dbRef
        return (find value db)
    where value = (== id) . index 

hash :: Entry -> String
hash (Entry id version payload) = id <> "::" <> (show version) <> "::" <> (show payload)

get :: String -> Comp IO (Maybe Entry)
get id = send (fromDB id)

encrypt :: Maybe Entry -> Comp Future (Maybe String)
encrypt (Just entry) = send (Async $ return $ Just $ hash entry)
encrypt (Nothing) = send (Sync $ Nothing)

runGet :: Comp (Union IO r2) a -> Comp r2 a
runGet (Value a)          = Value a
runGet (Effect (P1 io) f) = runGet $ f (unsafePerformIO io)
runGet (Effect (P2 r) f)  = Effect r (\x -> runGet $ f x)

runEncrypt :: Comp Future a -> a
runEncrypt (Value a)             = a
runEncrypt (Effect (Async io) f) = runEncrypt $ f $ unsafePerformIO io
runEncrypt (Effect (Sync a) f)   = runEncrypt $ f a

readEncrypt :: String -> Comp (Union IO Future) (Maybe String)
readEncrypt id = do
    mentry    <- injectP1 $ get id
    encrypted <- injectP2 $ encrypt mentry
    return encrypted

main :: IO ()
main = putStrLn $ show $ runEncrypt $ runGet $ readEncrypt "a"