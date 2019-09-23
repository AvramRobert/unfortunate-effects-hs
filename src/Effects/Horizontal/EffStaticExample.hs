{-# LANGUAGE GADTs #-}

module Effects.Horizontal.EffStaticExample where

import Effects.Horizontal.Eff
import qualified Effects.Horizontal.Static.Union2 as U2
import qualified Effects.Horizontal.Static.Union3 as U3
import Effects.Future
import Effects.Writer
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

echo :: String -> Comp (Writer [String]) ()
echo msg = send (tell [msg])

decomp :: Comp (U3.Union r1 r2 r3) a -> Comp (U2.Union r2 r3) a
decomp (Value a)            = Value a
decomp (Effect (U3.P2 r) f) = Effect (U2.P1 r) (\x -> decomp $ f x) 
decomp (Effect (U3.P3 r) f) = Effect (U2.P2 r) (\x -> decomp $ f x)

runGet :: Comp (U3.Union IO r2 r3) a -> Comp (U2.Union r2 r3) a
runGet (Effect (U3.P1 io) f) = decomp $ f (unsafePerformIO io)
runGet computation           = decomp computation

runEncrypt :: Comp (U2.Union Future r2) a -> Comp r2 a
runEncrypt (Value a)                     = (Value a)
runEncrypt (Effect (U2.P1 (Async io)) f) = runEncrypt $ f $ unsafePerformIO io
runEncrypt (Effect (U2.P1 (Sync a)) f)   = runEncrypt $ f a
runEncrypt (Effect (U2.P2 r) f)          = Effect r (\x -> runEncrypt $ f x)

runLog :: Comp (Writer [String]) a -> ([String], a)
runLog (Value a)                  = ([], a)
runLog (Effect (Writer logs a) f) = let (rest, x) = runLog (f a)
                                    in (logs <> rest, x)

readEncrypt :: String -> Comp (U3.Union IO Future (Writer [String])) (Maybe String)
readEncrypt id = do
    mentry    <- U3.injectP1 $ get id
    _         <- U3.injectP3 $ echo ("Entry is " <> (show mentry))
    encrypted <- U3.injectP2 $ encrypt mentry
    _         <- U3.injectP3 $ echo ("Encryped entry")
    _         <- U3.injectP3 $ echo ("Value is: " <> (show encrypted))
    return encrypted

main :: IO ()
main = let (logs, _) = runLog $ runEncrypt $ runGet $ readEncrypt "a"
       in putStrLn $ (unlines logs)