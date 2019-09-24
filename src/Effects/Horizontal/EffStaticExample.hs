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
import Data.Monoid ((<>))

data Entry = Entry { index :: String, version :: Integer, payload :: Int } deriving (Show)

dbRef = unsafePerformIO $ newIORef [Entry { index = "a", version = 1, payload = 2 }]

fromDB :: String -> IO (Maybe Entry)
fromDB id = do
        db <- readIORef dbRef
        return (find value db)
    where value = (== id) . index 

hash :: Entry -> String
hash (Entry id version payload) = id <> "::" <> (show version) <> "::" <> (show payload)

getComp :: String -> Comp IO (Maybe Entry)
getComp id = send (fromDB id)

encryptComp :: Maybe Entry -> Comp Future (Maybe String)
encryptComp (Just entry) = send (Async $ return $ Just $ hash entry)
encryptComp (Nothing) = send (Sync $ Nothing)

echoComp :: String -> Comp (Writer [String]) ()
echoComp msg = send (tell [msg])

get :: String -> Comp (U3.Union IO r1 r2) (Maybe Entry)
get = U3.injectP1 . getComp

encrypt :: Maybe Entry -> Comp (U3.Union r1 Future r2) (Maybe String)
encrypt = U3.injectP2 . encryptComp

echo :: String -> Comp (U3.Union r1 r2 (Writer [String])) ()
echo = U3.injectP3 . echoComp

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
    mentry    <- get id
    _         <- echo ("Entry is " <> (show mentry))
    encrypted <- encrypt mentry
    _         <- echo ("Encryped entry")
    _         <- echo ("Value is: " <> (show encrypted))
    return encrypted

main :: IO ()
main = let (logs, _) = runLog $ runEncrypt $ runGet $ readEncrypt "a"
       in putStrLn $ (unlines logs)