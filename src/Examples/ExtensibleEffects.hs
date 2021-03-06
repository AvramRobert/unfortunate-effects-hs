{-# LANGUAGE TypeOperators, DataKinds, FlexibleContexts, MonoLocalBinds #-}

module Examples.ExtensibleEffects where

import Effects.Horizontal.Extensible.Union
import Effects.Horizontal.Eff
import Effects.Future
import Effects.Writer
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef (newIORef, readIORef)
import Data.List (find)
import Data.Monoid ((<>))

data Entry = Entry { index :: String, version :: Integer, payload :: Int } deriving (Show)

dbRef = unsafePerformIO $ newIORef [Entry { index = "a", version = 1, payload = 2 }]

fromDB :: String -> IO (Maybe Entry)
fromDB id = fmap (find value) $ readIORef dbRef
    where value = (== id) . index 

hash :: Entry -> String
hash (Entry id version payload) = id <> "::" <> (show version) <> "::" <> (show payload)

getComp :: String -> Comp IO (Maybe Entry)
getComp id = perform (fromDB id)

encryptComp :: Maybe Entry -> Comp Future (Maybe String)
encryptComp (Just entry) = perform (Async $ return $ Just $ hash entry)
encryptComp (Nothing)    = perform (Sync $ Nothing)

echoComp :: String -> Comp (Writer [String]) ()
echoComp msg = perform (tell [msg])

get :: Member IO l => String -> Comp (Union l) (Maybe Entry)
get = inject . getComp

encrypt :: Member Future l => Maybe Entry -> Comp (Union l) (Maybe String)
encrypt = inject . encryptComp

echo :: Member (Writer [String]) l => String -> Comp (Union l) ()
echo = inject . echoComp

runGet :: Comp (Union (IO : l)) a -> Comp (Union l) a
runGet (Value a)    = Value a
runGet (Effect r f) = reRun $ decompose r 
    where reRun (Left union) = Effect union (runGet . f)
          reRun (Right io)   = runGet $ f $ unsafePerformIO io

runEncrypt :: Comp (Union (Future : l)) a -> Comp (Union l) a
runEncrypt (Value a)    = Value a
runEncrypt (Effect r f) = reRun $ decompose r 
    where reRun (Left union)       = Effect union (runEncrypt . f)
          reRun (Right (Async io)) = runEncrypt $ f $ unsafePerformIO io
          reRun (Right (Sync a))   = runEncrypt $ f a

runEcho :: Comp (Union ((Writer [String]) : l)) a -> Comp (Union l) ([String], a)
runEcho (Value a)    = Value ([], a)
runEcho (Effect r f) = reRun $ decompose r
    where reRun (Left union)            = Effect union (runEcho . f)
          reRun (Right (Writer logs a)) = fmap (merge logs) $ runEcho (f a)
          merge logs (rest, a)          = (logs <> rest, a)

readEncrypt :: String -> Comp (Union (IO : Future : (Writer [String]) : l)) (Maybe String)
readEncrypt id = do
    mentry    <- get id
    _         <- echo ("Entry is " <> (show mentry))
    encrypted <- encrypt mentry
    _         <- echo ("Encryped entry")
    _         <- echo ("Value is: " <> (show encrypted))
    return encrypted

main :: IO ()
main = let (logs, _) = terminate $ runEcho $ runEncrypt $ runGet $ readEncrypt "a"
       in putStrLn $ unlines logs