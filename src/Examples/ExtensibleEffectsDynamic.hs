{-# LANGUAGE TypeOperators, DataKinds, FlexibleContexts, MonoLocalBinds, GADTs #-}

module Examples.ExtensibleEffectsDynamic where

import Effects.Horizontal.Dynamic.Union
import Effects.Horizontal.Eff
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

get :: Member IO l => String -> Comp (Union l) (Maybe Entry)
get = inject . getComp

encrypt :: Member Future l => Maybe Entry -> Comp (Union l) (Maybe String)
encrypt = inject . encryptComp

echo :: Member (Writer [String]) l => String -> Comp (Union l) ()
echo = inject . echoComp

runGet :: Comp (Union (IO : l)) a -> Comp (Union l) a
runGet (Value a)    = Value a
runGet (Effect r f) = case (decompose r) of 
    (Left union) -> Effect union (\x -> runGet $ f x)
    (Right io)   -> runGet $ f $ unsafePerformIO io

runEncrypt :: Comp (Union (Future : l)) a -> Comp (Union l) a
runEncrypt (Value a)    = Value a
runEncrypt (Effect r f) = case (decompose r) of
    (Left union)       -> Effect union (\x -> runEncrypt $ f x)
    (Right (Async io)) -> runEncrypt $ f $ unsafePerformIO io
    (Right (Sync a))   -> runEncrypt $ f a

runLog :: Comp (Union ((Writer [String]) : l)) a -> Comp (Union l) ([String], a)
runLog (Value a)    = Value ([], a)
runLog (Effect r f) = case (decompose r) of
    (Left union)   -> Effect union (\x -> runLog $ f x)
    (Right (Writer logs a)) -> fmap insertLogs $ runLog (f a)
        where insertLogs (rest, res) = (logs <> rest, res)


-- The problem with this is that i sort of have to let it infer everything that I do
-- If I separate these things into separate functions, I have to prove, for each, that the kind i'm trying to add to the union can be a member
-- So that the union can add it to itself and infer a new list.  
readEncrypt :: String -> Comp (Union (IO : Future : (Writer [String]) : l)) (Maybe String)
readEncrypt id = do
    mentry    <- get id
    _         <- echo ("Entry is " <> (show mentry))
    encrypted <- encrypt mentry
    _         <- echo ("Encryped entry")
    _         <- echo ("Value is: " <> (show encrypted))
    return encrypted

main :: IO ()
main = let (logs, _) = terminate $ runLog $ runEncrypt $ runGet $ readEncrypt "a"
       in putStrLn $ unlines logs