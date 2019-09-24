module Examples.MonadTransformers where

import Effects.Vertical.Lift
import Effects.Vertical.Transformers.MaybeT
import Effects.Vertical.Transformers.FutureT
import Effects.Vertical.Transformers.WriterT
import Effects.Writer
import Effects.Future (Future (Async, Sync))
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef (newIORef, readIORef, modifyIORef)
import Data.List (find, deleteBy)

newtype Error = Error String deriving (Show)

type DBOp a = MaybeT (FutureT (WriterT [String] IO)) a

data Entry = Entry { index :: String, version :: Integer, payload :: Int } deriving (Show)

dbRef = unsafePerformIO $ newIORef [Entry { index = "a", version = 1, payload = 2 }]

io :: a -> IO a
io = return

hash :: Entry -> String
hash (Entry id version payload) = id <> "::" <> (show version) <> "::" <> (show payload)

getComp :: String -> IO (Maybe Entry)
getComp id = do
        db <- readIORef dbRef
        return (find value db)
    where value = (== id) . index 

encryptComp :: Entry -> Future String
encryptComp entry = Async $ return $ hash entry

echoComp :: String -> Writer [String] ()
echoComp msg = tell [msg]

-- You have to think idiotocally recursivelly in reverse

get :: String -> DBOp Entry
get id = MaybeT $ (lift $ lift $ getComp id)

encrypt :: Entry -> DBOp String
encrypt entry = MaybeT $ FutureT $ pure $ runMaybeT $ lift $ encryptComp entry

echo :: String -> DBOp ()
echo msg = MaybeT $ FutureT $ WriterT $ pure $ runFutureT $ runMaybeT $ lift $ lift $ echoComp msg

run :: DBOp a -> ([String], Maybe a)
run op = case (unsafePerformIO $ runWriterT $ runFutureT $ runMaybeT op) of
                (Writer logs (Async io)) -> case (unsafePerformIO io) of
                                                        (Just a)  -> (logs, Just a)
                                                        (Nothing) -> (logs, Nothing)
                (Writer logs (Sync a))   -> (logs, a) 

readEncrypt :: String -> DBOp String
readEncrypt id = do
    mentry    <- get id
    _         <- echo ("Entry is " <> (show mentry))
    encrypted <- encrypt mentry
    _         <- echo ("Encryped entry")
    _         <- echo ("Value is: " <> (show encrypted))
    return encrypted

main :: IO ()
main = let (logs, _) = run $ readEncrypt "a"
       in putStrLn $ unlines logs