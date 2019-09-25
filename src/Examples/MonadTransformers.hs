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

type Effect a = (FutureT (WriterT [String] IO)) a

data Entry = Entry { index :: String, version :: Integer, payload :: Int } deriving (Show)

dbRef = unsafePerformIO $ newIORef [Entry { index = "a", version = 1, payload = 2 }]

hash :: Entry -> String
hash (Entry id version payload) = id <> "::" <> (show version) <> "::" <> (show payload)

getComp :: String -> IO (Maybe Entry)
getComp id = fmap (find value) $ readIORef dbRef
    where value = (== id) . index

encryptComp :: Entry -> Future String
encryptComp entry = Async $ return $ hash entry

echoComp :: String -> Writer [String] ()
echoComp msg = tell [msg]

-- You have to think idiotocally recursivelly in reverse
-- Depending on where and how you do the effect, you have to patch the sctructure in very odd ways in order to make it work

get :: String -> Effect (Maybe Entry)
get id = FutureT $ lift $ fmap pure $ getComp id

encrypt :: Maybe Entry -> Effect (Maybe String)
encrypt (Just entry) = FutureT $ pure $ fmap Just $ encryptComp entry
encrypt (Nothing)    = FutureT $ pure $ pure Nothing

echo :: String -> Effect ()
echo msg = FutureT $ WriterT $ pure $ fmap pure $ echoComp msg

run :: Effect a -> ([String], a)
run op = case (unsafePerformIO $ runWriterT $ runFutureT op) of
                (Writer logs (Async io)) -> (logs, unsafePerformIO io)
                (Writer logs (Sync a))   -> (logs, a) 

readEncrypt :: String -> Effect (Maybe String)
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