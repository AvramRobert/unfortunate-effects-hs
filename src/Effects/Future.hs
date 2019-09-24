module Effects.Future where

import Data.Monoid ((<>))

data Future a = Async (IO a) | Sync a

instance Show a => Show (Future a) where
    show (Async io) = "Future ( <not computed> )"
    show (Sync a)   = "Future (" <> (show a) <> ")"

instance Functor Future where
    fmap f (Async io) = Async (fmap f io)
    fmap f (Sync a)   = Sync $ f a

instance Applicative Future where
    pure = Sync
    
    (Sync f) <*> (Async a) = (Async (pure f)) <*> (Async a)
    (Async f) <*> (Sync a) = (Async f) <*> (Async (pure a))  
    (Async iof) <*> (Async ioa) = Async (do 
                            f <- iof
                            a <- ioa
                            return (f a))
    
instance Monad Future where
    (Sync a) >>= f = f a
    (Async io) >>= f = Async $ (fmap f io) >>= unwrap
        where unwrap (Async io) = io
              unwrap (Sync a)   = pure a