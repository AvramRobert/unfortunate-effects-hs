module Effects.Vertical.Transformers.FutureT where

import System.IO.Unsafe (unsafePerformIO)
import Effects.Future (Future, Future(Sync, Async))

data FutureT m a = FutureT { runFutureT :: m (Future a) }

instance (Functor m) => Functor (FutureT m) where
    fmap f (FutureT m) = FutureT (fmap (fmap f) m)

instance (Applicative m) => Applicative (FutureT m) where
    pure = FutureT . pure . Sync

    (FutureT mff) <*> (FutureT mfa) = FutureT $ (pure merge) <*> mff <*> mfa
        where merge ff fa = ff <*> fa

instance (Monad m) => Monad (FutureT m) where
    return = pure

    (FutureT mfa) >>= f = FutureT (mfa >>= unwrap)
        where unwrap (Async io) = runFutureT $ f $ (unsafePerformIO io)
              unwrap (Sync a)   = runFutureT $ f a
