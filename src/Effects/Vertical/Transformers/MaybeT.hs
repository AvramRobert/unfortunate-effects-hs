module Effects.Vertical.Transformers.MaybeT where

import Effects.Vertical.Lift

data MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m) => (Functor (MaybeT m)) where
    fmap f (MaybeT m) = MaybeT (fmap (fmap f) m)

instance (Applicative m) => (Applicative (MaybeT m)) where
    pure = MaybeT . pure . Just

    (MaybeT mmf) <*> (MaybeT mma) = MaybeT ((pure merge) <*> mmf <*> mma)
        where merge mf ma = mf <*> ma

instance (Monad m) => (Monad (MaybeT m)) where
    return = pure

    (MaybeT ma) >>= f = MaybeT (ma >>= unwrap)
        where unwrap (Just a)  = runMaybeT $ f a
              unwrap (Nothing) = pure Nothing

instance (Lift MaybeT) where 
    lift fa = MaybeT { runMaybeT = fmap Just fa }