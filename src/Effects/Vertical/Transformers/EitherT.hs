module Effects.Vertical.Transformers.EitherT where

data EitherT m e a = EitherT { runEitherT :: m (Either e a) }

instance (Functor m) => Functor (EitherT m e) where
    fmap f (EitherT me) = EitherT (fmap (fmap f) me)

instance (Applicative m) => Applicative (EitherT m e) where
    pure = EitherT . pure . Right

    (EitherT mef) <*> (EitherT mea) = EitherT ((pure merge) <*> mef <*> mea)
        where merge ef ea = ef <*> ea

instance (Monad m) => Monad (EitherT m e) where
    return = pure
    (EitherT mea) >>= f = EitherT (mea >>= unwrap)
        where unwrap (Right a) = runEitherT $ f a
              unwrap (Left e)  = pure $ Left e