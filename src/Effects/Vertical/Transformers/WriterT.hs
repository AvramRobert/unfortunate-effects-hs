module Effects.Vertical.Transformers.WriterT where

import Effects.Writer
import Effects.Vertical.Lift

data WriterT w f a = WriterT { runWriter :: f (Writer w a) }

instance Functor f => Functor (WriterT w f) where
    fmap f (WriterT fa) = WriterT $ fmap applyF fa
        where applyF (Writer w a) = Writer w (f a)
        
instance (Applicative f, Monoid w) => Applicative (WriterT w f) where
    pure a = WriterT (pure $ pure a)

    (WriterT ff) <*> (WriterT fa) = WriterT ((pure applyF) <*> ff <*> fa)
        where applyF (Writer w f) (Writer w' a) = Writer (w <> w') (f a)

instance (Monad f, Monoid w) => Monad (WriterT w f) where
    return = pure

    (WriterT fa) >>= f = WriterT (fa >>= applyF)
        where applyF (Writer w a)   = case (f a) of (WriterT fb) -> fmap (merge w) $ fb
              merge w (Writer w' b) = Writer (w <> w') b

instance Monoid w => (Lift (WriterT w)) where
    lift fa = WriterT { runWriter = fmap (Writer mempty) fa }