module Effects.Writer where

data Writer w a = Writer w a

tell :: w -> Writer w ()
tell w = Writer w ()

instance Functor (Writer w) where
    fmap f (Writer w a) = Writer w (f a)

instance Monoid w => Applicative (Writer w) where
    pure a = Writer mempty a
    (Writer as f) <*> (Writer bs a) = Writer (as <> bs) (f a)

instance Monoid w => Monad (Writer w) where
    return = pure
    (Writer xs a) >>= f = case (f a) of (Writer ys b) -> Writer (xs <> ys) b 