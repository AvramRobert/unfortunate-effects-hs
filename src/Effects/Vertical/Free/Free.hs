{-# LANGUAGE GADTs, RankNTypes, TypeOperators #-}

module Effects.Vertical.Free.Free where

type ((~>)) f g = forall a . f a -> g a

data Free f a where
    Pure :: a -> Free f a
    Bind :: f (Free f a) -> Free f a

instance Functor f => Functor (Free f) where
    fmap f (Pure a)     = Pure $ f a
    fmap f (Bind ffree) = Bind $ fmap (fmap f) ffree

instance Functor f => Applicative (Free f) where
    pure = Pure
    (Pure f)     <*> (Pure a) = Pure $ f a
    (Bind ffree) <*> pure     = Bind $ fmap (<*> pure) ffree
    (Pure f)     <*> bind     = fmap f bind

instance Functor f => Monad (Free f) where
    return = pure
    (Pure a)     >>= f = f a
    (Bind ffree) >>= f = Bind $ fmap (>>= f) ffree

interpret :: (Functor f, Monad g) => (f ~> g) -> Free f a -> g a
interpret _ (Pure a)     = return a
interpret f (Bind ffree) = (f $ fmap (interpret f) ffree) >>= id

liftF :: Functor f => f a -> Free f a
liftF = Bind . fmap Pure