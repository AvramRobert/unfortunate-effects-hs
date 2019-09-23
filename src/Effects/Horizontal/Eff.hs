{-# LANGUAGE GADTs, RankNTypes #-}

module Effects.Horizontal.Eff where

data Comp r a where
    Value :: a -> Comp r a
    Effect :: r x -> (x -> Comp r a) -> Comp r a

instance Functor (Comp r) where
    fmap f (Value a)    = Value (f a)
    fmap f (Effect r g) = Effect r (\x -> fmap f (g x))

instance Applicative (Comp r) where
    pure a = Value a
    
    (Value f) <*> (Value a)         = Value (f a)
    (Effect r1 f) <*> (Effect r2 g) = Effect r1 (\x -> (f x) <*> (Effect r2 (\y -> (g y))))
    (Value f) <*> (Effect r g)      = Effect r (\x -> (Value f) <*> (g x))
    (Effect r f) <*> (Value g)      = Effect r (\x -> (f x) <*> (Value g))

instance Monad (Comp r) where
    return a = pure a
    (Value a) >>= f    = f a
    (Effect r f) >>= g = Effect r (\x -> (f x) >>= g)

send :: r a -> Comp r a
send r = Effect r Value