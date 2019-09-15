module Effects.Lift (Lift, lift) where

class Lift t where
    lift :: Functor f => f a -> t f a