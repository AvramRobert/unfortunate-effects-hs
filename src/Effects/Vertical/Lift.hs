module Effects.Vertical.Lift where

class Lift t where
    lift :: Functor f => f a -> t f a