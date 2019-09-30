{-# LANGUAGE TypeOperators, DataKinds, MonoLocalBinds #-}

module Effects.Horizontal.Extensible.Union (Union, Member, inject, terminate, decompose) where

import Effects.Horizontal.Eff
import Data.Open.Union

decompose :: Union (r : l) a -> Either (Union l a) (r a)
decompose = decomp

inject :: (Member r l) => Comp r a -> Comp (Union l) a
inject (Value a)    = Value a
inject (Effect r f) = Effect (inj r) (inject . f)

terminate :: Comp (Union '[]) a -> a
terminate (Value a) = a