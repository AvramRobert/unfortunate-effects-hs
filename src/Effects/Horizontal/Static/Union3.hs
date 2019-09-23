{-# LANGUAGE GADTs #-}

module Effects.Horizontal.Static.Union3 where

import Effects.Horizontal.Eff

data Union r1 r2 r3 a where
    P1 :: r1 a -> Union r1 r2 r3 a
    P2 :: r2 a -> Union r1 r2 r3 a
    P3 :: r3 a -> Union r1 r2 r3 a

injectP1 :: Comp r1 a -> Comp (Union r1 r2 r3) a
injectP1 (Value a)    = Value a
injectP1 (Effect r f) = Effect (P1 r) (\x -> injectP1 $ f x)

injectP2 :: Comp r2 a -> Comp (Union r1 r2 r3) a
injectP2 (Value a)    = Value a
injectP2 (Effect r f) = Effect (P2 r) (\x -> injectP2 $ f x)

injectP3 :: Comp r3 a -> Comp (Union r1 r2 r3) a
injectP3 (Value a)    = Value a
injectP3 (Effect r f) = Effect (P3 r) (\x -> injectP3 $ f x) 