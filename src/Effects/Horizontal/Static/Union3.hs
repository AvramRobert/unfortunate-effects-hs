{-# LANGUAGE GADTs #-}

module Effects.Horizontal.Static.Union3 where

import qualified Effects.Horizontal.Static.Union2 as U2
import Effects.Horizontal.Eff

data Union r1 r2 r3 a where
    P1 :: r1 a -> Union r1 r2 r3 a
    P2 :: r2 a -> Union r1 r2 r3 a
    P3 :: r3 a -> Union r1 r2 r3 a

injectP1 :: Comp r1 a -> Comp (Union r1 r2 r3) a
injectP1 (Value a)    = Value a
injectP1 (Effect r f) = Effect (P1 r) (injectP1 . f)

injectP2 :: Comp r2 a -> Comp (Union r1 r2 r3) a
injectP2 (Value a)    = Value a
injectP2 (Effect r f) = Effect (P2 r) (injectP2 . f)

injectP3 :: Comp r3 a -> Comp (Union r1 r2 r3) a
injectP3 (Value a)    = Value a
injectP3 (Effect r f) = Effect (P3 r) (injectP3 . f)

removeP1 :: Comp (Union r1 r2 r3) a -> Comp (U2.Union r2 r3) a
removeP1 (Value a)         = Value a
removeP1 (Effect (P2 r) f) = Effect (U2.P1 r) (removeP1 . f)
removeP1 (Effect (P3 r) f) = Effect (U2.P2 r) (removeP1 . f)

removeP2 :: Comp (Union r1 r2 r3) a -> Comp (U2.Union r1 r3) a
removeP2 (Value a)         = Value a
removeP2 (Effect (P1 r) f) = Effect (U2.P1 r) (removeP2 . f)
removeP2 (Effect (P3 r) f) = Effect (U2.P2 r) (removeP2 . f)

removeP3 :: Comp (Union r1 r2 r3) a -> Comp (U2.Union r1 r2) a
removeP3 (Value a)         = Value a
removeP3 (Effect (P1 r) f) = Effect (U2.P1 r) (removeP3 . f)
removeP3 (Effect (P2 r) f) = Effect (U2.P2 r) (removeP3 . f)

