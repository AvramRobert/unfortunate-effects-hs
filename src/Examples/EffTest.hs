{-# LANGUAGE GADTs #-}

module Examples.EffTest where

import Effects.Horizontal.Eff
import Effects.Horizontal.Static.Union2

data Get v a where
    Get :: Get a a

data Put v a where
    Put :: v -> Put v ()

put :: a -> Comp (Put a) ()
put = send . Put

get :: Comp (Get a) a
get = send Get

readInt :: Comp (Get Int) Int
readInt = get

runLeft :: e -> Comp (Union (Get e) r2) a -> Comp r2 a
runLeft e (Value a)           = Value a
runLeft e (Effect (P1 Get) f) = runLeft e $ f e
runLeft e (Effect (P2 r) f)   = Effect r (\x -> runLeft e $ f x)

runPut :: Comp (Put a) x -> ([a], x)
runPut (Value a)          = ([], a)
runPut (Effect (Put a) f) = let (as, x) = runPut (f ()) 
                            in (a : as, x)

expr1 = do
    a <- readInt
    b <- readInt
    return (a + b)

expr2 = do
    x <- expr1
    y <- readInt
    return ((x * y) - 1)

update = do
    x <- injectP1 $ expr2  
    y <- injectP1 $ expr1
    _ <- injectP2 $ (put x)
    _ <- injectP2 $ (put y)
    return ()

runGet :: v -> Get v b -> b
runGet r Get = r

runPut' :: Put v r -> ([v], r)
runPut' (Put a) = (a : [], ())

compute n = runPut $ runLeft 2 $ update