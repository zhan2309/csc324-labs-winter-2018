module Lab05 where

import Lab05Def

{- [6 marks]

   Help me make Weight an instance of Num so I can do basic arithmetic.
   No need to do (-), default implementation uses (+) and negate.
-}
instance Num Weight where
    -- (+) :: Weight -> Weight -> Weight
    -- Corner case: anything + Inf = Inf.  The other order too.
    (+) Inf _ = Inf
    (+) _ Inf = Inf
    (+) (Fin weight_a) (Fin weight_b) = Fin (weight_a+weight_b)
    -- negate :: Weight -> Weight
    -- Corner case done because unsupported:
    negate Inf = error "negative infinity not supported"
    negate (Fin weight_a) = Fin (-weight_a)
    -- (*) :: Weight -> Weight -> Weight
    -- Corner cases:
    -- non-negative anything * Inf = Inf
    -- negative anything * Inf is unsupported
    -- Don't forget the other order.
    (*) (Fin weight_a) Inf
        | weight_a >= 0 = Inf
        | otherwise = error "negative infinity not supported"
    (*) Inf (Fin weight_b)
        | weight_b >= 0 = Inf
        | otherwise = error "negative infinity not supported"
    (*) (Fin weight_a) (Fin weight_b) = Fin (weight_a*weight_b)
    (*) _ _ = Inf
    -- abs :: Weight -> Weight
    abs (Fin weight_a)
        | weight_a < 0 = Fin (-weight_a)
        | otherwise = (Fin weight_a)
    abs _ = Inf
    -- signum :: Weight -> Weight
    -- Corner case: signum Inf is positive one. But mind how to code it.
    signum (Fin weight_a)
        | weight_a > 0 = Fin (1)
        | weight_a == 0 = Fin (0)
        | otherwise = Fin (-1)
    signum _ = Fin (1)
    -- fromInteger :: Integer -> Weight
    fromInteger num = Fin (num)