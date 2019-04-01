module Lab06 where

import Lab06Def

-- Implement the following methods for Forky. See the PDF for what to aim for.

instance Functor Forky where
    -- fmap :: (a -> b) -> Forky a -> Forky b
    fmap f (Tip (a)) = Tip (f a)
    fmap f (Branch (b) (c)) = Branch (fmap f (b)) (fmap f (c))

instance Applicative Forky where
    -- pure :: a -> Forky a
    pure a = Tip a
    -- (<*>) :: Forky (a -> b) -> Forky a -> Forky b
    (Tip f) <*> Tip a = Tip (f a)
    (Tip f) <*> Branch (a) (b) = Branch ((Tip f) <*> (a)) ((Tip f) <*> (b))
    Branch (Tip fuc1) (Tip fuc2) <*> b = Branch ((Tip fuc1) <*> (b)) ( (Tip fuc2) <*> (b))
    