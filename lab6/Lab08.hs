module Lab08 where

import Lab08Def

instance Functor Forky where
    fmap f ta = do
        a <- ta
        pure (f a)

instance Applicative Forky where
    pure a = Tip a
    tf <*> ta = do
        f <- tf
        a <- ta
        pure (f a)

-- Implement Monad's (>>=) for Forky. See the PDF for what to aim for.

instance Monad Forky where
    return a = Tip a

    -- (>>=) :: Forky a -> (a -> Forky b) -> Forky b
    Tip leaf >>= f = f leaf
    Branch left right >>= f = Branch(left >>= f)(right >>= f)