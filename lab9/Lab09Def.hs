module Lab09Def where

newtype State s a = StateOf (s -> (s,a))

deState :: State s a -> s -> (s, a)
deState (StateOf stf) = stf

-- Analogous to C's "i++".  Increment the state variable, and return what's
-- before the increment.
postInc :: State Int Int
postInc = StateOf (\i -> (i+1, i))

-- Run a State program with an initial state.  Get keep its return value (final
-- state discarded).
run :: State s a -> s -> a
run prog s0 = snd (deState prog s0)

-- Before writing the Monad instance, here is an example program to show what
-- can happen.

-- Using >>= directly:
albertProg1 :: State Int (Int, Int)
albertProg1 =
    postInc >>= (\i -> postInc >>= (\j -> return (i, j)))
    -- Most parentheses here can be omitted.

-- Using do-notation:
albertProg2 :: State Int (Int, Int)
albertProg2 = do
    i <- postInc
    j <- postInc
    return (i, j)

-- Experiment and observe:
--    run albertProg1 42 = (42, 43)
--    run albertProg2 8 = (8, 9)
--    etc
-- Get the feeling that postInc increments an internal counter and returns the
-- counter value before the increment.
-- Initial counter value is from run's 2nd parameter.


instance Monad (State s) where
    return a = pure a
    StateOf t >>= k = StateOf (\i0 -> case t i0 of
                                  (i1, a) -> deState (k a) i1)

instance Functor (State s) where
    fmap f (StateOf t) = StateOf (\i0 -> case t i0 of (i1, a) -> (i1, f a))

instance Applicative (State s) where
    pure a = StateOf (\i -> (i, a))
    StateOf tf <*> StateOf ta = StateOf
        (\i0 -> case tf i0 of (i1, f) -> case ta i1 of (i2, a) -> (i2, f a))


data BT v = Null | Node (BT v) v (BT v) deriving Eq

-- This notation goes like: (left-subtree element right-subtree).
-- Example: (((0) 1 (2)) 3 (4 (5)))
instance Show v => Show (BT v) where
    showsPrec _ t = flatsParen t
      where
        flats Null = id
        flats (Node left v right) =
            (case left of Null -> id; t -> flatsParen t . showChar ' ')
            . shows v
            . (case right of Null -> id; t -> showChar ' ' . flatsParen t)
        flatsParen t = showChar '(' . flats t . showChar ')'
