module Lab09 where

import Lab09Def

{-
Lab09Def.hs defines the State monad and a binary tree type.  Please take a look.

You job: Working with the State monad, implement numberHelper below so that it
uses postInc and some of Functor, Applicatvie, Monad operations (do-notation
probably best) to convert a binary tree to a new binary tree that has the same
shape, but the elements are Int's, and the numbers go in the order 0, 1,
2,... in the tree's in-order.  There is an example below.
-}

number :: BT v -> BT Int
number t = run (numberHelper t) 0

numberHelper :: BT v -> State Int (BT Int)
numberHelper Null = do
    -- base case
    return (Null)
numberHelper (Node left _ right) = do
    -- left tree
    left_tree <- numberHelper(left)
    inner <- postInc
    -- right tree
    right_tree <- numberHelper(right)
    return (Node left_tree inner right_tree)
-- Try to take advantage of postInc and recursion.

-- Example: number albertTree = albertTreeNumbered, where:

albertTree :: BT String
albertTree = Node (Node (Node Null "ll" Null)
                        "l"
                        (Node Null "lr" Null))
                  "root"
                  (Node Null
                        "r"
                        (Node Null "rr" Null))

albertTreeNumbered :: BT Int
albertTreeNumbered = Node (Node (Node Null 0 Null)
                                1
                                (Node Null 2 Null))
                          3
                          (Node Null
                                4
                                (Node Null 5 Null))


-- Not required, but good exercise: Don't use Monad operations.  I.e., it can be
-- done with just postInc, Functor, and Applicative.

-- Not required, but good exercise: Think up a problem similar to this, but with
-- a twist, and the twist forces you to use Monad operations.  And solve it.
