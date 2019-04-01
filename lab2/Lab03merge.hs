module Lab03merge where

{- This is one of two questions. The other question is in another file. -}

{-
[8 marks]
Take two lists of integers.  Precondition: Each is already sorted (non-decreasing).
Perform the "merge" of "mergesort".  Linear time.

Example:
merge [2, 3, 5] [1, 3, 4] = [1, 2, 3, 3, 4, 5]
-}
merge :: [Integer] -> [Integer] -> [Integer]
-- merge = error "TODO"
merge x y
    | (x==[]&&y==[]) = []
    | x==[] = y
    | y==[] = x
merge (x:xs) (y:ys)
    | x > y = y : merge ys (x:xs)
    | x <= y = x : merge xs (y:ys)




