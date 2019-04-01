module Lab04 where

{- [4 marks]

   "filter pred lst" picks out the list items that satisfy the predicate. It can
   be implemented as:

   filter pred [] = []
   filter pred (x:xs) = if pred x then x : filter pred xs
                                  else filter pred xs

   E.g., filter (\x -> x > 0) [1, -2, 3] = [1, 3]

   filter is in the standard library, if you want to try it.

   Write a new version that uses foldr to replace the recursion.
-}

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter pred lst = foldr (\x ls -> if pred x then x : ls else ls) [] lst


-- It is OK if you want to use a local definiton like "where op x r = ...".


{- [4 marks]

   foldl can be expressed as foldr too! It is difficult, but it's less difficult
   if you change the parameter order to put the "accumulator" as the last
   parameter.

   E.g., newFoldl (-) [1,2,3] 10 = ((10-1)-2)-3 = 4

   To reduce name clashes, I'm renaming "op" to "f", "z" to "b".
-}

newFoldl :: (b -> a -> b) -> [a] -> b -> b
newFoldl f [] b = b
newFoldl f (x : xs) b = newFoldl f xs (f b x)

{- Hint: Think of "foo x y = ..." as "foo x = \y -> ...".
         Also "bar x y" as "(bar x) y".
-}
myFoldl :: (b -> a -> b) -> [a] -> (b -> b)
myFoldl f xs z = foldr (\i acc ->f acc i) z xs
-- myFoldl f xs z = foldl (\acc i -> f acc i) z xs

-- It is OK if you want to use a local definiton like "where op x r = ...".
