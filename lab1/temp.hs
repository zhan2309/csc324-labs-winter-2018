getMax :: (Ord a)=> [a] -> a
getMax [] = error "hello world"
getMax [x] = x
getMax (x:xs)
        | x > mx = x
        | otherwise = mx
        where mx = getMax xs