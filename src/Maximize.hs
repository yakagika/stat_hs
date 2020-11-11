
module Maximize where



max2 :: (Ord a) => [a] -> a
max2 xs = foldl1 (\x y -> if x >= y then x else y) xs

max3 :: (Ord a) => [a] -> [a]
max3 []         = []
max3 [x]        = [x]
max3 (x:xs)     | x >= head (max3 xs)  = [x]
                | otherwise            = max3 xs


max4 :: (Ord a) => [a] -> [a]
max4 xs = [x|x <- xs, and [x >= y | y <- xs]]
