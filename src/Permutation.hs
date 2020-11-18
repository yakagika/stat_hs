module Permutation where

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * (factorial (n - 1))

permute :: Integer -> Integer -> Integer
permute n r = div (factorial n) (factorial (n-r))

combinate :: Integer -> Integer -> Integer
combinate n r   =     (permute n r)
                `div` ----------------------
                      (factorial r)