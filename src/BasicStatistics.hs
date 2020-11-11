module BasicStatistics where


import qualified Data.List as L

-- | 算術平均
average :: [Double] -> Maybe Double
average []  = Nothing
average [x] = Just x
average xs  = Just $ (sum xs) / (fromIntegral (length xs))

-- | 幾何平均
geometric :: [Double] -> Maybe Double
geometric []  = Nothing
geometric [x] = Just x
geometric xs  = Just $ (foldl1 (*) xs) ** (1 / fromIntegral (length xs))

-- | 調和平均
harmonic :: [Double] -> Maybe Double
harmonic []  = Nothing
harmonic [x] = Just x
harmonic xs  = Just $ (fromIntegral (length xs))
                    / (L.sum (map (\x -> 1 / x) xs))

-- | 中央値
median :: [Double] -> Maybe Double
median []  = Nothing
median [x] = Just x
median xs  | mod len 2 /= 0 = Just $ (!!) sorted
                            $ (-) ((len + 1) `div` 2) 1
           | otherwise      = let (x,y) = L.splitAt (len `div` 2) sorted
                            in Just (((L.last x) +  (L.head y)) / 2)
           where
            sorted = L.sort xs
            len    = length xs


-- | 円柱の面積を求める
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h  -- 側面の面積
        topArea  = pi * r ^2       -- 上下の円の面積
    in  sideArea + 2 * topArea

{- 上と同じ
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h = sideArea + 2 * topArea
    where
    -- 側面の面積
    sideArea = 2 * pi * r * h
    -- 上下の円の面積
    topArea  = pi * r ^2
-}


-- | エラーが帰る場合にNothingを返すhead
maybeHead []     = Nothing
maybeHead [x]    = Just x
maybeHead (x:xs) = Just x


-- | 階乗
-- fact(n) = | 1,               if n = 0
--           | n * fact(n-1),   if n > 0
fact :: Int  -> Int
fact n | n == 0  = 1
       | n >  0  = n * (fact (n - 1))

{- これはコードではなく数式のイメージ
fact 5  = 5 * fact 4
        = 5 * 4 * fact 3
        = 5 * 4 * 3 * fact 2
        = 5 * 4 * 3 * 2 fact 1
        = 5 * 4 * 3 * 2 * fact 0
        = 5 * 4 * 3 * 2 * 1
-}


add :: Int -> Int -> Int
add x y = x + y






