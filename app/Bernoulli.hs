{-# LANGUAGE Strict, StrictData#-}

module Main where

import Data.Random
import qualified Data.Random.Distribution.Bernoulli as Be

-- 可視化
import              Graphics.Rendering.Chart.Easy            hiding ( (:<))
import              Graphics.Rendering.Chart.Backend.Cairo

-- その他
import qualified    Control.Monad                           as CM
import qualified    Data.List                               as L

bers :: RVar Double
bers = do
    x <- Be.bernoulli (0.525 :: Double)
    return x


main = do
    -- 実験回数を入力します
    putStrLn "何回実験を行うかを半角英数字で入力しましょう"
    num <- read <$> getLine  :: IO Int

    -- 実験結果
    res <- L.map (\x -> if x == 0 then True else False)
        <$> CM.replicateM num (sample bers)

    -- 確率の経過の計算
    let (rate,xs) = culc res [] 0 0

    -- 結果の表示
    toFile def ("bernoulli_" ++ show num ++ ".png") $ do
        layout_title .= "The probability of getting a front."
        plot $ line "rate" [L.zip [0..num] xs]

    putStrLn $ "表が出る確率は" ++ show rate


culc :: [Bool] -> [Double] -> Double -> Double -> (Double, [Double])
culc []  ys front back = let r = front / (front + back)  in (r, ys)
culc [x] ys front back = case x of
                            True    -> let r = (front + 1) / (front + 1 + back)
                                    in culc [] (ys ++ [r]) (front + 1) back
                            False   -> let r = front       / (front + 1 + back)
                                    in culc [] (ys ++ [r]) front (back + 1)

culc (x:xs) ys front back = case x of
                            True    -> let r = (front + 1) / (front + 1 + back)
                                    in culc xs (ys ++ [r]) (front + 1) back
                            False   -> let r = front       / (front + 1 + back)
                                    in culc xs (ys ++ [r]) front (back + 1)
