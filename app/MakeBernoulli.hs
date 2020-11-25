{-# LANGUAGE Strict, StrictData,FlexibleInstances #-}

module Main where

-- 乱数
import System.Random

-- 可視化
import              Graphics.Rendering.Chart.Easy            hiding ( (:<))
import              Graphics.Rendering.Chart.Backend.Cairo
import              Graphics.Rendering.Chart.Plot.Histogram (defaultNormedPlotHist)

-- その他
import qualified    Control.Monad                           as CM
import qualified    Data.List                               as L
import qualified    Data.Vector                             as V
import              Data.Tuple

-- | ベルヌーイ試行をn回行った結果
-- 表が出た回数のリストを返す
bernoulli_trial :: Int -> Int -> IO (V.Vector Double)
bernoulli_trial ex tr = V.replicateM tr
                      $ V.replicateM ex (randomRIO (0,1) :: IO Double) >>= \xs
                      -> return $ fromIntegral
                                $ V.length
                                $ V.filter (<= 0.5) xs


-- | データの分散
--  表が出る回数が平均値からどの程度乖離しているか
variance :: V.Vector Double -> Double
variance xs =  let len = fromIntegral (V.length xs)
            in let ave = (V.sum xs) / len
            in (*) (1/len)  $ V.sum
                            $ (flip V.map) xs
                            $ \x -> (x - ave)**2

-- | 二項分布の分散
-- 表が出る回数の平均値からどの程度乖離しているか
v :: Int -> Double -> Double
v n p = (fromIntegral n) * p * (1 - p)


main = do
    -- 実験回数を取得
    putStrLn "何回コインを投げるかを入力して下さい."
    num_of_experiment <- read <$> getLine :: IO Int
    -- 試行回数を取得
    putStrLn "何回ベルヌーイ実験を繰り返すかを入力して下さい."
    num_of_trial      <- read <$> getLine :: IO Int

    xs <- bernoulli_trial num_of_experiment num_of_trial

    putStrLn $ "確率分布の標準偏差: " ++ show ((v num_of_experiment 0.5) ** (1/2))
    putStrLn $ "実測値の標準偏差: "   ++ show ((variance xs) ** (1/2))

    -- 実験結果の表示
    let values = V.toList xs
    toFile def "Data/BernoulliCount.png" $ do
        layout_title .= "Distribution of the number of front side appears"
        layout_title_style . font_size .= 10
        layout_x_axis . laxis_title    .= "Number of fornt side appears"
        layout_y_axis . laxis_title    .= "Probability"
        plot $ fmap histToPlot $ liftEC $ do
            plot_hist_values .= values :: EC (PlotHist Double Double) ()

    -- 確率分布の表示
    let probabilities = [(x / fromIntegral num_of_trial) | x <- values]
    toFile def "Data/BernoulliProbability.png" $ do
        layout_title .= "Distribution of the number of front side appears"
        layout_title_style . font_size .= 10
        layout_x_axis . laxis_title    .= "Number of fornt side appears"
        layout_y_axis . laxis_title    .= "Probability"
        plot $ fmap histToPlot $ liftEC $ do
            plot_hist_values .= probabilities :: EC (PlotHist Double Double) ()


data HistKind = Qualitative | Quantitative deriving (Show, Eq)
instance Default (PlotHist x Double) where
    def = defaultNormedPlotHist
