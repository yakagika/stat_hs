{-# LANGUAGE FlexibleInstances  #-}

module Graph where

import qualified    Text.CSV                                as CSV
import qualified    Data.Text                               as T
import              Data.Text
import              Data.Attoparsec.Text                    as DAT
-- 可視化
import              Graphics.Rendering.Chart.Easy           hiding ( (:<))
import              Graphics.Rendering.Chart.Backend.Cairo
import              Graphics.Rendering.Chart.Axis
import              Graphics.Rendering.Chart.Axis.Int
import              Graphics.Rendering.Chart.Plot.Histogram (defaultNormedPlotHist)

-- その他
import qualified    Control.Monad                           as CM
import qualified    Data.List                               as L



type Title      = String
type FileName   = String
type OutputFile = String
type InputFile  = String
type Label      = String

type X = [Double]
type Y = [Double]
type Xs = [X]
type Ys = [Y]

-- | Doubleのパーサ
{-# INLINE parseDouble #-}
parseDouble :: Text -> Double
parseDouble tx = case DAT.parseOnly DAT.double tx of
        Right r   -> r
        Left  l   -> error $ "Error on parseDouble : " ++ show l


-- |
readCSV :: InputFile -> IO [[T.Text]]
readCSV input
    =  CSV.parseCSVFromFile input >>= \res
    -> case res of
            Right xs -> return $ L.map (L.map T.pack) xs
            Left  xs -> error $ "error on parseCSVFromFile: " ++ show xs

{- | 棒グラフの作成
header1 header2 header3 ...
value1  value2  value3  ...
の形のcsvから棒グラフを作成する
-}

plotCSV2Bar :: InputFile -> OutputFile -> Title -> IO ()
plotCSV2Bar input output title
    =  readCSV input >>= \xs
    -> let header = L.map T.unpack $ L.head xs
    in let body   = L.map parseDouble $ xs !! 1
    in let values = L.zipWith (\x y -> (x,[y])) header body
    in toFile def output $ do
            layout_title .= title
            layout_title_style . font_size .= 10
            layout_x_axis . laxis_generate .= autoIndexAxis (L.map fst values)
            plot $ fmap plotBars $ bars header (addIndexes (L.map snd values))





{- | 円グラフの作成
header1 header2 header3 ...
value1  value2  value3  ...
の形のcsvから円グラフを作成する
-}

plotCSV2Pie :: InputFile -> OutputFile -> Title -> IO ()
plotCSV2Pie input output title
    =  readCSV input >>= \xs
    -> let header = L.map T.unpack $ L.head xs
    in let body   = L.map parseDouble $ xs !! 1
    in let values = L.zipWith (\x y -> (x,y,False)) header body
    in let pitem (s,v,o) = pitem_value .~ v
              $ pitem_label .~ s
              $ pitem_offset .~ (if o then 25 else 0)
              $ def
    in toFile def output $ do
            pie_title .= title
            pie_plot . pie_data .= L.map pitem values


{- | 折れ線グラフの作成
xvalue header1   header2   header3 ...
xvalue y_value1  y_value2  y_value3  ...
xvalue y_value1  y_value2  y_value3  ...
xvalue y_value1  y_value2  y_value3  ...

の形のcsvから折れ線グラフを作成する

-}
plotCSV2Line :: InputFile -> OutputFile -> Title -> IO ()
plotCSV2Line input output title
    = readCSV input >>= \xs
    -> let header = L.map T.unpack $ L.head xs
    in let body   = L.map (L.map parseDouble ) $ L.tail $ L.transpose $ L.tail xs
    in let index  = L.map parseDouble $ L.head $ L.transpose $ L.tail xs
    in let values = L.zipWith (\x y -> (x, L.zip index y)) header body
    in toFile def output $ do
        layout_title .= title
        CM.forM_ values $ \(label,ys) -> plot $ line label [ys]

{- | 折れ線グラフの作成
X軸,Y軸のリストを与えると折れ線グラフを作成する
-}
plotLine :: X -> Y -> OutputFile -> Title -> IO ()
plotLine xs ys output title
    = toFile def output $ do
        layout_title .= title
        plot $ line title [L.zip xs ys]

{- |
Header
Category
Category
Category
Category
Category
Category

の形のCSVからヒストグラムを作成する

-}

plotCSV2Hist :: HistKind -> InputFile -> OutputFile -> Title -> IO ()
plotCSV2Hist kind input output title
    = case kind of
        Qualitative     -> readCSV input >>= \xs
                        -> let body   = L.map T.unpack $ L.head $ L.transpose $ L.tail xs
                        in let count  = L.map (\(z:zs) -> (z, L.length (z:zs))) $ L.group $ L.sort body
                        in let header = L.map fst count
                        in let values = L.zipWith (\x y -> (x,[y])) header (L.map snd count)
                        in toFile def output $ do
                                layout_title .= title
                                layout_title_style . font_size .= 10
                                plot $ fmap plotBars $ liftEC $ do
                                    plot_bars_spacing .= BarsFixGap 0 0
                                    plot_bars_titles  .= header
                                    plot_bars_values .= addIndexes (L.map snd values)

        Quantitative    -> readCSV input >>= \xs
                        -> let header = T.unpack $ L.head $ L.head xs
                        in let values = L.map parseDouble $ L.head $ L.transpose $ L.tail xs
                        in toFile def output $ do
                                layout_title .= title
                                layout_title_style . font_size .= 10
                                plot $ fmap histToPlot $ liftEC $ do
                                    plot_hist_bins .= sturgesNumber (L.length values)
                                    plot_hist_values .= values :: EC (PlotHist Double Double) ()


    where
    sturgesNumber :: Int -> Int
    sturgesNumber n = truncate
                $ 1 + logBase 2 (fromIntegral n)


data HistKind = Qualitative | Quantitative deriving (Show, Eq)


instance Default (PlotHist x Double) where
    def = defaultNormedPlotHist


{- |
Header Header
Value  Value
Value  Value
Value  Value

の形のcsvデータを読み込んで,散布図を作成します.
-}

plotCSV2Scatter :: InputFile -> OutputFile -> Title -> IO ()
plotCSV2Scatter input output title
    =  readCSV input >>= \xs
    -> let body    = L.transpose $ L.tail xs
    in let x_value = L.map parseDouble (body !! 0)
    in let y_value = L.map parseDouble (body !! 1)
    in let values  = L.zip y_value x_value
    in toFile def output $ do
            layout_title .= title
            layout_title_style . font_size .= 10
            plot $ fmap toPlot $ liftEC $ do
                plot_points_style  .= filledCircles 2 (opaque red)
                plot_points_values .= values

















