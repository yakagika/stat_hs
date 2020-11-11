
module Main (main) where

-- package の import
-- CSVの読み込み
import qualified    Text.CSV                                as CSV
import qualified    Data.Text                               as T
import              Data.Text
import              Data.Attoparsec.Text                    as DAT

-- 可視化
import              Graphics.Rendering.Chart.Easy            hiding ( (:<))
import              Graphics.Rendering.Chart.Backend.Cairo
import              Graphics.Rendering.Chart.Axis
import              Graphics.Rendering.Chart.Axis.Int
import              Graphics.Rendering.Chart.Grid

-- 重回帰
import Statistics.Regression

-- その他
import qualified    Control.Monad                           as CM
import qualified    Data.List                               as L
import qualified    Data.Vector.Unboxed                     as VU

-- | Doubleのパーサ
{-# INLINE parseDouble #-}
parseDouble :: Text -> Double
parseDouble tx = case DAT.parseOnly DAT.double tx of
        Right r   -> r
        Left  l   -> error $ "Error on parseDouble : " ++ show l


type Title      = String
type FileName   = String
type Label      = String

-- | 折れ線グラフの作成
plotLine :: (PlotValue a, PlotValue b) => [(Label, [(a, b)])] -> FileName -> Title -> IO ()
plotLine xs file title
    = toFile def (file ++ ".png") $ do
        layout_title .= title
        CM.forM_ xs $ \(title,ys) -> plot $ line title [ys]

plotBar :: [(String,[Double])] -> FileName -> Title -> [Label] -> IO ()
plotBar xs file title labels
    = toFile def (file ++ ".png") $ do
    layout_title .= title
    layout_title_style . font_size .= 10
    layout_x_axis . laxis_generate .= autoIndexAxis (L.map fst xs)
    plot $ fmap plotBars $ bars labels (addIndexes (L.map snd xs))


-- | 散布図の作成
plotGridPoints :: (PlotValue a, PlotValue b)
             => [[[(Label, [(a, b)])]]] -> FileName -> Title -> IO ()
plotGridPoints xs file name
    = let  ys   = aboveN $ (flip L.map) xs
                $ \ cols  -> besideN
                $ (flip L.map) cols -- columns
                $ \ series -> layoutToGrid
                $ execEC
                $ CM.forM_ series
                $ \(t,xs) -> plot $ points t xs
    in CM.void  $ renderableToFile def (file ++ ".png")
                $ fillBackground def
                $ gridToRenderable
                $ title `wideAbove` ys
  where
    title = setPickFn nullPickFn $ label ls HTA_Centre VTA_Centre name
    ls = def { _font_size   = 15 , _font_weight = FontWeightBold }



main = do
    -- CSVファイルの読み込み
    -- Dataフォルダを作成し,そこにデータを入れておきましょう
    xs <- CSV.parseCSVFromFile "Data/multiple_regression_test.csv" >>= \res
       -> case res of
            Right xs -> return $ L.map (L.map T.pack) xs
            Left  xs -> error $ "error on parseCSVFromFile: " ++ show xs

    -- Headerの抽出 Vectorへの変換
    let headers = L.map T.unpack
                $ L.head xs

    -- 値のDoubleへの変換 Vectorへの変換
    let body    = L.transpose
                $ L.map (L.map parseDouble)
                $ L.tail xs

    let year        = body !! 0
        death       = body !! 1
        index       = body !! 2
        hospital    = body !! 3

    -- データをグラフで表示
    plotLine    (L.map (\(x,y) -> (x, L.zip year y))
                [("Number of Death", death)
                ,("Economic Indicators", index)
                ,("Number of Hospitals", hospital)])
                "Data/initial_data"
                "Data Visualization"

    -- 散布図行列を作成する
    plotGridPoints  [[[(xt ++ "_" ++ yt, L.zip xs ys)]
                     | (yt,ys) <- L.zip (L.tail headers) (L.tail body)]
                    | (xt,xs) <- L.zip (L.tail headers) (L.tail body)]
                   "Data/plot_matrix"
                   "Plot Matrix"

    -- heatmap の作成はちょっと難しいのでpass

    -- 重回帰
    let (coeffs, r2) = olsRegress   [ VU.fromList index
                                    , VU.fromList hospital]
                                    ( VU.fromList death)

    putStrLn $ "回帰係数:" ++ show (L.init (VU.toList coeffs))
    putStrLn $ "切片:"     ++ show (L.last (VU.toList coeffs))

    -- 結果のプロット
    plotBar (L.zipWith  (\x y -> (x,[y]))
                        ["Economic Indicators", "Number of Hospitals"]
                        (L.init (VU.toList coeffs)))
            "Data/bar_result"
            "Plot Result"
            ["Coeff"]






