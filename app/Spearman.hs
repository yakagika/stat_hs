module Main where
-- package の import
-- CSVの読み込み
import qualified    Text.CSV                                as CSV
import qualified    Data.Text                               as T
import              Data.Attoparsec.Text                    as DAT
-- 統計のためのパッケージ
import qualified    Statistics.Correlation                  as SC
-- その他
import qualified    Control.Monad                           as CM
import qualified    Data.List                               as L
import qualified    Data.Vector.Unboxed                     as VU
import              Data.IORef

main = do
    -- csvファイルの読み込み
    xs <- read_csv "Data/spearman.csv"
    -- データの表示
    print_data (L.head xs) (L.tail xs)
    -- Headerの抽出
    let headers = L.map T.unpack $ L.head xs
    -- 値のDoubleへの変換
    let body    = L.transpose
                $ L.map  (L.map parseDouble)
                $ L.tail xs
    -- 分析するデータの選択
    -- xとyのHeader名を書き換えよう
    let x = column body "FIFA" headers
    let y = column body "WBSC" headers
    -- 相関係数を求める
    let res = SC.spearman
            $ VU.zip (VU.fromList x) (VU.fromList y)
    -- 相関係数の表示
    putStrLn $ "相関係数:" ++ show res




-- ** 作業用関数
-- 別のモジュールにまとめておくのが良いが複雑になるのでここにまとめておく
-- | Doubleのパーサ
{-# INLINE parseDouble #-}
parseDouble :: T.Text -> Double
parseDouble tx = case DAT.parseOnly DAT.double tx of
        Right r   -> r
        Left  l   -> error $ "Error on parseDouble : " ++ show l

-- | データの読み込み
read_csv :: String -> IO [[T.Text]]
read_csv file
    = CSV.parseCSVFromFile file >>= \res
    -> case res of
        Right xs -> return $ L.map (L.map T.pack) xs
        Left  xs -> error $ "error on parseCSVFromFile: " ++ show xs

-- | data set っぽく表示する
print_data :: (Show a)  => [T.Text] {- header -}
                        -> [[a]]    {- body -}
                        -> IO ()
print_data hs bs = do
    let max_length  =  let hs_max = L.maximum (L.map T.length hs)
                    in let bs_max = L.maximum
                                  $ (flip L.map) bs
                                  $ \x  -> L.maximum
                                  $ L.map (L.length . show) x
                    in case compare hs_max bs_max  of
                        LT -> bs_max
                        _  -> hs_max

    let bs_digit_num =  L.length (show (L.length bs))
    putStr $ " " ++ L.replicate bs_digit_num ' '
    CM.forM_ hs $ \x -> let tx = T.unpack x
                     in case compare (L.length tx) max_length of
                        LT -> putStr $ tx ++ (L.replicate  (max_length -  (L.length tx)) ' ')  ++ " "
                        _  -> putStr $ tx ++ " "
    putStrLn ""

    currentNum <- newIORef 0 :: IO (IORef Int)

    let  total_col = L.length bs
    case total_col  > 10 of
        False -> CM.forM_ bs $  \xs -> readIORef currentNum >>= \num
                                    -> putStr ((show num)   ++ " "
                                                            ++ L.replicate
                                                                (bs_digit_num - (L.length (show num))) ' ')
                                    >> modifyIORef currentNum (\x -> x + 1)
                                    >> CM.forM_ xs  ( \x -> let tx = show x
                                                         in putStr  $ tx
                                                                    ++ (L.replicate  (max_length -  (L.length tx)) ' ')
                                                                    ++ " ")
                                    >> putStrLn ""
        True ->  CM.forM_ bs $  \xs -> readIORef currentNum >>= \num
                                    -> case (num <= 4 || (total_col - num) <= 4) of
                                        True  -> putStr ((show num)   ++ " " ++ L.replicate (bs_digit_num - (L.length (show num))) ' ')
                                              >> modifyIORef currentNum (\x -> x + 1)
                                              >> CM.forM_ xs  ( \x -> let tx = show x
                                                         in putStr  $ tx
                                                                    ++ (L.replicate  (max_length -  (L.length tx)) ' ')
                                                                    ++ " ")
                                              >> putStrLn ""
                                        False -> modifyIORef currentNum (\x -> x + 1)


-- | headerによる情報の抜き出し
column :: [[Double]] -> String -> [String] -> [Double]
column xs name hs = case (L.elemIndex name hs) of
                    Just i  -> xs !! i
                    Nothing -> error $ "error at column: " ++ name ++ "doesn't exest."
