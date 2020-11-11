module Main (main) where

-- csv を読み込んで表示するだけのプログラム

-- package の import
-- CSVの読み込み
import qualified    Text.CSV                                as CSV
import qualified    Data.Text                               as T
import              Data.Text
import              Data.Attoparsec.Text                    as DAT


-- その他
import qualified    Control.Monad                           as CM
import qualified    Data.List                               as L
import              Data.IORef

-- | Doubleのパーサ
{-# INLINE parseDouble #-}
parseDouble :: Text -> Double
parseDouble tx = case DAT.parseOnly DAT.double tx of
        Right r   -> r
        Left  l   -> error $ "Error on parseDouble : " ++ show l

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


main = do
    -- CSVファイルの読み込み
    -- Dataフォルダを作成し,そこにデータを入れておきましょう
    -- Dataの名称を自分の保存したデータ名に変更します
    xs <- CSV.parseCSVFromFile "Data/salary_data.csv" >>= \res
       -> case res of
            Right xs -> return $ L.map (L.map T.pack) xs
            Left  xs -> error $ "error on parseCSVFromFile: " ++ show xs

    print xs
    print_data (L.head xs) (L.tail xs)





