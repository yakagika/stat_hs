

module Sturges where

{- | 関数名を sturgesNumber として引数をnとします
     公式の通り k = 1 + log2 n
     階級数は整数が良いので,truncateで小数点以下を切り捨てます
     対数を返す関数 logBaseは
     logBase :: Floating a => a -> a -> a
     と Floating (小数値) を受け取るので
     Int (整数)である n を fromIntegral で Floatingに変換しています
-}

sturgesNumber :: Int -> Int
sturgesNumber n = truncate
                $ 1 + logBase 2 (fromIntegral n)

