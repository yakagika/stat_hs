module Third  where


-- 列挙型
-- data 型の名前(型構築子) = カテゴリ1  | カテゴリ2 | ...
-- のように書きます

-- 信号の状態を表すデータ型を作成
data TraficLights = Blue | Red | Yellow

-- このとき,
-- TraficLights を 型構築子(context)
-- Blue, Red, Yellow を コンストラクタ と言います.

-- パターンマッチなどで利用できます.

-- | 例: 車が進めるか判断する関数
canGo :: TraficLights  -> Bool
canGo tr = case tr of
    Blue    ->  True
    Red     ->  False
    Yellow  ->  True



-- 型クラスを継承する方法には
-- 自動で継承する deriving 及び instance 宣言 の2つがあります
-- 宣言の後に deriving 型クラス名
-- を追加することで,自動でそのクラスのインスタンスに出来る場合があります

-- 以下では,Stringに変換するメソッド show を使えるように
-- Show class の instance にしています
data Rank = First | Second | Third deriving Show

-- instance 宣言によって 自分でメソッドの定義もできます
-- instance 型クラス名 型 where の形で宣言できます.

-- 以下では, 同値比較が可能な Eq Classのinstanceにするために
-- method である (==) と (/=) の定義をしています

instance Eq Rank where 
    First   == First  = True 
    Second  == Second = True 
    Third   == Third  = True 
    x       == y      = False 