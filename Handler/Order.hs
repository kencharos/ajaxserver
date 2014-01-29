module Handler.Order where

import Import
import Control.Monad

getOrderR :: Handler Html
getOrderR = do
    -- Get the list of articles inside the database.
    -- "<-"はモナドから値を一つ取り出し値に束縛する。イテレータみたいなもん
    {-
    ドルマーク は関数連続呼び出しのシュガー構文。 reunDB(selectList,..)と同じ。
    square a = a * a
    add ab = a + b
    square $ add 3 4 => 49
    -} 
    {- dataの話。
    　　　Entityはdataとして定義している。dataは scalaのcase classみたいなもの。
       data Point = Point Int Int deriving (Eq,Show) ..  let p = Point 1 2
       (ちなみに、data Point = P Int Intのように、左右であってなくてもよい。左辺は名前、右辺はコンストラクタの関数名で一致する必要はない。)
       deriving は型クラスのEq,Showの派生としてShowやEqの関数を自動実装して使えるようにすることを意味する、derivingできる型クラスは限定。 (Mix-inに近い?)
       Showを派生しないと、printで表示もできない。物によっては特定の関数を定義する必要もあるのかも？
        dataにラベルを付けるなら
        data Car = Car{company :: String, year ::Int}deriving Show         
        let car = Car{company="toyota", year=1991}
        または、 let car = Car "toyora" 1991
        ラベルを付けると、ラベルの名前で関数ができ（セレクタと呼ぶ)、以下のような方法で値を取得できる
        compnay car や、year car。 ( company(Car) という呼び出しになる,
        Car型の変数しか受け付けない、company, year関数ができたと理解。構造化タイピング、ダックタイピング間がある。
        OOPと違って、関数は特定のオブジェクトに紐づかないので、car companyとしそうだが、異なる。
    -}
    {- 続き
     \ x -> x * x は関数リテラル。
     detailsにmapし、FKから実エンティティを取得して、タプルにする。
     YesodはSQL/NoSQL対応のため、join SQLなんて仕組みはないらしい。常にN+1 select(esqueltoなんてライブラリがあるとかないとか)
    http://stackoverflow.com/questions/20890971/how-to-use-rundb-correctly-in-yesod-persistent
    mapMはよくわからんが、Detailからタプルへの変換がこれでうまく行くようだ。
     http://qiita.com/craccho/items/2d57f7fe6296590faa80  Entityと値の型は微妙に異なる。
     -}
     -- >>= はバインド(scalaでいうflatmap。selectlistでとったdetailのエンティティから、他のエンティティを足してタプルに変換する。
     -- >>= なので、 関数は a- > m b であり、モナドに変換する必要があり、最後にreturnしている。
    orders <- runDB $ selectList [] [Asc DetailId] >>= mapM (\detail@(Entity _ d) -> do
                let pid = detailProduct d
                let oid = detailOrder d
                prod <- get404 pid
                order <- get404 oid
                return (detail, Entity pid prod, Entity oid order))
    -- DB取得結果は、Entity id val の形式。
    -- valにデータが含まれるので、テンプレートでは、タプルと合わせ業で、パターンマッチングによる分解を行っている。
    defaultLayout $ do
        $(widgetFile "order")


-- ブラウザから受け取る注文データ
data OrderDetails = OrderDetails{
  order :: Order,
  details :: [Detail2]
}deriving Show

data Detail2 = Detail2{
  num :: Text,
  net :: Int
}deriving Show
{- 型クラス
モデルの場合、jsonを記述すれば勝手にパース処理を作るが、自前の場合は自作する。
dataが特定の関数から呼ばれてもOKにするには、ToJson　data/FromJson dataで上記OrderDetailsを受け付けてくれるようにinstance定義をする。
ちなみに、 FromJSONとかToJSONは、型クラス(classキーワードで定義するが、OOPのクラスと別)と呼ばれるもので、関数の集合。 Eq,Showなどもそう。
特定のdataに型クラスの関数を適用するのに、instance宣言を用いる。
型クラスでないと実行できない関数の定義は、 Eq a=> a-> a-> Bool のように、=> の左に型aがEqのインスタンスでなければならないという風に示される。
多相関数を定義する手段として、後付で対応する型を増やせる仕組みと理解。OOPの継承より柔軟か。
-}
instance FromJSON OrderDetails where 
  parseJSON (Object v) = OrderDetails <$>
                         v .: "order" <*>
                         v .: "details"
instance FromJSON Detail2 where 
  parseJSON (Object v) = Detail2 <$>
                         v .: "num" <*>
                         v .: "net"

postOrderR :: Handler Value
postOrderR = do
  orderDetails <- parseJsonBody_
  $(logInfo) "parse OK"
  runDB $ do
      oid <- insert $ order orderDetails
      --　何でこれがうまく行って、do式のループだとだめなのか、、、
      forM (details orderDetails) $ \d -> do
         prod <- getBy $ UniqueNum $ num d
         case prod of
            Just x@(Entity pid _) -> insert $ Detail pid oid (net d)

  jsonToRepJson $ object ["status" .= ("ok" :: Text)]
