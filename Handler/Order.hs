module Handler.Order where

import Import
import Control.Monad

getOrderR :: Handler Html
getOrderR = do
    -- Get the list of articles inside the database.
    -- "<-"はモナドから値を一つ取り出し値に束縛する。いてれー他みたいなもん
    -- ドルマーク は関数連続呼び出しのシュガー構文。 reunDB(selectList,..)と同じ。
    -- square a = a * a
    -- add ab = a + b
    -- square $ add 3 4 => 49 
    {- Entityはdataとして定義している。 scalaのcase classみたいな。
       data Point Int Int deriving (Eq,Show) ..  let p = Point 1 2
       deriving はEq,Showの派生としてShowやEqの関数を使えるようにすることを意味する。 (Mix-inに近い?)
       Showを派生しないと、printで表示もできない。物によっては特定の関数を定義する必要もあるのかも？
                 名前付きにするなら
        data Car = Car{company :: String, year ::Int}deriving Show         
        let car = Car{company="toyota", year=1991}
                 名前の値をとるには
        compnay car や、year car とする。 ( company(Car) と同じなので,
        Car型の変数しか受け付けない、company, year関数ができたと理解。構造化タイピング、ダックタイピング間がある。
        OOPと違って、関数は特定のオブジェクトに紐づかないので、car companyとしそうだが、異なる。
    -}
    -- \ x -> x * x は関数リテラル。
    -- detailsにmapし、FKから実エンティティを取得して、タプルにする。
    -- YesodはSQL/NoSQL対応のため、join SQLなんて仕組みはないらしい。常にN+1 select(esqueltoなんてライブラリがあるとかないとか)
    --http://stackoverflow.com/questions/20890971/how-to-use-rundb-correctly-in-yesod-persistent
    --mapMはよくわからんが、Detailからタプルへの変換がこれでうまく行くようだ。
    -- http://qiita.com/craccho/items/2d57f7fe6296590faa80  Entityと値の型は微妙に異なる。
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


postOrderR :: Handler Value
postOrderR = do
  order <- parseJsonBody_
  $(logInfo) "parse OK"
  tid <- runDB $ insert (order :: Order)
  jsonToRepJson $ object ["status" .= ("ok" :: Text)]
