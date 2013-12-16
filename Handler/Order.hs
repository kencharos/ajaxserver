module Handler.Order where

import Import

getOrderR :: Handler Html
getOrderR = do
    -- Get the list of articles inside the database.
    -- 以下は全て、テンプレートに与えるパラメータ(product, productWidget, enctype,,,)
    orders <- runDB $ selectList [] [Asc OrderId]
    defaultLayout $ do
        $(widgetFile "order")

postOrderR :: Handler Value
postOrderR = do
  order <- parseJsonBody_
  $(logInfo) "parse OK"
  tid <- runDB $ insert (order :: Order)
  jsonToRepJson $ object ["status" .= ("ok" :: Text)]
