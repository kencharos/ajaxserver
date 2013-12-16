module Handler.ProductJson where

import Import

getProductJsonR :: Handler Value
getProductJsonR = do
    -- Get the list of articles inside the database.
    products <- runDB $ selectList [] [Asc ProductId]
    returnJson products
