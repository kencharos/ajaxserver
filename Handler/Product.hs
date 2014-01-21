module Handler.Product where

import Import

-- define form
--You just have to remember areq is for required form input.
-- Its arguments being: areq type label default_value.
entryForm :: Form Product
entryForm = renderDivs $ Product
    <$> areq   textField "num" Nothing
    <*> areq   textField "Name" Nothing
    <*> areq   intField "Price" Nothing

-- The view showing the list of articles
getProductR :: Handler Html
getProductR = do
    -- Get the list of articles inside the database.
    -- 以下は全て、テンプレートに与えるパラメータ(product, productWidget, enctype,,,)
    products <- runDB $ selectList [] [Asc ProductId]
    (productWidget, enctype) <- generateFormPost entryForm
    defaultLayout $ do
        $(widgetFile "product")


postProductR :: Handler Html
postProductR = do
    ((res,productWidget),enctype) <- runFormPost entryForm
    case res of
        -- on success
         FormSuccess prod -> do
            -- IDがどのテーブルにも付与される。
            productId <- runDB $ insert prod
            -- テーブル名 + 属性名で取得できる。
            setMessage $ toHtml $ (productName prod) <> " created"
            redirect $ ProductR
        -- on error 
         _ -> do
              redirect $ ProductR
