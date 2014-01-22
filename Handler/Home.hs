{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    -- test join
    -- runDBの後で doで複数文書くとトランザクション
    {-
    runDB $ do 
            -- insertはidが戻り値
            johnId <-insert $ Person "John Doe" $ Just 35　-- justはOption,Some
            janeId <-insert $ Person "Jane Doe" Nothing
            -- FKを設定してinsert
            insert $ BlogPost "My fr1st p0st" johnId
            insert $ BlogPost "One more for good measure" johnId
            
            oneJohnPost <- selectList [BlogPostAuthorId ==. johnId] [LimitTo 1]
            liftIO $ print (oneJohnPost :: [Entity BlogPost])
            
            liftIO $ print "-------------"
            -- 基本的に、relation先のエンティティをjoinして持ってきてくれたりはしない。
            john <- get johnId
            liftIO $ print (john :: Maybe Person)
            -- タプルしてみる。
            liftIO $ print (john, oneJohnPost)
            
                        
            delete janeId
            deleteWhere [BlogPostAuthorId ==. johnId]
    -}
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getHomeR" :: Text
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postHomeR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing

    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

sampleForm :: Form (FileInfo, Text)
sampleForm = renderDivs $ (,)
    <$> fileAFormReq "Choose a file"
    <*> areq textField "What's on the file?" Nothing
