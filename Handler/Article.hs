module Handler.Article where

import Import

getArticleR :: ArticleId -> Handler Html
getArticleR articleId = do
    --The get404 function tries to do a get on the DB. If it fails, it returns a 404 page.
    article <- runDB $ get404 articleId
    defaultLayout $ do
        setTitle $ toHtml $ articleTitle article
        $(widgetFile "article")
