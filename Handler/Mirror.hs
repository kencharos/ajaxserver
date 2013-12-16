module Handler.Mirror where

import Import
import qualified Data.Text as T

getMirrorR :: Handler Html
getMirrorR = defaultLayout $(widgetFile "mirror")

postMirrorR :: Handler Html
postMirrorR =  do
        -- content という名前のフィールドをリクエストから取得し、postedTextに設定。
        postedText <- runInputPost $ ireq textField "content"
        -- postedTextはテンプレート内で参照する。
        defaultLayout $(widgetFile "posted")
