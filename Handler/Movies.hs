module Handler.Movies where

import Import

getMoviesR :: Handler Html
getMoviesR = do
  movies <- runDB $ selectList [] [Asc MovieTitle]
  defaultLayout $ do
               $(widgetFile "movies")
