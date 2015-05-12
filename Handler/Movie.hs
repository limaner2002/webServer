module Handler.Movie where

import Import

getMovieR :: MovieId -> Handler Html
getMovieR movieId = do
  movie <- runDB $ get404 movieId
  defaultLayout $ do
    setTitle $ toHtml $ movieTitle movie
    $(widgetFile "movie")
