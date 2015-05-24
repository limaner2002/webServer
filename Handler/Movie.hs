module Handler.Movie where

import Import
import Handler.MovieFormR (movieForm)

postMovieR :: MovieId -> Handler Html
postMovieR movieId = do
  movie <- runDB $ get404 movieId
  (movieWidget, enctype) <- generateFormPost $ movieForm $ Just movie
  defaultLayout $ do
      $(widgetFile "movieForm")

getMovieR :: MovieId -> Handler Html
getMovieR movieId = do
  movie <- runDB $ get404 movieId
  defaultLayout $ do
    setTitle $ toHtml $ movieTitle movie
    $(widgetFile "movie")
