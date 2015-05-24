module Handler.MovieFormR where

import Import

movieForm :: Maybe Movie -> Html -> MForm Handler (FormResult Movie, Widget)
movieForm movie = renderTable $ Movie
                       <$> areq textField "Title" (fmap movieTitle movie)
                       <*> areq intField "Year" (fmap movieYear movie)
                       <*> aopt textField "Genre" (fmap movieGenre movie)
                       <*> aopt textField "Rating" (fmap movieMpaa movie)
                       <*> aopt textField "Director" (fmap movieDirector movie)
                       <*> aopt textField "Actors" (fmap movieActors movie)
                       <*> aopt textField "Description" (fmap movieDescription movie)
                       <*> areq textField "Path" (fmap moviePath movie)
                       <*> aopt textField "Codec" (fmap movieCodec movie)
                       <*> aopt intField "Length" (fmap movieLength movie)
                       <*> areq textField "Poster" (fmap moviePoster movie)
                       <*> lift (liftIO getCurrentTime)

getMovieFormRR :: Handler Html
getMovieFormRR = do
  (movieWidget, enctype) <- generateFormPost $ movieForm Nothing
  defaultLayout $ do
      $(widgetFile "movieForm")

postMovieFormRR :: Handler Html
postMovieFormRR = do -- error "Not yet implemented: postMovieFormRR"
  ((res,movieWidget),enctype) <- runFormPost $ movieForm Nothing
  case res of
       FormSuccess movie -> do
            movieId <- runDB $ insert movie
	    setMessage $ toHtml $ (movieTitle movie) <> " created"
	    redirect $ MovieR movieId
       _ -> defaultLayout $ do
       	    		  setTitle "Please correct your entry form"
			  $(widgetFile "movieForm")
