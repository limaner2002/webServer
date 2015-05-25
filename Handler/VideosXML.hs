module Handler.VideosXML where

import Import
import Text.Hamlet.XML (xmlFile, xml)
import Text.XML
-- import Text.Hamlet          (hamletFile)
import Data.Map (empty)

getVideosXMLR :: Handler TypedContent
getVideosXMLR = do
  allMovies <- runDB $ selectList [] [Desc MovieAdded]
  let latest = map (addMovieGenre "#Last Added") $ take 5 allMovies
  let rest = drop 5 allMovies
  let movies = map (addMovieGenre "#All") $ latest ++ sortBy (\(Entity _ m1) (Entity _ m2) -> compare (movieTitle m1) (movieTitle m2)) rest
  let r = $(xmlFile "templates/videos.hamlet")
  let root = Element "xml" empty r
  let doc = Document (Prologue [] Nothing []) root []
  respond typeXml $ renderText def doc

movieNodes :: Entity Movie -> [Node]
movieNodes (Entity _ movie) = [xml|
<poster>#{"movies/download/" ++ (encode $ moviePoster movie)}
<description>#{fromMaybe "" $ movieDescription movie}
<origtitle>#{movieTitle movie}
<mpaa>#{fromMaybe "" $ movieMpaa movie}
<director>#{fromMaybe "" $ movieDirector movie}
<length>#{pack $ show $ fromMaybe -1 $ movieLength movie}
<videocodec>#{fromMaybe "" $ movieCodec movie}
<actors>#{fromMaybe "" $ movieActors movie}
<year>#{pack $ show $ movieYear movie}
<genre>#{fromMaybe "" $ movieGenre movie}
<path>#{"movies/download/" ++ (encode $ moviePath movie)}
|]

encode :: Text -> Text
encode = decodeUtf8 . (urlEncode True) . encodeUtf8
-- getVideosXMLR :: Handler Html
-- getVideosXMLR = do
--   movies <- runDB $ selectList [] [Asc MovieTitle]
--   defaultLayout $ do
--                $(widgetFile "videosHTML")

addMovieGenre :: Text -> Entity Movie -> Entity Movie
addMovieGenre attr (Entity x oldMovie) = Entity x newMovie
  where
    newMovie = oldMovie { movieGenre = Just (concat [fromMaybe "" oldGenre, ",", attr]) }
    oldGenre = movieGenre oldMovie
