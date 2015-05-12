{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi

import qualified Data.Text as T

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

instance PathPiece MovieId
    where
      toPathPiece (MovieKey title year) = s
          where
            s = pack (show year) ++ pack "-" ++ title

      fromPathPiece t = do
        year <- fromPathPiece $ maybeYear
        let result = MovieKey title year
        Just $ result
       where
         [maybeYear, title] = T.splitOn (pack "-") t
