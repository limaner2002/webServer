module Handler.Download where

import Import

import Network.Mime (defaultMimeLookup)

getDownloadR :: Text -> Handler TypedContent
getDownloadR path = do
  let fullPath = "/media/" ++ path
  sendFile (defaultMimeLookup fullPath) $ unpack fullPath
