module Handler.Download where

import Prelude (read)
import Import

import System.Posix (getFileStatus, fileSize)
import Network.Mime (defaultMimeLookup)
import qualified Network.Wai as W

import Text.ParserCombinators.Parsec
import qualified Text.Parsec.ByteString as PBS
import qualified Data.ByteString.Char8 as C8
-- import Blaze.ByteString.Builder.ByteString (fromByteString)

parseByteRange :: PBS.Parser (Integer, Integer)
parseByteRange = do
  _ <- string "bytes="
  rangeFromTo
 where
   -- range = rangeFromTo <|> rangeSuffix
   rangeFromTo = do
     frm <- many1 digit
     char '-'
     to <- many1 digit
     return (read frm, read to)

-- getContentType :: Text -> ContentType
-- getContentType path = do
  

getDownloadR :: Text -> Handler TypedContent
getDownloadR path = do
  let fullPath = "/Library/WebServer/Documents/" ++ path
  sendFile (defaultMimeLookup fullPath) $ unpack fullPath
  -- return $ TypedContent "video/mp4" $ ContentSource $
  --        mapOutput (Chunk . fromByteString) $ sourceFile $ fpFromText fullPath

-- getDownloadR :: Text -> Handler TypedContent
-- getDownloadR path = do
--   let fullPath = "/Library/WebServer/Documents/" ++ path
--   let mimeType = defaultMimeLookup fullPath
--   mStat <- liftIO $ tryJust (guard . isDoesNotExistError) $ getFileStatus $ unpack fullPath
--   case mStat of
--     Left _ -> do sendResponseStatus status404 $ concat [fullPath, " not found"]
--     Right stat -> do
--                req <- waiRequest
--                $(logDebug) $ pack $ show $ W.requestMethod req
--                let mrange = W.requestHeaderRange req
--                case mrange of
--                  Nothing -> sendFile mimeType $ unpack fullPath
--                  Just range -> do
--                              let result = parse parseByteRange "" $ range
--                              case result of
--                                Left _ -> sendFile mimeType $ unpack fullPath
--                                Right (frm, to) -> do
--                                            let range = concat ["bytes ", C8.pack $ show frm, "-", C8.pack $ show to,
--                                                                "/", C8.pack $ show $ System.Posix.fileSize stat]
--                                            let hdrs = [("Accept-Ranges", "bytes"),
--                                                        ("Content-Length", (C8.pack $ show $ to - frm + 1)),
--                                                        ("Content-Range", range),
--                                                        ("Content-Type", mimeType),
--                                                        ("Keep-Alive", "timeout=5, max=100"),
--                                                        ("Connection", "Keep-Alive")]
--                                            $(logDebug) $ pack $ show hdrs
--                                            let rf = W.responseFile status206 hdrs (unpack fullPath) $
--                                                     Just $ W.FilePart frm to (to - frm + 1)
--                                            sendWaiResponse $ rf

--   -- -- let ct = "video/mp4"
--   -- mStat <- liftIO $ tryJust (guard . isDoesNotExistError) $ getFileStatus $ unpack fullPath
--   -- case mStat of
--   --   Right stat -> do
--   --              let size = fromIntegral $ System.Posix.fileSize stat
--   --              addHeader "Accept-Ranges" "bytes"
--   --              addHeader "Content-Range" (pack $ show size)
--   --              req <- waiRequest
--   --              liftIO $ print $ req
--   --              sendFilePart "video/mp4" (unpack fullPath) 0 size
--   --              -- respondSource "video/mp4" $
--   --              --               sourceFile (fpFromText fullPath) $= awaitForever sendChunkBS
--   --   Left _ -> do
--   --              sendResponseStatus status404 $ path ++ pack " not found"
--   -- -- sendFile "video/mp4" $ unpack fullPath

--   -- W.responseFile status206 [] (unpack fullPath) Nothing
