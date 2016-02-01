module VideoDownload.Download (getFrame, DownloadError(CannotFindVideoUrl, CannotDownloadFrame)) where

import Import

import qualified Control.Exception as X
import qualified Data.Text as DT
import Network.HTTP.Conduit (http)
import qualified Network.HTTP.Types.Status as S

import VideoDownload.Urls (getFreshUrl, forceRefreshUrls)

data DownloadError
    = CannotFindVideoUrl
    | CannotDownloadFrame Status

getFrame :: FilePath -> Text -> Handler (Maybe DownloadError)
getFrame = getFrame' True

getFrame' :: Bool -> FilePath -> Text -> Handler (Maybe DownloadError)
getFrame' refreshAndTryAgain filePath publicVideoId = do
    maybeFreshUrl <- getFreshUrl publicVideoId
    case maybeFreshUrl of
      Just freshUrl -> do
        request' <- parseUrl $ DT.unpack freshUrl
        let request = request' { checkStatus = \_ _ _ -> Nothing }
        manager <- newManager
        result <- liftIO $ runResourceT $ do
          response <- http request manager
          case responseStatus response of
            status
              | status == S.status200 -> do
                responseBody response $$+- sinkFile filePath
                return Nothing
              | otherwise -> return $ Just $ CannotDownloadFrame $ responseStatus response
        case result of
          Just (CannotDownloadFrame status)
            | status == S.status401 && refreshAndTryAgain -> do
              forceRefreshUrls
              getFrame' False filePath publicVideoId
          otherResult -> return $ otherResult
      Nothing -> return $ Just CannotFindVideoUrl

