module VideoDownload.Urls (getFreshUrl, forceRefreshUrls) where

import Import

import Data.Aeson (decode)
import Data.Time.Clock (diffUTCTime, getCurrentTime, secondsToDiffTime)

getFreshUrl :: Text -> Handler (Maybe Text)
getFreshUrl publicVideoId = do
    let freshnessSeconds = 600
    currentTime <- liftIO $ getCurrentTime
    maybeExisting <- runDB $ getBy (UniquePublicVideoId publicVideoId)
    maybeFresh <- case maybeExisting of
      Just (Entity _ existing)
        | diffUTCTime currentTime (videoUrlGeneratedAt existing) < freshnessSeconds ->
          return $ Just existing
      _ -> do
        forceRefreshUrls
        maybeRefreshed <- runDB $ getBy (UniquePublicVideoId publicVideoId)
        case maybeRefreshed of
          Just (Entity _ refreshed)
            | diffUTCTime currentTime (videoUrlGeneratedAt refreshed) < freshnessSeconds ->
            return $ Just refreshed
          _ ->
            return Nothing
    case maybeFresh of
      Just fresh -> return $ Just $ videoUrlUrl fresh
      Nothing -> return Nothing

data CameraResponseContent = CameraResponseContent
  { fullJpeg :: Text }

instance FromJSON CameraResponseContent where
    parseJSON (Object o) = CameraResponseContent
      <$> o .: "fullJpeg"

    parseJSON _ = mzero

data CameraResponse = CameraResponse
  { publicId :: Text
  , content :: CameraResponseContent
  }

instance FromJSON CameraResponse where
    parseJSON (Object o) = CameraResponse
      <$> o .: "publicId"
      <*> o .: "content"

    parseJSON _ = mzero

forceRefreshUrls :: Handler ()
forceRefreshUrls = do
    liftIO $ print "Warning: Downloading URLs."
    currentTime <- liftIO $ getCurrentTime
    request <- parseUrl "https://www.sccgov.org/sites/scc/_layouts/SCCGOV/SccgovProxy.aspx?a=tlj&b=%26ignore%3Dtrue"
    response <- withManager $ httpLbs request
    case (decode $ responseBody response :: Maybe [CameraResponse]) of
      Just cameraResponses ->
        mapM_ (handleCameraResponse currentTime) cameraResponses
      Nothing ->
        liftIO $ print "Warning: Could not decode response!"

handleCameraResponse :: UTCTime -> CameraResponse -> Handler ()
handleCameraResponse currentTime cameraResponse = do
    _ <- runDB $ upsert
      (VideoUrl
        (publicId cameraResponse)
        (fullJpeg $ content $ cameraResponse)
        currentTime)
      []
    return ()
