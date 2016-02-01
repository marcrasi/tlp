module Handler.FrameRequests where

import Import hiding (fromList)

import qualified Data.Text as DT
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Network.HTTP.Conduit (http)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>), FilePath, takeDirectory)

import Handler.ResourceResponse
import VideoDownload.ImportFrame (importFrame)
import VideoDownload.Urls (getFreshUrl)

data FrameRequest = FrameRequest
  { directionId :: DirectionId }

instance FromJSON FrameRequest where
    parseJSON (Object o) = FrameRequest
      <$> o .: "directionId"

    parseJSON _ = mzero

postFrameRequestsR :: Handler Value
postFrameRequestsR = do
    FrameRequest directionId <- requireJsonBody
    direction <- runDB $ get404 directionId
    intersection <- runDB $ get404 (directionIntersection direction)
    maybeUrl <- getFreshUrl (directionPublicVideoId direction)
    url <- case maybeUrl of
      Just u -> return u
      Nothing -> sendResponseStatus status500 ("Could not find video url." :: Text)
    currentTime <- liftIO $ getCurrentTime
    let framePath = getFramePath currentTime intersection direction
    liftIO $ createDirectoryIfMissing True $ takeDirectory framePath
    fileExists <- liftIO $ doesFileExist framePath
    if fileExists
      then return ()
      else do
        request <- parseUrl (DT.unpack url)
        manager <- newManager
        liftIO $ runResourceT $ do
          response <- http request manager
          responseBody response $$+- sinkFile framePath
        importFrame framePath directionId currentTime

    returnJson ("Hello World" :: Text)

getFramePath :: UTCTime -> Intersection -> Direction -> FilePath
getFramePath currentTime intersection direction =
    "static/images"
      </> (formatTime defaultTimeLocale "%F" currentTime)
      </> (DT.unpack $ intersectionName intersection)
      </> (DT.unpack $ directionName direction)
      </> ((formatTime defaultTimeLocale "%s" currentTime) ++ ".jpg")
