module Handler.FrameRequests where

import Import hiding (fromList)

import qualified Data.Text as DT
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>), FilePath, takeDirectory)

import Handler.ResourceResponse
import VideoDownload.Download (getFrame, DownloadError(CannotFindVideoUrl, CannotDownloadFrame))
import VideoDownload.ImportFrame (importFrame)

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
    -- Note that currentTime might be way out of date by the time we
    -- actually download the frame. Maybe find a way to fix??
    currentTime <- liftIO $ getCurrentTime
    let framePath = getFramePath currentTime intersection direction
    liftIO $ createDirectoryIfMissing True $ takeDirectory framePath
    fileExists <- liftIO $ doesFileExist framePath
    if fileExists
      then return ()
      else do
        frameResult <- getFrame framePath (directionPublicVideoId direction)
        case frameResult of
          Just CannotFindVideoUrl ->
            sendResponseStatus status500 ("Cannot find video url." :: Text)
          Just (CannotDownloadFrame status) ->
            sendResponseStatus status500 ("Cannot download frame: " ++ (show status))
          Nothing ->
            importFrame framePath directionId currentTime

    returnJson ("Hello World" :: Text)

getFramePath :: UTCTime -> Intersection -> Direction -> FilePath
getFramePath currentTime intersection direction =
    "static/images"
      </> (formatTime defaultTimeLocale "%F" currentTime)
      </> (DT.unpack $ intersectionName intersection)
      </> (DT.unpack $ directionName direction)
      </> ((formatTime defaultTimeLocale "%s" currentTime) ++ ".jpg")
