module Handler.FrameRequests where

import Import hiding (fromList)

import qualified Data.Text as DT
import Data.Map (fromList)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>), FilePath, takeDirectory)

import Handler.ResourceResponse
import VideoDownload.Download (getFrame, DownloadError(CannotFindVideoUrl, CannotDownloadFrame))
import VideoDownload.ImportFrame (importFrame)

data FrameRequestCreate = FrameRequestCreate DirectionId

instance FromJSON FrameRequestCreate where
    parseJSON (Object o) = FrameRequestCreate
      <$> o .: "directionId"

    parseJSON _ = mzero

data FrameRequest = FrameRequest
  { directionId :: DirectionId
  , frameId :: FrameId
  , isNew :: Bool
  }

instance ToJSON FrameRequest where
    toJSON (FrameRequest directionId frameId isNew) = object
      [ "directionId" .= directionId
      , "frameId" .= frameId
      , "isNew" .= isNew
      ]

postFrameRequestsR :: Handler Value
postFrameRequestsR = do
    FrameRequestCreate directionId <- requireJsonBody
    direction <- runDB $ get404 directionId
    intersection <- runDB $ get404 (directionIntersection direction)
    -- Note that currentTime might be way out of date by the time we
    -- actually download the frame. Maybe find a way to fix??
    currentTime <- liftIO $ getCurrentTime
    let framePath = getFramePath currentTime intersection direction
    liftIO $ createDirectoryIfMissing True $ takeDirectory framePath
    fileExists <- liftIO $ doesFileExist framePath
    importResult <- if fileExists
      then importFrame framePath directionId currentTime
      else do
        frameResult <- getFrame framePath (directionPublicVideoId direction)
        case frameResult of
          Just CannotFindVideoUrl ->
            sendResponseStatus status500 ("Cannot find video url." :: Text)
          Just (CannotDownloadFrame status) ->
            sendResponseStatus status500 ("Cannot download frame: " ++ (show status))
          Nothing ->
            importFrame framePath directionId currentTime

    (frameId, isNew) <- case importResult of
      Left error -> sendResponseStatus status500 error
      Right result -> return result

    frames <- runDB $ get frameId

    returnJson $ ResourceResponse
      { elements = [ FrameRequest directionId frameId isNew ]
      , linked = fromList [("frames.v1", map (\x -> toJSON $ Entity frameId x) (maybeToList frames))]
      , pagination = Nothing
      }

getFramePath :: UTCTime -> Intersection -> Direction -> FilePath
getFramePath currentTime intersection direction =
    "static/images"
      </> (formatTime defaultTimeLocale "%F" currentTime)
      </> (DT.unpack $ intersectionName intersection)
      </> (DT.unpack $ directionName direction)
      </> ((formatTime defaultTimeLocale "%s" currentTime) ++ ".jpg")
