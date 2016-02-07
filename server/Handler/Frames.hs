{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Frames (getFramesR, putFrameR) where

import Import hiding (fromList, empty)

import Data.Map (fromList, empty)
import Data.Time.Clock

import Handler.ArgumentParser
import Handler.ResourceResponse

data LabelUpdate = LabelUpdate
  { regionId :: RegionId
  , value :: Text
  }

instance FromJSON LabelUpdate where
    parseJSON (Object o) = LabelUpdate
      <$> o .: "regionId"
      <*> o .: "value"

    parseJSON _ = mzero

data FrameUpdate = FrameUpdate
  { labels :: [LabelUpdate]
  }

instance FromJSON FrameUpdate where
    parseJSON (Object o) = FrameUpdate
      <$> o .: "labels"

    parseJSON _ = mzero

getFramesR :: Handler Value
getFramesR = do
    maybeQuery :: Maybe Text <- getOptionalArgument "q"
    case maybeQuery of
      Nothing -> getAll
      Just "direction" -> getDirection
      Just otherQuery -> invalidArgs ["Unknown query '" ++ otherQuery ++ "'."]

putFrameR :: FrameId -> Handler Value
putFrameR frameId = do
    FrameUpdate labelUpdates <- requireJsonBody

    frame <- runDB $ get404 frameId
    mapM_ (checkLabelCreate $ frameDirection frame) labelUpdates

    runDB $ deleteWhere [LabelFrame ==. frameId]
    mapM_ (insertLabelUpdate frameId) labelUpdates

    sendResponseStatus status200 ("OK" :: Text)

-- I wonder if there's some sql constraint that checks for this?? Or some
-- way of structuring the schema so that it can't fail.
checkLabelCreate :: DirectionId -> LabelUpdate -> Handler ()
checkLabelCreate directionId (LabelUpdate regionId _) = do
    region <- runDB $ get404 regionId
    if regionDirection region /= directionId
      then
        invalidArgs ["Region " ++ (fromString $ show regionId) ++ " not in direction " ++ (fromString $ show directionId) ++ "."]
      else
        return ()

insertLabelUpdate :: FrameId -> LabelUpdate -> Handler ()
insertLabelUpdate frameId (LabelUpdate regionId value) = do
    _ <- runDB $ insert $ Label frameId regionId value
    return ()

getAll :: Handler Value
getAll = do
    -- Note that the pagination for this one is a bit screwed up because
    -- there can be multiple entries with the same pagination field
    -- (frameCapturedAt).
    pagination <- getPagination
    frames :: [Entity Frame] <- runDB $ selectList (paginationFilters pagination) (paginationOptions pagination)
    let returnFrames = take (length frames - 1) frames
    let nextFrame = lastMay frames
    returnJson $ ResourceResponse { elements = returnFrames, linked = empty, pagination = map paginationResponse nextFrame }

getDirection :: Handler Value
getDirection = do
    -- Theoretically the pagination for this one could be screwed up too
    -- but in practice we never have multiple frames in the same direction
    -- captured at the same second.
    pagination <- getPagination
    directionId <- getArgument "directionId"
    frames :: [Entity Frame] <- runDB $ selectList ((FrameDirection ==. directionId) : (paginationFilters pagination)) (paginationOptions pagination)
    let returnFrames = take (length frames - 1) frames
    let nextFrame = lastMay frames
    labels <- runDB $ selectList [LabelFrame <-. map (\(Entity k _) -> k) returnFrames] []
    returnJson $ ResourceResponse
      { elements = returnFrames
      , linked = fromList [("labels.v1", map toJSON labels)]
      , pagination = map paginationResponse nextFrame
      }

paginationFilters :: PaginationArgument UTCTime -> [Filter Frame]
paginationFilters pagination =
    maybeToList $ map (FrameCapturedAt >.) (start pagination)

paginationOptions :: PaginationArgument UTCTime -> [SelectOpt Frame]
paginationOptions pagination =
    Asc FrameCapturedAt : (maybeToList $ map (LimitTo . (+1)) (limit pagination))

paginationResponse :: Entity Frame -> Pagination
paginationResponse (Entity _ nextFrame) = Pagination
    { next = toPathPiece $ addUTCTime (0-1) (frameCapturedAt nextFrame)
    , total = Nothing
    }
