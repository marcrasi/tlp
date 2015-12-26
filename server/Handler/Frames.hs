{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Frames (getFramesR) where

import Import hiding (empty)

import Data.Map (empty)
import Data.Time.Clock

import Handler.ArgumentParser
import Handler.ResourceResponse

getFramesR :: Handler Value
getFramesR = do
    maybeQuery :: Maybe Text <- getOptionalArgument "q"
    case maybeQuery of
      Nothing -> getAll
      Just "direction" -> getDirection
      Just otherQuery -> invalidArgs ["Unknown query '" ++ otherQuery ++ "'."]

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
    returnJson $ ResourceResponse { elements = returnFrames, linked = empty, pagination = map paginationResponse nextFrame }

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
