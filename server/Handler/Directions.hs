{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Directions where

import Import hiding (fromList)

import Data.Map (fromList)

import Handler.ResourceResponse

data DirectionUpdate = DirectionUpdate
  { annotations :: Text 
  }

instance FromJSON DirectionUpdate where
  parseJSON (Object o) = DirectionUpdate
    <$> o .: "annotations"

  parseJSON _ = mzero

getDirectionsR :: Handler Value
getDirectionsR = do
    directions :: [Entity Direction] <- runDB $ selectList [] []
    intersections :: [Entity Intersection] <- runDB $ selectList [] []
    returnJson $ ResourceResponse
      { elements = directions
      , linked = fromList [("intersections.v1", map toJSON intersections)]
      , pagination = Nothing
      }

getDirectionR :: DirectionId -> Handler Value
getDirectionR directionId = do
    direction <- runDB $ get404 directionId
    let intersectionId = directionIntersection direction
    intersections <- runDB $ get intersectionId 
    returnJson $ ResourceResponse
      { elements = [Entity directionId direction]
      , linked = fromList [("intersections.v1", map (toJSON . Entity intersectionId) (toList intersections))]
      , pagination = Nothing
      }

putDirectionR :: DirectionId -> Handler Value
putDirectionR directionId = do
  directionUpdate :: DirectionUpdate <- requireJsonBody
  runDB $ update directionId [DirectionAnnotations =. (annotations directionUpdate)]
  direction <- runDB $ get404 directionId
  returnJson direction 
