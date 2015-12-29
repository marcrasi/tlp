{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Directions where

import Import hiding (fromList, (\\))

import Data.List ((\\))
import Data.Map (fromList)

import Handler.ResourceResponse

-- `id` Nothing means that it's a new region
data RegionUpdate = RegionUpdate
  { id :: Maybe RegionId
  , value :: Text
  }

instance FromJSON RegionUpdate where
    parseJSON (Object o) = RegionUpdate
      <$> o .: "id"
      <*> o .: "value"

    parseJSON _ = mzero

-- any regions for the direction that are not specified in the update will
-- be deleted
data DirectionUpdate = DirectionUpdate
  { regions :: [RegionUpdate] 
  }

instance FromJSON DirectionUpdate where
    parseJSON (Object o) = DirectionUpdate
      <$> o .: "regions"

    parseJSON _ = mzero

getDirectionsR :: Handler Value
getDirectionsR = do
    directions :: [Entity Direction] <- runDB $ selectList [] []
    intersections :: [Entity Intersection] <- runDB $ selectList [] []
    regions :: [Entity Region] <- runDB $ selectList [RegionIsDeleted ==. False] []
    returnJson $ ResourceResponse
      { elements = directions
      , linked = fromList
          [ ("intersections.v1", map toJSON intersections)
          , ("regions.v1", map toJSON regions)
          ]
      , pagination = Nothing
      }

getDirectionR :: DirectionId -> Handler Value
getDirectionR directionId = do
    direction <- runDB $ get404 directionId
    let intersectionId = directionIntersection direction
    intersections <- runDB $ get intersectionId
    regions <- runDB $ selectList [RegionDirection ==. directionId, RegionIsDeleted ==. False] []
    returnJson $ ResourceResponse
      { elements = [Entity directionId direction]
      , linked = fromList
          [ ("intersections.v1", map (toJSON . Entity intersectionId) (toList intersections))
          , ("regions.v1", map toJSON regions)
          ]
      , pagination = Nothing
      }

putDirectionR :: DirectionId -> Handler Value
putDirectionR directionId = do
  _ <- runDB $ get404 directionId

  DirectionUpdate regionUpdates <- requireJsonBody
  existingRegions <- runDB $ selectList [RegionDirection ==. directionId] []
  
  let regionUpdateIds = concat $ map (toList . Handler.Directions.id) regionUpdates
  let existingRegionIds = map (\(Entity k _) -> k) existingRegions

  case regionUpdateIds \\ existingRegionIds of
    x : xs -> invalidArgs [(fromString $ show (x : xs)) ++ " are not regions for this direction."]
    [] -> return ()

  mapM_ (doRegionUpdate directionId) regionUpdates
  mapM_ doRegionDelete (existingRegionIds \\ regionUpdateIds)

  getDirectionR directionId

doRegionUpdate :: DirectionId -> RegionUpdate -> Handler ()
doRegionUpdate directionId (RegionUpdate Nothing value) = do
    _ <- runDB $ insert $ Region directionId False value
    return ()
doRegionUpdate _ (RegionUpdate (Just regionId) value) = do
    runDB $ update regionId [RegionValue =. value]
    return ()

doRegionDelete :: RegionId -> Handler()
doRegionDelete regionId = do
    runDB $ delete regionId
    return ()
